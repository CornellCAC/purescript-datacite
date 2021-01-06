module DataCite.JSON.Decode.Simple where

import Control.Apply (class Apply)
import Control.Bind (class Bind)
import Control.Category ((<<<), (>>>))
import Control.Monad (class Monad)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (runExcept, runExceptT)
import Control.Monad.Except.Trans (ExceptT(..))
import Control.Monad.Writer (Writer, runWriter)
import Control.Monad.Writer.Class (class MonadTell, class MonadWriter, tell)
import Data.Array as A
import Data.Array.NonEmpty (NonEmptyArray, fromArray)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Functor (class Functor)
import Data.HeytingAlgebra ((&&), (||))
import Data.Identity (Identity)
import Data.Lazy (Lazy, defer, force)
import Data.List.NonEmpty (toUnfoldable)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Semigroup ((<>))
import Data.String.NonEmpty (NonEmptyString, fromString)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import DataCite.JSON.Util (preParse, tryPrettyJson)
import DataCite.Types (Resource, Title)
import DataCite.Types.Common (Identifier, IdentifierType(..), altIdToId)
import Foreign (Foreign, ForeignError, isNull, isUndefined)
import Foreign as Foreign
import Foreign.Index as Foreign
import Prelude (class Applicative, bind, discard, identity, map, pure, ($), (<$>), (>>=))
import Simple.JSON (class ReadForeign)
import Simple.JSON as JSON
import Type.Data.Row (RProxy(..))


newtype JSONWithErr a = JSONWithErr (Writer (Array ForeignError) a)
derive instance newtypeJSONWithErr:: Newtype (JSONWithErr a) _
derive newtype instance jsonWithErrApply :: Apply JSONWithErr
derive newtype instance jsonWithErrApplicative :: Applicative JSONWithErr
derive newtype instance jsonWithErrFunctor :: Functor JSONWithErr
derive newtype instance jsonWithErrBind :: Bind JSONWithErr
derive newtype instance jsonWithErrMonad :: Monad JSONWithErr
derive newtype instance jsonWithErrTell :: MonadTell (Array ForeignError) JSONWithErr
derive newtype instance jsonWithErrWriter :: MonadWriter (Array ForeignError) JSONWithErr

newtype JSONParse a = JSONParse (ExceptT (NonEmptyList ForeignError) JSONWithErr a)
derive instance newtypeJSONParse:: Newtype (JSONParse a) _
derive newtype instance jsonParseApply :: Apply JSONParse
derive newtype instance jsonParseApplicative :: Applicative JSONParse
derive newtype instance jsonParseFunctor :: Functor JSONParse
derive newtype instance jsonParseBind :: Bind JSONParse
derive newtype instance jsonParseMonad :: Monad JSONParse
derive newtype instance jsonParseTell :: MonadTell (Array ForeignError) JSONParse
derive newtype instance jsonParseWriter :: MonadWriter (Array ForeignError) JSONParse
derive newtype instance jsonParseThrow :: MonadThrow (NonEmptyList ForeignError) JSONParse

generalize :: forall m a. Monad m => Identity a -> m a
generalize = unwrap >>> pure

-- note: Except e = ExceptT e Identity
genExcept :: forall m e a. Monad m => ExceptT e Identity a -> ExceptT e m a
genExcept = unwrap >>> generalize >>> ExceptT


toNonFatalDef :: forall a. a -> JSONParse a -> JSONParse a
toNonFatalDef def jParse = case jsonEi of
    Left (errs) -> do
      tell $ toUnfoldable errs
      pure def
    Right (_) -> jParse
  where
    Tuple jsonEi nfErrs = runWriter $ unwrap $ runExceptT $ unwrap jParse

toNonFatalOther :: forall a b. b -> JSONParse a -> JSONParse (Either b a)
toNonFatalOther def jParse = case jsonEi of
    Left (errs) -> do
      tell $ toUnfoldable errs
      pure $ Left def
    Right a -> pure $ Right a
  where
    Tuple jsonEi nfErrs = runWriter $ unwrap $ runExceptT $ unwrap jParse

-- Original API is:
-- readJSON' :: forall a. ReadForeign a => String -> F a
-- type F = Except MultipleErrors == ExceptT MultipleErrors Identity
readJSON' :: forall a. JSON.ReadForeign a => String -> JSONParse a
readJSON' = JSONParse <<< genExcept <<< JSON.readJSON'

read' :: forall a. ReadForeign a => Foreign -> JSONParse a
read' = JSONParse <<< genExcept <<< JSON.read'

readProp :: String -> Foreign -> JSONParse Foreign
readProp field fObj = JSONParse $ genExcept $ Foreign.readProp field fObj

readArray :: Foreign -> JSONParse (Array Foreign)
readArray = JSONParse <<< genExcept <<< Foreign.readArray

type IdTypePairF r = (identifier :: Foreign, identifierType :: Foreign | r)
type AltIdTypePairF r = (
  alternateIdentifier :: Foreign
, alternateIdentifierType :: Foreign | r
)


emptyRow :: RProxy ()
emptyRow = RProxy

-- | Note: calls `preParse`, unlike `readRecordJSON'`.
readRecordJSON :: String -> JSONWithErr (Either Foreign.MultipleErrors Resource)
readRecordJSON jsStr = runExceptT $ unwrap $ readRecordJSON' $ preParse jsStr

readRecordJSON' :: String -> JSONParse Resource
readRecordJSON' jsStr = do
  recBase <- readJSON' jsStr
  resId <- readNEString (ctxt "data.id") recBase.data.id
  idType <- readNEString (ctxt "data.type") recBase.data.type
  doi <- readNEString (ctxt "data.attributes.doi")
    recBase.data.attributes.doi
  doiPfx <- readNEString (ctxt "data.attributes.prefix")
    recBase.data.attributes.prefix
  doiSfx <- readNEString (ctxt "data.attributes.suffix")
    recBase.data.attributes.suffix
  idents <- traverse (readIdTypePairPx emptyRow (ctxt "data.attributes.identifiers"))
    recBase.data.attributes.identifiers
  altIdents <- traverse
    (readAltIdTypePairPx emptyRow (ctxt "data.attributes.alternateIdentifiers"))
    recBase.data.attributes.alternateIdentifiers
  creatorsIn <- readNEArray (ctxt "data.attributes.creators")
    recBase.data.attributes.creators
  creators <- mkCreators creatorsIn
  titles <- readNEArrayWith (ctxt "data.attributes.titles") readTitle
    recBase.data.attributes.titles
  publisher <- readNEString
    (ctxt "data.attributes.publisher") recBase.data.attributes.publisher
  containerMay <- mkCont recBase.data.attributes.container
  pure $ recBase { "data" {
      id = resId
    , "type" = idType
    , attributes {
        doi = doi
      , prefix = doiPfx
      , suffix = doiSfx
      , identifiers = idents
      , alternateIdentifiers = altIdents
      , creators = creators
      , titles = titles
      , publisher = publisher
      , container = containerMay
      }
    }}
  where
    outerCtxt = tryPrettyJson jsStr
    ctxt = context outerCtxt
    mkCreators ctors = traverse (\ctr -> do
      nameNE <- readNEString (ctxt "Creator name" ) ctr.name
      affilsNE <- traverse (readNEString (ctxt "Creator affiliations"))
        ctr.affiliation
      pure $ ctr {
          name = nameNE
        , nameType = ctr.nameType >>= fromString
        , givenName = ctr.givenName >>= fromString
        , familyName = ctr.familyName >>= fromString
        , affiliation = affilsNE
        }
      ) ctors
    mkCont contIn =
      if isNot contIn.type
        && isNot contIn.identifier
        && isNot contIn.identifierType then pure Nothing
      else do
        typeStr <- read' contIn."type"
        let typeMay = fromString typeStr
        idPair <- readIdTypePair (ctxt "container") contIn
        pure $ Just $ contIn {
            "type" = typeMay
          , identifier = idPair.identifier
          , identifierType = idPair.identifierType
          }

context :: Lazy String -> String -> Lazy String
context ctxt label = defer \_ ->
  "Couldn't read for " <> label <> " in: \n" <> (force ctxt)

readStr :: Foreign -> JSONParse String
readStr f = toNonFatalDef "" $ read' f

readNEString :: Lazy String -> Foreign -> JSONParse NonEmptyString
readNEString ctxt f = do
  str <- readStr f
  case fromString str of
    Just nes -> pure nes
    Nothing -> throwError $ pure $ Foreign.ForeignError $
      "Nonempty string expected in:\n" <> (force ctxt)

readNEArray :: forall a. JSON.ReadForeign a
  => Lazy String -> Foreign -> JSONParse (NonEmptyArray a)
readNEArray ctxt f = do
  arrF <- readArray f
  let arrEis = (JSON.read' >>> runExcept) <$> arrF
  traverse_ tell $ map toUnfoldable $ catLefts arrEis
  let arr = catRights arrEis
  case fromArray arr of
    Just nea -> pure nea
    Nothing -> throwError $ pure $ Foreign.ForeignError $
      "Nonempty array expected in:\n" <> (force ctxt)

readNEArrayWith :: forall a. Lazy String
  -> (Lazy String -> Foreign -> JSONParse a) -> Foreign
  -> JSONParse (NonEmptyArray a)
readNEArrayWith ctxt read f = do
  arrF <- readArray f
  -- Tuple jsonEi nfErrs = runWriter $ unwrap $ runExceptT $ unwrap jParse
  let arrTups = (read ctxt >>> unwrap >>> runExceptT >>> unwrap >>> runWriter)
        <$> arrF
  let arrEis = fst <$> arrTups
  let nfErrs = snd <$> arrTups
  traverse_ tell $ map toUnfoldable $ catLefts arrEis
  traverse_ tell nfErrs
  let arr = catRights arrEis
  case fromArray arr of
    Just nea -> pure nea
    Nothing -> throwError $ pure $ Foreign.ForeignError $
      "Nonempty array expected in:\n" <> (force ctxt)

readTitle :: Lazy String -> Foreign -> JSONParse Title
readTitle ctxt f = do
  titleF <- readProp "title" f
  title <- readNEString ctxt titleF
  pure $ {title: title}

-- | Assigns Handle as the default identifier type in case
-- | a document doesn't follow the schema.
readIdTypePair :: forall r. Lazy String -> Record (IdTypePairF r) -> JSONParse Identifier
readIdTypePair ctxt idPairF = do
  id <- readNEString ctxt idPairF.identifier
  let idTypeParse = read' idPairF.identifierType
  idType <- toNonFatalDef Handle idTypeParse
  pure $ {identifier: id, identifierType: idType}

readIdTypePairPx :: forall r. RProxy r
  -> Lazy String -> Record (IdTypePairF r) -> JSONParse Identifier
readIdTypePairPx _ ctxt idPairF = readIdTypePair ctxt idPairF

readAltIdTypePair :: forall r. Lazy String -> Record (AltIdTypePairF r) -> JSONParse Identifier
readAltIdTypePair ctxt idPairF = do
  id <- readNEString ctxt idPairF.alternateIdentifier
  let idTypeParse = read' idPairF.alternateIdentifierType
  idType <- toNonFatalDef Handle idTypeParse
  pure $ altIdToId {alternateIdentifier: id, alternateIdentifierType: idType}

readAltIdTypePairPx :: forall r. RProxy r
  -> Lazy String -> Record (AltIdTypePairF r) -> JSONParse Identifier
readAltIdTypePairPx _ ctxt idPairF = readAltIdTypePair ctxt idPairF

-- FIXME: remove
{- readIdTypePairPxDummy :: forall r. RProxy r
  -> Lazy String -> Record (IdTypePairF r) -> JSONParse (Array Identifier)
readIdTypePairPxDummy _ ctxt idPairF = pure [] -}

mayStrToStr :: Maybe String -> String
mayStrToStr (Just s) = s
mayStrToStr Nothing = ""

isNot :: Foreign -> Boolean
isNot x = isNull x || isUndefined x

catLefts ::  forall a b. Array (Either a b) -> Array a
catLefts = catMapLefts identity

catMapLefts :: forall a b c. (a -> c) -> Array (Either a b) -> Array c
catMapLefts f = A.concatMap (leftOr [] (A.singleton <<< f))

leftOr :: forall a b c. c -> (a -> c) -> Either a b -> c
leftOr _ f (Left a) = f a
leftOr c _ (Right _) = c

catRights ::  forall a b. Array (Either a b) -> Array b
catRights = catMapRights identity

catMapRights :: forall a b c. (b -> c) -> Array (Either a b) -> Array c
catMapRights f = A.concatMap (rightOr [] (A.singleton <<< f))

rightOr :: forall a b c. c -> (b -> c) -> Either a b -> c
rightOr c _ (Left _) = c
rightOr _ f (Right b) = f b
