module DataCite.JSON.Decode.Simple where

import Control.Apply (class Apply)
import Control.Bind (class Bind)
import Control.Category ((<<<), (>>>))
import Control.Monad (class Monad)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (runExcept, runExceptT)
import Control.Monad.Except.Trans (ExceptT(..))
import Control.Monad.Writer (Writer)
import Control.Monad.Writer.Class (class MonadTell, class MonadWriter, tell)
import Data.Array as A
import Data.Array.NonEmpty (NonEmptyArray, fromArray)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Functor (class Functor)
import Data.HeytingAlgebra ((&&), (||))
import Data.Identity (Identity)
import Data.Lazy (Lazy, force)
import Data.List.NonEmpty (toUnfoldable)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Semigroup ((<>))
import Data.String.NonEmpty (NonEmptyString, fromString)
import Data.Traversable (traverse)
import DataCite.JSON.Util (tryPrettyJson)
import DataCite.Types (Resource)
import DataCite.Types.Common (Identifier)
import Foreign (Foreign, ForeignError, isNull, isUndefined)
import Foreign as Foreign
import Prelude (class Applicative, bind, identity, map, pure, ($), (>>=))
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

-- Original API is:
-- readJSON' :: forall a. ReadForeign a => String -> F a
-- type F = Except MultipleErrors == ExceptT MultipleErrors Identity
readJSON' :: forall a. JSON.ReadForeign a => String -> JSONParse a
readJSON' = JSONParse <<< genExcept <<< JSON.readJSON'

read' :: forall a. ReadForeign a => Foreign -> JSONParse a
read' = JSONParse <<< genExcept <<< JSON.read'

readArray :: Foreign -> JSONParse (Array Foreign)
readArray = JSONParse <<< genExcept <<< Foreign.readArray

-- TODO: make an optional preparser to remove fields that are definitely unused,
-- called before readRecordJSON; this will make the JSON easier to view
-- in case of errors.

type IdTypePairF r = (identifier :: Foreign, identifierType :: Foreign | r)

emptyRow :: RProxy ()
emptyRow = RProxy

readRecordJSON :: String -> JSONWithErr (Either Foreign.MultipleErrors Resource)
readRecordJSON jsStr = runExceptT $ unwrap $ readRecordJSON' jsStr

readRecordJSON' :: String -> JSONParse Resource
readRecordJSON' jsStr = do
  recBase <- readJSON' jsStr
  resId <- readNEStringImpl ctxt recBase.data.id
  idType <- readNEStringImpl ctxt recBase.data.type
  doi <- readNEStringImpl ctxt recBase.data.attributes.doi
  doiPfx <- readNEStringImpl ctxt recBase.data.attributes.prefix
  doiSfx <- readNEStringImpl ctxt recBase.data.attributes.suffix
  idents  <- traverse (readIdTypePairPx emptyRow ctxt)
    recBase.data.attributes.identifiers
  altIdents <- traverse (readIdTypePairPx emptyRow ctxt)
    recBase.data.attributes.alternateIdentifiers
  creatorsIn <- readNEArrayImpl ctxt recBase.data.attributes.creators
  creators <- mkCreators creatorsIn
  titles <- readNEArrayImpl ctxt recBase.data.attributes.titles
  publisher <- readNEStringImpl ctxt recBase.data.attributes.publisher
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
    ctxt = tryPrettyJson jsStr
    mkCreators ctors = traverse (\ctr -> do
      nameNE <- readNEStringImpl ctxt ctr.name
      affilsNE <- traverse (readNEStringImpl ctxt) ctr.affiliation
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
        typeStr  <- read' contIn."type"
        let typeMay = fromString typeStr
        idPair <- readIdTypePair ctxt contIn
        pure $ Just $ contIn {
            "type" = typeMay
          , identifier = idPair.identifier
          , identifierType = idPair.identifierType
          }

readNEStringImpl :: Lazy String -> Foreign -> JSONParse NonEmptyString
readNEStringImpl ctxt f = do
  str :: String <- read' f
  case fromString str of
    Just nes -> pure nes
    Nothing -> throwError $ pure $ Foreign.ForeignError $
      "Nonempty string expected in:\n" <> (force ctxt)


readNEArrayImpl :: forall a. JSON.ReadForeign a
  => Lazy String -> Foreign -> JSONParse (NonEmptyArray a)
readNEArrayImpl ctxt f = do
  arrF <- readArray f
  let arrEis = map (JSON.read' >>> runExcept) arrF
  _ <- traverse_ tell $ map toUnfoldable $ catLefts arrEis
  let arr = catRights arrEis
  case fromArray arr of
    Just nea -> pure nea
    Nothing -> throwError $ pure $ Foreign.ForeignError $
      "Nonempty array expected in:\n" <> (force ctxt)


readIdTypePair :: forall r. Lazy String -> Record (IdTypePairF r) -> JSONParse Identifier
readIdTypePair ctxt idPairF = do
  id <- readNEStringImpl ctxt idPairF.identifier
  idType <- read' idPairF.identifierType
  pure $ {identifier: id, identifierType: idType}

readIdTypePairPx :: forall r. RProxy r
  -> Lazy String -> Record (IdTypePairF r) -> JSONParse Identifier
readIdTypePairPx _ ctxt idPairF = readIdTypePair ctxt idPairF


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
