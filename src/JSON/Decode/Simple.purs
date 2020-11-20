module DataCite.JSON.Decode.Simple where

import Control.Monad.Except (except, runExcept)
import Data.Array.NonEmpty (NonEmptyArray, fromArray)
import Data.Either (Either(..))
import Data.HeytingAlgebra ((&&), (||))
import Data.Lazy (Lazy, force)
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.String.NonEmpty (NonEmptyString, fromString)
import Data.Traversable (traverse)
import DataCite.JSON.Util (tryPrettyJson)
import DataCite.Types (Resource)
import DataCite.Types.Common (Identifier)
import Foreign (F, Foreign, isNull, isUndefined)
import Foreign as Foreign
import Prelude (bind, pure, ($), (>>=))
import Simple.JSON as JSON
import Type.Data.Row (RProxy(..))

-- TODO: make an optional preparser to remove fields that are definitely unused,
-- called before readRecordJSON; this will make the JSON easier to view
-- in case of errors.

type IdTypePairF r = (identifier :: Foreign, identifierType :: Foreign | r)

emptyRow :: RProxy ()
emptyRow = RProxy

readRecordJSON :: String -> Either Foreign.MultipleErrors Resource
readRecordJSON jsStr = runExcept do
  recBase <- JSON.readJSON' jsStr
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
    mkCont contIn = if isNot contIn.type
        && isNot contIn.identifier
        && isNot contIn.identifierType then pure Nothing
      else do
        typeStr  <- JSON.read' contIn."type"
        let typeMay = fromString typeStr
        idPair <- readIdTypePair ctxt contIn
        pure $ Just $ contIn {
            "type" = typeMay
          , identifier = idPair.identifier
          , identifierType = idPair.identifierType
          }

readNEStringImpl :: Lazy String -> Foreign -> F NonEmptyString
readNEStringImpl ctxt f = do
  str :: String <- JSON.readImpl f
  except $ case fromString str of
    Just nes -> Right nes
    Nothing -> Left $ pure $ Foreign.ForeignError $ 
      "Nonempty string expected in:\n" <> (force ctxt)


readNEArrayImpl :: forall a. JSON.ReadForeign a
  => Lazy String -> Foreign -> F (NonEmptyArray a)
readNEArrayImpl ctxt f = do
  arr <- JSON.readImpl f
  except $ case fromArray arr of
    Just nea -> Right nea
    Nothing -> Left $ pure $ Foreign.ForeignError $ 
      "Nonempty array expected in:\n" <> (force ctxt)


readIdTypePair :: forall r. Lazy String -> Record (IdTypePairF r) -> F Identifier
readIdTypePair ctxt idPairF = do
  id <- readNEStringImpl ctxt idPairF.identifier
  idType <- JSON.read' idPairF.identifierType
  pure $ {identifier: id, identifierType: idType}

readIdTypePairPx :: forall r. RProxy r
  -> Lazy String -> Record (IdTypePairF r) -> F Identifier
readIdTypePairPx _ ctxt idPairF = readIdTypePair ctxt idPairF


mayStrToStr :: Maybe String -> String
mayStrToStr (Just s) = s
mayStrToStr Nothing = ""

isNot :: Foreign -> Boolean
isNot x = isNull x || isUndefined x