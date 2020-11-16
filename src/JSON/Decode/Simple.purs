module DataCite.JSON.Decode.Simple where

import Data.Array.NonEmpty (NonEmptyArray, fromArray)
import Data.Lazy (Lazy, force)
import DataCite.JSON.Util (tryPrettyJson)
import Control.Monad.Except (except, runExcept)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.String.NonEmpty (NonEmptyString, fromString)
import DataCite.Types (Resource)
import Foreign (F, Foreign)
import Foreign as Foreign
import Prelude (bind, pure, ($))
import Simple.JSON as JSON

-- TODO: make an optional preparser to remove fields that are definitely unused,
-- called before readRecordJSON; this will make the JSON easier to view
-- in case of errors.

readRecordJSON :: String -> Either Foreign.MultipleErrors Resource
readRecordJSON jsStr = runExcept do
  recBase <- JSON.readJSON' jsStr
  resId <- readNEStringImpl ctxt recBase.data.id
  idType <- readNEStringImpl ctxt recBase.data."type"
  doi <- readNEStringImpl ctxt recBase.data.attributes.doi
  doiPfx <- readNEStringImpl ctxt recBase.data.attributes.prefix
  doiSfx <- readNEStringImpl ctxt recBase.data.attributes.suffix
  creators <- readNEArrayImpl ctxt recBase.data.attributes.creators
  pure $ recBase { "data" {
      id = resId
    , "type" = idType
    , attributes {
        doi = doi
      , prefix = doiPfx
      , suffix = doiSfx
      , creators = creators
      }
    }}
  where
    ctxt = tryPrettyJson jsStr

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
