module DataCite.JSON.Util where

import Data.Lazy (Lazy)

foreign import tryPrettyJson :: String -> Lazy String
