module DataCite.JSON.Util where

import Data.Lazy (Lazy)

foreign import tryPrettyJson :: String -> Lazy String

-- | Removes the `xml` attribute, which tends to be large.
foreign import preParse :: String -> String
