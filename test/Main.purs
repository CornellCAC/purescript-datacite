module Test.Main where

import Prelude
import Data.Array as A
import Test.Data

import Effect (Effect)
import Effect.Console (log)
import Node.Process (cwd)

main :: Effect Unit
main = do
  repoDir <- cwd
  log $ "repo dir is: " <> repoDir
  -- log $ "Example lengths: " <> (show $ map A.length allJSonExs)

