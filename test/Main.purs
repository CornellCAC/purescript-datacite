module Test.Main where

import Control.Applicative (when)
import Data.Array as A
import Data.Either (isLeft, isRight)
import Data.String.Common (toLower)
import Data.String.Utils (endsWith)
import Data.Traversable (traverse, traverse_)
import DataCite.JSON.Decode.Simple (readRecordJSON)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Stats (Stats, isFile)
import Node.FS.Sync (readTextFile, readdir, stat)
import Node.Path (FilePath)
import Node.Process (cwd)
import Prelude (Unit, bind, discard, map, pure, show, ($), (&&), (<$>), (<>), (=<<))
import Test.Unit (Test, suite, test)
import Test.Unit.Assert (assert)
import Test.Unit.Main (runTest)

type FPAndStats = {fp :: FilePath, stats :: Stats}

testJsonDir :: FilePath
testJsonDir = "test/Data/Json/"

main :: Effect Unit
main = do
  repoDir <- cwd
  log $ "repo dir is: " <> repoDir
  -- log $ "Example lengths: " <> (show $ map A.length allJSonExs)
  log =<< readTextFile UTF8 "test/Data/Json/pubJsonEx.json"
  jsonFiles <- allJsonFiles
  log $ show $ jsonFiles
  runTest do
    suite "JSON functions" do
      test "Basic JSON success" $ traverse_ testJsonFile jsonFiles


-- | Basic tests to see if parsing didn't fantastically fail.
testJsonFile :: FilePath -> Test
testJsonFile fp = do
  fConts <- liftEffect $ readTextFile UTF8 fp
  let jsonRes = readRecordJSON fConts
  when (isLeft jsonRes) $ liftEffect $ log $ show $ jsonRes
  assert ("isRight on " <> fp) $ isRight jsonRes

allJsonFiles :: Effect (Array FilePath)
allJsonFiles = do
  dirContents <- readdir testJsonDir
  let dirContentsRel = (\f -> testJsonDir <> f) <$> dirContents
  dirContAndStats <- traverse withStats dirContentsRel
  pure $ map (\fps -> fps.fp) $ A.filter isJsonFile dirContAndStats

isJsonFile :: FPAndStats -> Boolean
isJsonFile fps = (endsWith ".json" $toLower fps.fp)
  && (isFile fps.stats)

withStats :: FilePath -> Effect FPAndStats
withStats fp = do
  fStats <- stat fp
  pure {fp: fp, stats: fStats}
