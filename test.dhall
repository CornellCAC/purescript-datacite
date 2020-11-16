{-
  This spago config adds dependencies used for testing
-}
let conf = ./spago.dhall
in conf // { 
  sources = conf.sources # [ "test/**/*.purs" ]
, name = "purescript-datacite-tests"
, dependencies = conf.dependencies # [
    "console"
  , "node-fs"
  , "node-process"
  , "stringutils"
  , "test-unit"
  ]
, packages = ./packages.dhall
}
