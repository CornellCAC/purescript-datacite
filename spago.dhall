{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ sources = [ "src/**/*.purs" ]
, name = "purescript-datacite"
, dependencies = [
    "effect"
  , "either-extra"
  , "functors"
  , "generics-rep"
  , "naturals"
  , "simple-json"
  ]
, packages = ./packages.dhall
}
