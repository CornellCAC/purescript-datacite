{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ sources = [ "src/**/*.purs" ]
, name = "purescript-datacite"
, dependencies = [ "effect", "generics-rep", "simple-json" ]
, packages = ./packages.dhall
}
