{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ sources = [ "src/**/*.purs", "test/**/*.purs" ]
, name = "purescript-datacite"
, dependencies = [ "effect", "console", "simple-json" ]
, packages = ./packages.dhall
}
