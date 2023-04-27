{ name = "otp-reader"
, dependencies =
  [ "aff"
  , "arrays"
  , "avar"
  , "datetime"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "foreign"
  , "foreign-generic"
  , "foreign-object"
  , "maybe"
  , "newtype"
  , "numbers"
  , "parallel"
  , "prelude"
  , "presto"
  , "refs"
  , "strings"
  , "tracker"
  , "transformers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs"]
}