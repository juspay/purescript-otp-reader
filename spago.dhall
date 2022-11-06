{ name = "otp-reader"
, dependencies = 
    [ "effect"
    , "prelude"
    , "presto"
    , "foreign-generic"
    , "tracker"
    , "aff"
    , "arrays"
    , "avar"
    , "datetime"
    , "either"
    , "exceptions"
    , "foldable-traversable"
    , "foreign"
    , "foreign-object"
    , "maybe"
    , "newtype"
    , "numbers"
    , "parallel"
    , "refs"
    , "strings"
    , "transformers"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
