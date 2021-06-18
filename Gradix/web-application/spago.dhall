{ name = "halogen-project"
, dependencies =
  [ "aff"
  , "affjax"
  , "argonaut"
  , "console"
  , "css"
  , "effect"
  , "halogen"
  , "halogen-css"
  , "halogen-select"
  , "halogen-vdom"
  , "parseint"
  , "psci-support"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
