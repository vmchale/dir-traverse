let dhallCi =
      https://raw.githubusercontent.com/vmchale/github-actions-dhall/master/dhall-ci.dhall sha256:ca3826e88b36decf1546e017eedce8def804eee021ef550562e09710f435bbbd

in    dhallCi.dhallSteps
        [ dhallCi.dhallYamlInstall
        , dhallCi.checkDhallYaml
            [ "dhall-ci.dhall", "haskell-ci.dhall", "hlint-ci.dhall" ]
        ]
    : dhallCi.CI
