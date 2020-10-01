let haskellCi =
      https://raw.githubusercontent.com/vmchale/github-actions-dhall/master/haskell-ci.dhall sha256:48c4cdf0faac0d1fd40884ff938abb3f1e8049e09a032fbec24e8ca337ce6ff9

in    haskellCi.generalCi
        [ haskellCi.checkout
        , haskellCi.haskellEnv haskellCi.matrixEnv
        , haskellCi.cabalDeps
        , haskellCi.cabalBuild
        , haskellCi.cabalDoc
        ]
        ( Some
            { ghc =
              [ haskellCi.GHC.GHC802
              , haskellCi.GHC.GHC822
              , haskellCi.GHC.GHC844
              , haskellCi.GHC.GHC865
              , haskellCi.GHC.GHC883
              ]
            , cabal = [ haskellCi.Cabal.Cabal30 ]
            }
        )
    : haskellCi.CI.Type
