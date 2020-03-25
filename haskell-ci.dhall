let haskellCi =
      https://raw.githubusercontent.com/vmchale/github-actions-dhall/master/haskell-ci.dhall sha256:2de95c8bd086c21660c2849dfe2d9af72e675bed44396159d647292d329a20e4

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
