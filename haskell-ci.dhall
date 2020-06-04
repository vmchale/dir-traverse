let haskellCi =
      https://raw.githubusercontent.com/vmchale/github-actions-dhall/master/haskell-ci.dhall sha256:abbcf1ffd0630835e80fe7c953e6fd2cacc8f8a2f70a0250b7e8f5a68171b232

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
