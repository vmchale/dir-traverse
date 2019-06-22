{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Monad              (filterM, (<=<))
import           Control.Monad.IO.Class     (liftIO)
import           Criterion.Main
import           Data.DirStream
import qualified Filesystem.Path            as F
import           Pipes                      (every, for, runEffect)
import           Pipes.Safe                 (runSafeT)
import           System.Directory           (doesDirectoryExist)
import           System.Directory.Recursive


-- todo use http://hackage.haskell.org/package/pipes-4.3.9/docs/Pipes-Prelude.html#v:toListM
dirstreamGet :: F.FilePath -> IO ()
dirstreamGet fp =
    runSafeT $ runEffect $
        for (every (descendentOf fp)) (liftIO . (\x -> pure $ x `seq` ()))

discard :: Functor f => f a -> f ()
discard = fmap (\x -> x `seq` ())

filtered :: FilePath -> IO [FilePath]
filtered = filterM doesDirectoryExist <=< getDirRecursive

main :: IO ()
main =
    defaultMain [ bgroup "."
                      [ bench "getDirRecursive" $ nfIO (discard <$> getDirRecursive ".")
                      , bench "DirStream" $ nfIO (dirstreamGet ".")
                      ]
                , bgroup "dirs"
                      [ bench "getSubdirsRecursive" $ nfIO (discard <$> getSubdirsRecursive ".")
                      , bench "filterM doesDirectoryExist <=< getDirRecursive" $ nfIO (discard <$> filtered ".")
                      ]
                ]
