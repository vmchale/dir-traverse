{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Monad.IO.Class     (liftIO)
import           Criterion.Main
import           Data.DirStream
import qualified Filesystem.Path            as F
import           Pipes                      (every, for, runEffect)
import           Pipes.Safe                 (runSafeT)
import           System.Directory.Recursive

dirstreamGet :: F.FilePath -> IO ()
dirstreamGet fp =
    runSafeT $ runEffect $
        for (every (descendentOf fp)) (liftIO . (\x -> pure $ x `seq` ()))

discard :: Functor f => f a -> f ()
discard = fmap (\x -> x `seq` ())

main :: IO ()
main =
    defaultMain [ bgroup "."
                      [ bench "getDirRecursive" $ nfIO (discard <$> getDirRecursive ".")
                      , bench "DirStream" $ nfIO (dirstreamGet ".")
                      ]
                ]
