module Main (main) where

import           Criterion.Main
import           System.Directory.Recursive

main :: IO ()
main =
    defaultMain [ bgroup "getDirRecursive"
                      [ bench "." $ nfIO (getDirRecursive ".") ]
                ]
