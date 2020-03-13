module Main ( main ) where

import           System.Directory.Recursive (getDirRecursive)
import           System.Environment         (getArgs)

main :: IO ()
main = force $ getDirRecursive . head =<< getArgs

force :: IO [FilePath] -> IO ()
force x = (\fps -> last fps `seq` mempty) =<< x
