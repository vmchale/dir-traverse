module Main (main) where

import           Control.Composition        ((.*))
import           Control.Monad              (filterM)
import           Criterion.Main
import           Data.Foldable              (fold, toList)
import           System.Directory           (doesDirectoryExist,
                                             getDirectoryContents)
import           System.Directory.Recursive
import           System.FilePath            ((</>))

getDirRecursiveList :: FilePath -> IO [FilePath]
getDirRecursiveList fp = do
    all' <- exclude <$> getDirectoryContents fp
    dirs <- exclude <$> filterM doesDirectoryExist (mkRel <$> all')
    case dirs of
        [] -> pure (mkRel <$> all')
        ds -> do
            next <- foldMapA getDirRecursiveList ds
            pure $ next <> (mkRel <$> all')

    where foldMapA = fmap fold .* traverse
          exclude = filter (\x -> x /= "." && x /= "..")
          mkRel = (fp </>)

main :: IO ()
main =
    defaultMain [ bgroup "getDirRecursive"
                      [ bench "DList" $ nfIO (toList <$> getDirRecursive "/home/vanessa/programming/haskell")
                      , bench "[]" $ nfIO (getDirRecursiveList "/home/vanessa/programming/haskell")
                      ]
                ]
