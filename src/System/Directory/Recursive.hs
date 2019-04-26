module System.Directory.Recursive ( getDirRecursive ) where

import           Control.Composition ((.*))
import           Control.Monad       (filterM)
import qualified Data.DList          as DL
import           Data.Foldable       (fold)
import           System.Directory    (doesDirectoryExist, getDirectoryContents)
import           System.FilePath     ((</>))

{-# INLINE getDirRecursive #-}
getDirRecursive :: FilePath -> IO (DL.DList FilePath)
getDirRecursive fp = do
    all' <- exclude <$> getDirectoryContents fp
    dirs <- exclude <$> filterM doesDirectoryExist (mkRel <$> all')
    case dirs of
        [] -> pure $ DL.fromList (mkRel <$> all')
        ds -> do
            next <- foldMapA getDirRecursive ds
            pure $ next <> DL.fromList (mkRel <$> all')

    where foldMapA = fmap fold .* traverse
          exclude = filter (\x -> x /= "." && x /= "..")
          mkRel = (fp </>)
