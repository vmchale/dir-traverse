module System.Directory.Recursive ( getDirRecursive ) where

import           Control.Applicative (pure, (<$>))
import           Control.Composition ((.*))
import           Control.Monad       (filterM)
import qualified Data.DList          as DL
import           Data.Foldable       (fold)
import           Data.Traversable    (traverse)
import           System.Directory    (doesDirectoryExist, listDirectory)
import           System.FilePath     ((</>))

{-# INLINE getDirRecursive #-}
getDirRecursive :: FilePath -> IO (DL.DList FilePath)
getDirRecursive fp = do
    all' <- listDirectory fp
    dirs <- filterM doesDirectoryExist (mkRel <$> all')
    case dirs of
        [] -> pure $ DL.fromList (mkRel <$> all')
        ds -> do
            next <- foldMapA getDirRecursive ds
            pure $ next `DL.append` DL.fromList (mkRel <$> all')

    where foldMapA = fmap fold .* traverse
          mkRel = (fp </>)
