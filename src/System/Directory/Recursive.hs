module System.Directory.Recursive ( getDirRecursive
                                  , getSubdirsRecursive
                                  , getDirFiltered
                                  ) where

import           Control.Applicative (pure, (<$>))
import           Control.Monad       (filterM)
import           Data.Foldable       (fold)
import           Data.Traversable    (traverse)
import           System.Directory    (doesDirectoryExist, listDirectory)
import           System.FilePath     ((</>))


-- | @since 0.2.1.0
getSubdirsRecursive :: FilePath -> IO [FilePath]
getSubdirsRecursive = getDirFiltered doesDirectoryExist

getDirRecursive :: FilePath -> IO [FilePath]
getDirRecursive = getDirFiltered (const (pure True))

{-# INLINE getDirFiltered #-}
-- | @since 0.2.2.0
getDirFiltered :: (FilePath -> IO Bool) -> FilePath -> IO [FilePath]
getDirFiltered p fp = do
    all' <- listDirectory fp
    all'' <- filterM p (mkRel <$> all')
    dirs <- filterM doesDirectoryExist all''
    case dirs of
        [] -> pure all''
        ds -> do
            next <- foldMapA (getDirFiltered p) ds
            pure $ all'' ++ next

    where mkRel = (fp </>)
          foldMapA = (fmap fold .) . traverse
