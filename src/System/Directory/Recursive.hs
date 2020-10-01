module System.Directory.Recursive ( getDirRecursive
                                  , getSubdirsRecursive
                                  , getFilesRecursive
                                  , getDirFiltered
                                  ) where

import           Control.Applicative (pure, (<$>))
import           Control.Monad       (filterM)
import           Data.Foldable       (fold)
import           Data.Traversable    (traverse)
import           System.Directory    (doesDirectoryExist, doesFileExist, listDirectory)
import           System.FilePath     ((</>))
import           System.IO.Unsafe    (unsafeInterleaveIO)


-- | Recursively get all subdirectories in the given directory.
--
-- @since 0.2.1.0
getSubdirsRecursive :: FilePath -> IO [FilePath]
getSubdirsRecursive = getDirFiltered doesDirectoryExist

-- | Recursively get all files and subdirectories in the given directory.
getDirRecursive :: FilePath -> IO [FilePath]
getDirRecursive = getDirFiltered (const $ pure True)

-- | Recursively get all files in the given directory.
--
-- @since TODO
getFilesRecursive :: FilePath -> IO [FilePath]
getFilesRecursive fp = getDirRecursive fp >>= filterM doesFileExist

{-# INLINE getDirFiltered #-}
-- | Recursively get all files and subdirectories in the given directory that
-- satisfy the given predicate. Note that the content of subdirectories not
-- matching the filter is ignored. In particular, that means something like
-- @getDirFiltered doesFileExist@ will /not/ recursively return all files.
--
-- @since 0.2.2.0
getDirFiltered :: (FilePath -> IO Bool) -- ^ Filepath filter
               -> FilePath
               -> IO [FilePath]
getDirFiltered p fp = do
    all' <- listDirectory fp
    all'' <- filterM p (mkRel <$> all')
    dirs <- filterM doesDirectoryExist all''
    case dirs of
        [] -> pure all''
        ds -> do
            next <- unsafeInterleaveIO $ foldMapA (getDirFiltered p) ds
            pure $ all'' ++ next

    where mkRel = (fp </>)
          foldMapA = (fmap fold .) . traverse
