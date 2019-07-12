module System.Directory.Recursive ( getDirRecursive
                                  , getSubdirsRecursive
                                  ) where

import           Control.Applicative (pure, (<$>))
import           Control.Monad       (filterM)
import           Data.Foldable       (fold)
import           Data.Traversable    (traverse)
import           System.Directory    (doesDirectoryExist, listDirectory)
import           System.FilePath     ((</>))


{-# INLINE getSubdirsRecursive #-}
-- | @since 0.2.1.0
getSubdirsRecursive :: FilePath -> IO [FilePath]
getSubdirsRecursive fp = do
    all' <- listDirectory fp
    let all'' = mkRel <$> all'
    dirs <- filterM doesDirectoryExist all''
    case dirs of
        [] -> pure dirs
        ds -> do
            next <- foldMapA getSubdirsRecursive ds
            pure $ dirs ++ next

    where mkRel = (fp </>)
          foldMapA = (fmap fold .) . traverse

{-# INLINE getDirRecursive #-}
getDirRecursive :: FilePath -> IO [FilePath]
getDirRecursive fp = do
    all' <- listDirectory fp
    let all'' = mkRel <$> all'
    dirs <- filterM doesDirectoryExist all''
    case dirs of
        [] -> pure all''
        ds -> do
            next <- foldMapA getDirRecursive ds
            pure $ all'' ++ next

    where mkRel = (fp </>)
          foldMapA = (fmap fold .) . traverse
