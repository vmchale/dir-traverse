module System.Directory.Recursive ( getDirRecursive ) where

import           Control.Applicative (pure, (<$>))
import           Control.Monad       (filterM)
import           Data.Foldable       (fold)
import           Data.Traversable    (traverse)
import           System.Directory    (doesDirectoryExist, getDirectoryContents)
import           System.Info         (os)

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

    where foldMapA = (fmap fold .) . traverse
          mkRel = (fp </>)
          (</>) x y =
            case os of
                "mingw32" -> x ++ "\\" ++ y
                _         -> x ++ "/" ++ y
          listDirectory = fmap (filter (\p -> p /= "." && p /= "..")) . getDirectoryContents
