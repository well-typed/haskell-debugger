module Test.Utils where

import System.FilePath
import System.IO.Temp
import qualified System.Process as P

-- | Copy the contents of a test directory (with a `*.hdb-test` in the root) to
-- a temporary location and return the path to the new location.
withHermeticDir :: Bool               -- ^ Whether to keep the temp dir around for inspection
                -> FilePath           -- ^ Test dir
                -> (FilePath -> IO r) -- ^ Continuation receives hermetic test dir (in temporary dir)
                -> IO r
withHermeticDir keep src k = do
  withTmpDir "hdb-test" $ \dest -> do
    P.callCommand $ "cp -r " ++ src ++ " " ++ dest
    k (dest </> takeBaseName src)
  where
    withTmpDir | keep      = withPersistentSystemTempDirectory
               | otherwise = withSystemTempDirectory

    withPersistentSystemTempDirectory :: String -> (FilePath -> IO r) -> IO r
    withPersistentSystemTempDirectory template k' = do
      dir <- flip createTempDirectory template =<< getCanonicalTemporaryDirectory
      k' dir

