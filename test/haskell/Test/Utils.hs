module Test.Utils where

import Control.Monad (when)
import Data.List (isInfixOf)
import System.Directory (doesFileExist)
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
    let destTestDir = dest </> takeBaseName src
    -- Some test projects reference @./haskell-debugger-view@ in their
    -- @cabal.project@. If such a dir is expected, copy the in-tree
    -- @haskell-debugger-view@ there so that cabal can resolve it when
    -- building the test project from its hermetic copy.
    cpHaskellDebuggerViewIfNeeded destTestDir
    k destTestDir
  where
    withTmpDir | keep      = withPersistentSystemTempDirectory
               | otherwise = withSystemTempDirectory

    withPersistentSystemTempDirectory :: String -> (FilePath -> IO r) -> IO r
    withPersistentSystemTempDirectory template k' = do
      dir <- flip createTempDirectory template =<< getCanonicalTemporaryDirectory
      k' dir

    cpHaskellDebuggerViewIfNeeded testDir = do
      let cabalProject = testDir </> "cabal.project"
      existsCP <- doesFileExist cabalProject
      when existsCP $ do
        contents <- readFile cabalProject
        when ("haskell-debugger-view" `isInfixOf` contents) $
          P.callCommand $
            "cp -r haskell-debugger-view " ++ testDir </> "haskell-debugger-view"

