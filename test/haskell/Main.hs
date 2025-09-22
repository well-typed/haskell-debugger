{-# LANGUAGE LambdaCase, OverloadedStrings, ViewPatterns, QuasiQuotes #-}
module Main (main) where

import Text.RE.TDFA.Text.Lazy
import Text.Printf
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified System.Process as P
import System.FilePath
import System.IO.Temp
import System.Exit
import Control.Exception

import Test.Tasty
import Test.Tasty.Golden as G
import Test.Tasty.Golden.Advanced as G

main :: IO ()
main = do
  goldens <- mapM (mkGoldenTest False) =<< findByExtension [".hdb-test"] "test/golden"
  defaultMain $
    testGroup "Tests"
      goldens

-- | Receives as an argument the path to the @*.hdb-test@ which contains the
-- shell invocation for running 
mkGoldenTest :: Bool -> FilePath -> IO TestTree
mkGoldenTest keepTmpDirs path = do
  let testName   = takeBaseName     path
  let goldenPath = replaceExtension path ".hdb-stdout"
  return (goldenVsStringComparing testName goldenPath action)
  where
    action :: IO LBS.ByteString
    action = do
      script <- readFile path
      withHermeticDir keepTmpDirs (takeDirectory path) $ \test_dir -> do
        (_, Just hout, _, p)
          <- P.createProcess (P.shell script){P.cwd = Just test_dir, P.std_out = P.CreatePipe}
        P.waitForProcess p >>= \case
          ExitSuccess   -> LBS.hGetContents hout
          ExitFailure c -> error $ "Test script in " ++ test_dir ++ " failed with exit code: " ++ show c
        
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

--------------------------------------------------------------------------------
-- Tasty Golden Advanced wrapper
--------------------------------------------------------------------------------

-- | Compare a given string against the golden file's contents using the given normalising function
-- This is inlined from 'goldenVsString' and the accompanying functions. We
-- wanted the same but with a normalising function.
goldenVsStringComparing
  :: TestName -- ^ test name
  -> FilePath -- ^ path to the «golden» file (the file that contains correct output)
  -> IO LBS.ByteString
  -- ^ action that returns a string
  -> TestTree
  -- ^ the test verifies that the returned string is the same as the golden file contents
goldenVsStringComparing name ref act =
  goldenTest name (readFileStrict ref) act cmpNormalising upd
  where
  upd = createDirectoriesAndWriteFile ref

  readFileStrict :: FilePath -> IO LBS.ByteString
  readFileStrict path = do
    s <- LBS.readFile path
    evaluate $ forceLbs s
    return s

  forceLbs :: LBS.ByteString -> ()
  forceLbs = LBS.foldr seq ()

--------------------------------------------------------------------------------
-- Normalisation
--------------------------------------------------------------------------------

  -- | Compare the golden test against the actual output after normalisation
  cmpNormalising :: LBS.ByteString -> LBS.ByteString -> IO (Maybe String)
  cmpNormalising x y = do

    tmpDir <- getCanonicalTemporaryDirectory
    replaceRE <- compileSearchReplace (tmpDir ++ ".*" ++ takeBaseName (takeDirectory ref) {- the folder in which the test is run, inside the canonical temp dir-})
                                      "<TEMPORARY-DIRECTORY>"

    let
      msg = printf "Test output was different from '%s'. It was:\n" ref <> (LT.unpack (normalising y))

      normalising (LT.decodeUtf8 -> txt) =
        txt *=~/ replaceRE

    return $ if normalising x == normalising y then Nothing else Just msg

