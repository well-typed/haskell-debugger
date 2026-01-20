{-# LANGUAGE LambdaCase, OverloadedStrings, ViewPatterns, QuasiQuotes, CPP #-}
module Main (main) where

import Data.List (isSuffixOf)
import qualified Data.Set as Set
import Text.RE.TDFA.Text.Lazy
import Text.Printf
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text.Lazy.IO as LT
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified System.Process as P
import System.FilePath
import System.IO.Temp
import System.Exit
import System.IO
import System.Environment
import Control.Exception

import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.Golden as G
import Test.Tasty.Golden.Advanced as G

import Test.DAP.RunInTerminal
import Test.Utils

main :: IO ()
main = do
  env <- getEnvironment
  let mkTest = mkGoldenTest False env
  golden_tests_paths <- findByExtension [".hdb-test"] "test/golden"

  let internalOnlyTests = filter (\p -> ".internal" `isSuffixOf` takeBaseName p) golden_tests_paths
  let externalOnlyTests = filter (\p -> ".external" `isSuffixOf` takeBaseName p) golden_tests_paths

  let internalOnlySet = Set.fromList internalOnlyTests
  let externalOnlySet = Set.fromList externalOnlyTests
  let allTestsSet     = Set.fromList golden_tests_paths

  let defaultTestsSet = allTestsSet `Set.difference` (internalOnlySet `Set.union` externalOnlySet)

  let testsForInternal = Set.toList $ internalOnlySet `Set.union` defaultTestsSet
  let testsForExternal = Set.toList $ externalOnlySet `Set.union` defaultTestsSet

  default_goldens   <- mapM (mkTest "") testsForExternal
  intinterp_goldens <- mapM (mkTest "--internal-interpreter") testsForInternal

  defaultMain $
#ifdef mingw32_HOST_OS
    ignoreTestBecause "Testsuite is not enabled on Windows (#149)" $
#endif
    testGroup "Tests"
      [ testGroup "Golden tests" default_goldens
      , testGroup "Golden tests (--internal-interpreter)" intinterp_goldens
      , testGroup "Unit tests" unitTests
      ]

unitTests :: [TestTree]
unitTests =
  [ runInTerminalTests
  ]

-- | Receives as an argument the path to the @*.hdb-test@ which contains the
-- shell invocation for running
mkGoldenTest :: Bool -> [(String, String)] -> FilePath -> String -> IO TestTree
mkGoldenTest keepTmpDirs inheritedEnv flags path = do
  let testName   = takeBaseName     path
  let goldenPath = replaceExtension path ".hdb-stdout"
  return $ goldenVsStringComparing testName goldenPath action
  where
    action :: IO LBS.ByteString
    action = do
      script <- readFile path
      withHermeticDir keepTmpDirs (takeDirectory path) $ \test_dir -> do
        (_, Just hout, _, p)
          <- P.createProcess (P.shell script)
            { P.cwd = Just test_dir, P.std_out = P.CreatePipe
            , P.env = Just $
              inheritedEnv ++
              [ ("HDB", "hdb " ++ flags)
              ]
            }
        P.waitForProcess p >>= \case
          ExitSuccess   -> LBS.hGetContents hout
          ExitFailure c -> error $ "Test script in " ++ test_dir ++ " failed with exit code: " ++ show c

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
goldenVsStringComparing name ref act = do

  -- Normalise the output. The test file should already be saved normalised.
  goldenTest name (LT.decodeUtf8 <$> readFileStrict ref) normalisingAct cmpNormalising upd

  where
  upd = createDirectoriesAndWriteFile ref . LT.encodeUtf8

  escapePathSeparators c =
    if isPathSeparator c
      then "\\" ++ [c]
      else [c]

  escapeRegex :: String -> String
  escapeRegex = concatMap escapePathSeparators

  -- Normalise the action producing the output
  normalisingAct = do
    tmpDir <- getCanonicalTemporaryDirectory
    replaceREs <- traverse (uncurry compileSearchReplace)
      [ ( escapeRegex $ tmpDir </> "[^/\\]+" </> takeBaseName (takeDirectory ref) {- the folder in which the test is run, inside the canonical temp dir-}
        , "<TEMPORARY-DIRECTORY>" )
      , ( "Using cabal specification: .*"
        , "Using cabal specification: <VERSION>" )
      ]

    let normalising (LT.decodeUtf8 -> txt) = foldl' (*=~/) txt replaceREs

    normalising <$> act

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
  cmpNormalising :: LT.Text -> LT.Text -> IO (Maybe String)
  cmpNormalising x y = do

    let
      msg = printf "Test output was different from '%s'. It was:\n" ref <> (LT.unpack y)

    if x == y
      then return Nothing
      else do
        -- Call diff to show the difference
        withSystemTempFile "x.txt" $ \xf xH -> do
          withSystemTempFile "y.txt" $ \yf yH -> do
            LT.hPutStr xH x
            LT.hPutStr yH y
            hFlush xH
            hFlush yH
            hClose xH
            hClose yH
            (_exitCode, out, err) <- P.readProcessWithExitCode "diff" ["-u", xf, yf] ""
            return $ Just $ msg ++ "\nDiff output:\n" ++ out ++ err
