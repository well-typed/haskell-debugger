{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Development.Debug.Adapter.Flags where

import Control.Applicative ((<|>))
import Control.Exception (handleJust)
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.Bifunctor
import Data.Function
import Data.Functor ((<&>))
import Data.Maybe
import Data.Version
import Data.Void
import System.Directory hiding (findFile)
import System.FilePath
import System.IO.Error
import Text.ParserCombinators.ReadP (readP_to_S)
import Prettyprinter

import qualified HIE.Bios as HIE
import qualified HIE.Bios.Config as Config
import qualified HIE.Bios.Cradle as HIE
import qualified HIE.Bios.Environment as HIE
import qualified HIE.Bios.Types as HIE
import qualified Hie.Cabal.Parser as Implicit
import qualified Hie.Locate as Implicit
import qualified Hie.Yaml as Implicit

import GHC.Debugger.Logger

data FlagsLog
  = HieBiosLog HIE.Log
  | LogCradle (HIE.Cradle Void)

instance Pretty FlagsLog where
  pretty = \ case
    HieBiosLog msg -> pretty msg
    LogCradle crdl -> "Determined Cradle:" <+> viaShow crdl

-- | Flags inferred by @hie-bios@ to invoke GHC
data HieBiosFlags = HieBiosFlags
      { ghcInvocation :: [String]
      , libdir :: FilePath
      , units :: [String]
      , rootDir :: FilePath
      -- ^ Root dir as reported by the 'Cradle'
      , componentDir :: FilePath
      -- ^ Root dir of the loaded 'ComponentOptions'.
      -- Important for multi-package cabal projects, as packages are not in the
      -- root of the cradle, but in some sub-directory.
      }

hieBiosCradle :: Recorder (WithSeverity FlagsLog) {-^ Logger -}
              -> FilePath {-^ Project root -}
              -> FilePath {-^ Entry file relative to root -}
              -> IO (Either String (HIE.Cradle Void))
hieBiosCradle logger root relTarget = runExceptT $ do
  let target = root </> relTarget
  explicitCradle <- HIE.findCradle target & liftIO
  cradle <- maybe (loadImplicitCradle hieBiosLogger target)
                  (HIE.loadCradle hieBiosLogger) explicitCradle & liftIO
  logWith logger Info $ LogCradle cradle
  pure cradle
  where
    hieBiosLogger = toCologAction $ cmapWithSev HieBiosLog logger

hieBiosRuntimeGhcVersion :: Recorder (WithSeverity FlagsLog)
                         -> HIE.Cradle Void
                         -> IO (Either String Version)
hieBiosRuntimeGhcVersion _logger cradle = runExceptT $ do
  out <- liftIO (HIE.getRuntimeGhcVersion cradle) >>= unwrapCradleResult "Failed to get runtime GHC version"
  case versionMaybe out of
    Nothing -> throwError $ "Failed to parse GHC version: " <> out
    Just ver -> pure ver

-- | Make 'HieBiosFlags' from the given target file
hieBiosFlags :: Recorder (WithSeverity FlagsLog) {-^ Logger -}
             -> HIE.Cradle Void {-^ Project cradle the entry file belongs to -}
             -> FilePath {-^ Project root -}
             -> FilePath {-^ Entry file relative to root -}
             -> IO (Either String HieBiosFlags)
hieBiosFlags _logger cradle root relTarget = runExceptT $ do
  let target = root </> relTarget
  libdir <- liftIO (HIE.getRuntimeGhcLibDir cradle) >>= unwrapCradleResult "Failed to get runtime GHC libdir"

  -- To determine the flags we MUST set the current directory to the root
  -- because hie.yaml may invoke programs relative to the root (e.g. GHC's hie.yaml does)
  -- (HIE.getCompilerOptions depends on CWD being the proper root dir)
  let compilerOpts = liftIO $ withCurrentDirectory root $
#if MIN_VERSION_hie_bios(0,14,0)
                          HIE.getCompilerOptions target (HIE.LoadWithContext [target]) cradle
#else
                          HIE.getCompilerOptions target [] cradle
#endif
  componentOpts <- compilerOpts >>= unwrapCradleResult "Failed to get compiler options using hie-bios cradle"
#if __GLASGOW_HASKELL__ >= 913
  -- fwrite-if-simplified-core requires a recent bug fix regarding GHCi loading
  -- ROMES:TODO: Re-enable as soon as I'm using Matthew's patch.
  -- ["-fwrite-if-simplified-core"] ++
#endif

  let (units', flags') = extractUnits (HIE.componentOptions componentOpts)
  return HieBiosFlags
    { ghcInvocation = flags' ++ ghcDebuggerFlags
    , libdir = libdir
    , units  = units'
    , rootDir = HIE.cradleRootDir cradle
    , componentDir = HIE.componentRoot componentOpts
    }

unwrapCradleResult :: MonadError String m => [Char] -> HIE.CradleLoadResult a -> m a
unwrapCradleResult m = \case
  HIE.CradleNone      -> throwError $ "HIE.CradleNone\n" ++ m
  HIE.CradleFail err  -> throwError $ unlines (HIE.cradleErrorStderr err) ++ "\n" ++ m
  HIE.CradleSuccess x -> return x

extractUnits :: [String] -> ([String], [String])
extractUnits = go [] []
  where
    -- TODO: we should likely use the 'processCmdLineP' instead
    go units rest ("-unit" : x : xs) = go (x : units) rest xs
    go units rest (x : xs)           = go units (x : rest) xs
    go units rest []                 = (reverse units, reverse rest)

-- | Flags specific to haskell-debugger to append to all GHC invocations.
ghcDebuggerFlags :: [String]
ghcDebuggerFlags =
  [ "-fno-it" -- don't introduce @it@ after evaluating something at the prompt
  ]


-- ----------------------------------------------------------------------------
-- Utilities
-- ----------------------------------------------------------------------------

versionMaybe :: String -> Maybe Version
versionMaybe xs = case reverse $ readP_to_S parseVersion xs of
  [] -> Nothing
  (x:_) -> Just (fst x)

-- ----------------------------------------------------------------------------
-- Implicit cradle discovery logic mirroring the one used by HLS.
-- The code itself is copy-pasted from HLS.
-- Obviously, we want to reuse the logic without having to depend on HLS.
-- We should factor out a common sub-component from HLS and use it here as well.
-- ----------------------------------------------------------------------------

loadImplicitCradle :: Show a => LogAction IO (WithSeverity HIE.Log) -> FilePath -> IO (HIE.Cradle a)
loadImplicitCradle l wfile = do
  is_dir <- doesDirectoryExist wfile
  let wdir | is_dir = wfile
           | otherwise = takeDirectory wfile
  cfg <- runMaybeT (implicitConfig wdir)
  case cfg of
    Just bc -> HIE.getCradle l absurd bc
    Nothing -> return $ HIE.defaultCradle l wdir

-- | Wraps up the cradle inferred by @inferCradleTree@ as a @CradleConfig@ with no dependencies
implicitConfig :: FilePath -> MaybeT IO (Config.CradleConfig a, FilePath)
implicitConfig = (fmap . first) (Config.CradleConfig noDeps) . inferCradleTree
  where
  noDeps :: [FilePath]
  noDeps = []


inferCradleTree :: FilePath -> MaybeT IO (Config.CradleTree a, FilePath)
inferCradleTree start_dir =
       maybeItsBios
   -- If we have both a config file (cabal.project/stack.yaml) and a work dir
   -- (dist-newstyle/.stack-work), prefer that
   <|> (cabalExecutable >> cabalConfigDir start_dir >>= \dir -> cabalWorkDir dir >> pure (simpleCabalCradle dir))
   <|> (stackExecutable >> stackConfigDir start_dir >>= \dir -> stackWorkDir dir >> stackCradle dir)
   -- If we have a cabal.project OR we have a .cabal and dist-newstyle, prefer cabal
   <|> (cabalExecutable >> (cabalConfigDir start_dir <|> cabalFileAndWorkDir) <&> simpleCabalCradle)
   -- If we have a stack.yaml, use stack
   <|> (stackExecutable >> stackConfigDir start_dir >>= stackCradle)
   -- If we have a cabal file, use cabal
   <|> (cabalExecutable >> cabalFileDir start_dir <&> simpleCabalCradle)

  where
  maybeItsBios = (\wdir -> (Config.Bios (Config.Program $ wdir </> ".hie-bios") Nothing Nothing, wdir)) <$> biosWorkDir start_dir

  cabalFileAndWorkDir = cabalFileDir start_dir >>= (\dir -> cabalWorkDir dir >> pure dir)

-- | Generate a stack cradle given a filepath.
--
-- Since we assume there was proof that this file belongs to a stack cradle
-- we look immediately for the relevant @*.cabal@ and @stack.yaml@ files.
-- We do not look for package.yaml, as we assume the corresponding .cabal has
-- been generated already.
--
-- We parse the @stack.yaml@ to find relevant @*.cabal@ file locations, then
-- we parse the @*.cabal@ files to generate a mapping from @hs-source-dirs@ to
-- component names.
stackCradle :: FilePath -> MaybeT IO (Config.CradleTree a, FilePath)
stackCradle fp = do
  pkgs <- Implicit.stackYamlPkgs fp
  pkgsWithComps <- liftIO $ catMaybes <$> mapM (Implicit.nestedPkg fp) pkgs
  let yaml = fp </> "stack.yaml"
  pure $ (,fp) $ case pkgsWithComps of
    [] -> Config.Stack (Config.StackType Nothing (Just yaml))
    ps -> Config.StackMulti mempty $ do
      Implicit.Package n cs <- ps
      c <- cs
      let (prefix, comp) = Implicit.stackComponent n c
      pure (prefix, Config.StackType (Just comp) (Just yaml))

-- | By default, we generate a simple cabal cradle which is equivalent to the
-- following hie.yaml:
--
-- @
--   cradle:
--     cabal:
-- @
--
-- Note, this only works reliable for reasonably modern cabal versions >= 3.2.
simpleCabalCradle :: FilePath -> (Config.CradleTree a, FilePath)
simpleCabalCradle fp = (Config.Cabal $ Config.CabalType Nothing Nothing, fp)

cabalExecutable :: MaybeT IO FilePath
cabalExecutable = MaybeT $ findExecutable "cabal"

stackExecutable :: MaybeT IO FilePath
stackExecutable = MaybeT $ findExecutable "stack"

biosWorkDir :: FilePath -> MaybeT IO FilePath
biosWorkDir = findFileUpwards (".hie-bios" ==)

cabalWorkDir :: FilePath -> MaybeT IO ()
cabalWorkDir wdir = do
  check <- liftIO $ doesDirectoryExist (wdir </> "dist-newstyle")
  unless check $ fail "No dist-newstyle"

stackWorkDir :: FilePath -> MaybeT IO ()
stackWorkDir wdir = do
  check <- liftIO $ doesDirectoryExist (wdir </> ".stack-work")
  unless check $ fail "No .stack-work"

cabalConfigDir :: FilePath -> MaybeT IO FilePath
cabalConfigDir = findFileUpwards (\fp -> fp == "cabal.project" || fp == "cabal.project.local")

cabalFileDir :: FilePath -> MaybeT IO FilePath
cabalFileDir = findFileUpwards (\fp -> takeExtension fp == ".cabal")

stackConfigDir :: FilePath -> MaybeT IO FilePath
stackConfigDir = findFileUpwards isStack
  where
    isStack name = name == "stack.yaml"

-- | Searches upwards for the first directory containing a file to match
-- the predicate.
findFileUpwards :: (FilePath -> Bool) -> FilePath -> MaybeT IO FilePath
findFileUpwards p dir = do
  cnts <-
    liftIO
    $ handleJust
        -- Catch permission errors
        (\(e :: IOError) -> if isPermissionError e then Just [] else Nothing)
        pure
        (findFile p dir)

  case cnts of
    [] | dir' == dir -> fail "No cabal files"
            | otherwise   -> findFileUpwards p dir'
    _ : _ -> return dir
  where dir' = takeDirectory dir

-- | Sees if any file in the directory matches the predicate
findFile :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
findFile p dir = do
  b <- doesDirectoryExist dir
  if b then getFiles >>= filterM doesPredFileExist else return []
  where
    getFiles = filter p <$> getDirectoryContents dir
    doesPredFileExist file = doesFileExist $ dir </> file
