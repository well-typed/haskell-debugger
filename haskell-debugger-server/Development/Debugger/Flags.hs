{-# LANGUAGE CPP, RecordWildCards, LambdaCase #-}
module Development.Debugger.Flags where

import Data.Maybe
import qualified Data.List as L

import qualified HIE.Bios as HIE
import qualified HIE.Bios.Types as HIE
import qualified HIE.Bios.Environment as HIE

-- | Flags inferred by @hie-bios@ to invoke GHC
data HieBiosFlags = HieBiosFlags
      { ghcInvocation :: [String]
      , libdir :: Maybe FilePath
      , units :: [String]
      }

-- | Make 'HieBiosFlags' from the given target file
hieBiosFlags :: FilePath -> IO HieBiosFlags
hieBiosFlags target = do

  explicitCradle <- HIE.findCradle target
  cradle <- maybe (HIE.loadImplicitCradle mempty target)
                  (HIE.loadCradle mempty) explicitCradle

  libdir <- (HIE.getRuntimeGhcLibDir cradle) >>= unwrapCradleResult "Failed to get runtime GHC libdir"

  -- getCompilerOptions depends on CWD being the proper root dir.
  -- TODO: What is the correct CWD for this process launched by the client?
  let compilerOpts = -- D.withCurrentDirectory cwd $
#if MIN_VERSION_hie_bios(0,14,0)
                          HIE.getCompilerOptions target HIE.LoadFile cradle
#else
                          HIE.getCompilerOptions target [] cradle
#endif
  HIE.ComponentOptions {HIE.componentOptions = flags} <- compilerOpts >>= unwrapCradleResult "Failed to get compiler options using hie-bios cradle"

#if __GLASGOW_HASKELL__ >= 913
  -- fwrite-if-simplified-core requires a recent bug fix regarding GHCi loading
  -- ROMES:TODO: Re-enable as soon as I'm using Matthew's patch.
  -- ["-fwrite-if-simplified-core"] ++
#endif

  return HieBiosFlags
    { ghcInvocation = -- [ target | not $ any (`L.isSuffixOf` target) flags ] ++ -- TODO is this correct?
                      flags ++ ghcDebuggerFlags
    , libdir = Just libdir
    , units  = mapMaybe (\case ("-unit", u) -> Just u; _ -> Nothing) $ zip flags (drop 1 flags)
    }
  where
    unwrapCradleResult m = \case
      HIE.CradleNone     -> error $ "HIE.CradleNone\n" ++ m
      HIE.CradleFail err -> error $ (unlines $ HIE.cradleErrorStderr err) ++ "\n" ++ m
      HIE.CradleSuccess x -> return x

-- | Flags specific to ghc-debugger to append to all GHC invocations.
ghcDebuggerFlags :: [String]
ghcDebuggerFlags =
  [ "-fno-it" -- don't introduce @it@ after evaluating something at the prompt
  ]
