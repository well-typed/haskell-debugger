{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module GHC.Debugger.Session.Interactive where

import Control.Monad.IO.Class
import Data.Maybe

#if MIN_VERSION_ghc(9,15,0)
import GHC.Linker.Types (modifyHomePackageBytecodeState)
#endif
import GHC
import GHC.Driver.Config
import GHC.Driver.DynFlags as GHC
import GHC.Driver.Env
import GHC.Driver.Main
import qualified GHC.Linker.Loader as Loader
import GHC.Runtime.Interpreter as GHCi
import GHC.Runtime.Context as GHCi
import GHCi.RemoteTypes (HValueRef)

-- | Defines a strict @noPrintConstant :: a -> IO ()@ while taking care not to introduce its OccName into scope.
defineNoPrint :: Ghc Name
defineNoPrint = do
    let ExecOptions{execSourceFile,execLineNumber} = GHC.execOptions
    hsc_env <- getSession
    let input = "let noPrintConstant x = x `Prelude.seq` Prelude.return () :: Prelude.IO ()"
    stmt <-
      liftIO $ fmap (fromMaybe $ error "impossible: parsing noPrintConstant") $
      runInteractiveHsc hsc_env $
      hscParseStmtWithLocation execSourceFile execLineNumber input

    mr <- execStmtNoOccEnv stmt
    case mr of
      Just ExecComplete{execResult = Right [n]} -> return n
      _ -> error "impossible: defining noPrintConstant"

-- | Executes a statement to completion without bringing any OccName in scope.
execStmtNoOccEnv :: GhcMonad m => GhciLStmt GhcPs -> m (Maybe ExecResult)
execStmtNoOccEnv stmt = do
    hsc_env <- getSession
    let interp = hscInterp hsc_env

    -- Turn off -fwarn-unused-local-binds when running a statement, to hide
    -- warnings about the implicit bindings we introduce.
    let ic       = hsc_IC hsc_env -- use the interactive dflags
        idflags' = ic_dflags ic `wopt_unset` Opt_WarnUnusedLocalBinds
        hsc_env' = mkInteractiveHscEnv (hsc_env{ hsc_IC = ic{ ic_dflags = idflags' }})

    r <- liftIO $ hscParsedStmt hsc_env' stmt

    case r of
      Nothing ->
        -- empty statement / comment
        return (Just $ ExecComplete (Right []) 0)
      Just (ids, hval, _fix_env) -> do
        -- updateFixityEnv fix_env

        status <-
          liftIO $ do
            let eval_opts = initEvalOpts idflags' EvalStepNone
            evalStmt interp eval_opts (EvalThis hval)

        handleCompleted extendInteractiveContextWithIdsNoOccEnv ids status

handleCompleted :: GhcMonad m
                => (InteractiveContext -> [Id] -> InteractiveContext)
                -> [Id]
                -> EvalStatus_ [ForeignHValue] [HValueRef]
                -> m (Maybe ExecResult)
handleCompleted extendIC final_ids status = do
  hsc_env <- getSession
  let
    interp = hscInterp hsc_env
  case status of

    -- Completed successfully
    EvalComplete allocs (EvalSuccess hvals) -> do
      let
        final_ic = extendIC (hsc_IC hsc_env) final_ids
        final_names = map getName final_ids
#if MIN_VERSION_ghc(9,15,0)
      liftIO $ Loader.extendLoadedEnv interp modifyHomePackageBytecodeState (zip final_names hvals)
#else
      liftIO $ Loader.extendLoadedEnv interp (zip final_names hvals)
#endif
      -- hsc_env' <- liftIO $ rttiEnvironment hsc_env{hsc_IC=final_ic}
      setSession $ hsc_env{hsc_IC=final_ic}
      return (Just $ ExecComplete (Right final_names) allocs)
    _ -> return Nothing

-- | Extend the @InteractiveContext@ with the @Id@s without bringing their @OccName@s in scope.
extendInteractiveContextWithIdsNoOccEnv :: InteractiveContext -> [Id] -> InteractiveContext
extendInteractiveContextWithIdsNoOccEnv ictxt new_ids
  | null new_ids = ictxt
  | otherwise
  = ictxt { ic_mod_index  = ic_mod_index ictxt + 1
          , ic_tythings   = new_tythings ++ ic_tythings ictxt
          }
    -- no change to ic_gre_cache as that deals with OccNames
  where
    new_tythings = map AnId new_ids
