{-# LANGUAGE LambdaCase #-}
module GHC.Debugger.Run.Resume
  ( resolveResumeStep
  , resumeExec
  ) where

import GHC.Runtime.Eval hiding (resumeExec)
import GHC.Debugger.Monad
import GHC.Debugger.Interface.Messages
import GHC.Driver.Monad
import GHC.Driver.Env
import GHC.ByteCode.Breakpoints
import GHC.Runtime.Debugger.Breakpoints
import Data.Maybe
import GHC.Types.SrcLoc
import Data.Function
import qualified GHC.Runtime.Eval as GHC

-- | Construct the 'SingleStep' resume option for a paused thread from its
-- 'Resume' info and the type of step to do.
resolveResumeStep :: Resume -> ResumeStep -> Debugger SingleStep
resolveResumeStep r = \case
  ResumeNoStep     -> pure RunToCompletion
  ResumeSingleStep -> pure SingleStep
  ResumeStepLocal
    | Nothing <- rbid -- exception
    -> pure SingleStep
    | Just ibi <- rbid
    -> do
       spn <- enclosing_decl ibi
       pure LocalStep { breakAt = RealSrcSpan spn mempty }
  ResumeStepOut
    | Nothing <- rbid
    -> pure StepOut { initiatedFrom = Nothing }
    | Just ibi <- rbid
    -> do
       spn <- enclosing_decl ibi
       pure StepOut { initiatedFrom = Just (RealSrcSpan spn mempty) }
  where
    rbid = resumeBreakpointId r
    loc  = resumeSpan r
    -- To do a local step, we get the SrcSpan of the current suspension state
    -- and get its 'enclosingTickSpan' to use as a filter for breakpoints in
    -- the call to 'resumeExec'. Execution will only stop at breakpoints whose
    -- span matches this enclosing span.
    enclosing_decl ibi = do
      hug   <- hsc_HUG <$> getSession
      brks  <- readIModBreaks hug ibi & liftIO
      let md = getBreakSourceMod ibi brks
      ticks <- fromMaybe (error "doLocalStep:getTicks") <$> makeModuleLineMap md
      pure $ enclosingTickSpan ticks loc

-- | Wrap around GHC's "odd behavior"?
resumeExec :: GhcMonad m => SingleStep -> Maybe Int -> Resume -> m ExecResult
resumeExec a b resume = do
  -- IC's ic_imports field is not kept in sync with ic_gre_cache, so we could do
  -- this call later, but why rely on that.
  imports <- GHC.getContext

#if MIN_VERSION_ghc(10,1,0)
  v <- GHC.resumeExec a MultiThreadedBreaks b resume
#else
  v <- GHC.resumeExec a b
#endif

  -- To have interactive imports persist after a `continue` command we have to
  -- work around how GHC.resumeExec handles the InteractiveContext (IC).
  --
  -- GHC.resumeExec resets the scope of the IC (i.e. ic_gre_cache) to what it
  -- was before the last ExecBreak.
  --
  -- It makes sense for GHC.resumeExec to remove from the IC scope the
  -- breakpoint locals and anything that could have been defined with them, in
  -- fact they are also unloaded.
  --
  -- The way it's done though also rollbacks any import statements that were
  -- executed since the last ExecBreak. The only fix is to reimport everything
  -- again.
  --
  -- Note: GHC.setContext recomputes the scope of the interactive imports from
  -- scratch everytime. Considering `runDebugger` adds the whole home unit to
  -- the interactive imports this might become a bottleneck. GHC does not keep
  -- any reference to the scope containing just the imports, so we would have to
  -- cache it ourselves (and then extend it with the cached scope of
  -- ic_tythings, i.e. igre_prompt_env (c.f. replaceImportEnv)).
  GHC.setContext imports
  pure v
