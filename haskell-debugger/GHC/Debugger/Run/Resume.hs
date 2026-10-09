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
resumeExec :: SingleStep -> Maybe Int -> Resume -> Debugger ExecResult
resumeExec a b _resume = do

#ifdef GHC_HAS_MULTITHREADED_DBG
  v <- GHC.resumeExec a MultiThreadedBreaks b _resume
#else
  v <- GHC.resumeExec a b
#endif

  pure v
