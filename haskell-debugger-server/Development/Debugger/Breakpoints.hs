{-# LANGUAGE OverloadedStrings, RecordWildCards, PatternSynonyms #-}
module Development.Debugger.Breakpoints where

import qualified Data.ByteString.Lazy.Char8 as BL8 ( pack )
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.IntSet as IS
import Control.Monad
import Data.Maybe

import qualified GHC

import DAP

import Debugger.Interface.Messages hiding (Command, Response)

import Development.Debugger.Adaptor
import Development.Debugger.Interface

-- | Execute adaptor command set module breakpoints
commandSetBreakpoints :: DebugAdaptor ()
commandSetBreakpoints = do
  SetBreakpointsArguments {..} <- getArguments
  let
    file = T.unpack $
            fromMaybe (error "sourceReference unsupported") $
              sourcePath setBreakpointsArgumentsSource
    breaks_wanted = fromMaybe [] setBreakpointsArgumentsBreakpoints

  -- Clear existing module breakpoints
  DidClearBreakpoints <- sendSync (ClearModBreakpoints file)

  -- Set requested ones
  breaks <- forM breaks_wanted $ \bp -> do
    DidSetBreakpoint bf <-
      sendSync (SetBreakpoint (ModuleBreak file (DAP.sourceBreakpointLine bp) (DAP.sourceBreakpointColumn bp)))
    registerBreakFound bf

  sendSetBreakpointsResponse breaks

-- | Execute adaptor command set function breakpoints
commandSetFunctionBreakpoints :: DebugAdaptor ()
commandSetFunctionBreakpoints = do
  SetFunctionBreakpointsArguments{..} <- getArguments
  let
    breaks_wanted = mapMaybe functionBreakpointName setFunctionBreakpointsArgumentsBreakpoints

  -- Clear existing function breakpoints
  DidClearBreakpoints <- sendSync ClearFunctionBreakpoints

  -- Set requested ones
  breaks <- forM breaks_wanted $ \bp -> do
    DidSetBreakpoint bf <-
      sendSync (SetBreakpoint (FunctionBreak (T.unpack bp)))
    registerBreakFound bf

  sendSetFunctionBreakpointsResponse breaks

-- | Execute adaptor command set exception breakpoints
commandSetExceptionBreakpoints :: DebugAdaptor ()
commandSetExceptionBreakpoints = do
  s@SetExceptionBreakpointsArguments{..} <- getArguments
  -- ROMES:TODO:Implement exceptions breakpoints (see how I did it in HDA)
  logInfo $ BL8.pack $ "Set Exceptions: " ++ show s
  sendSetExceptionBreakpointsResponse []

--------------------------------------------------------------------------------
-- * Aux
--------------------------------------------------------------------------------

pattern BREAK_ON_EXCEPTION, BREAK_ON_ERROR :: T.Text
pattern BREAK_ON_EXCEPTION = "break-on-exception"
pattern BREAK_ON_ERROR = "break-on-error"

-- | Turn a ghc-debugger 'BreakFound' into a DAP 'Breakpoint'.
--
-- Additionally, gets a fresh Id for the breakpoint and registers it on the breakpoint map
registerBreakFound :: BreakFound -> DebugAdaptor DAP.Breakpoint
registerBreakFound b =
  case b of
    BreakFoundNoLoc _ch ->
      -- exception breakpoint
      pure DAP.defaultBreakpoint {
        DAP.breakpointVerified = True
      }
    BreakFound _ch sl el sc ec iid -> do
      bid <- registerNewBreakpoint iid
      pure DAP.defaultBreakpoint
        { DAP.breakpointVerified = True
        , DAP.breakpointLine = Just sl
        , DAP.breakpointEndLine = Just el
        , DAP.breakpointColumn = Just sc
        , DAP.breakpointEndColumn = Just ec
        , DAP.breakpointId = Just bid
        }

-- | Adds new BreakpointId for a givent StgPoint
registerNewBreakpoint :: GHC.BreakpointId -> DebugAdaptor BreakpointId
registerNewBreakpoint breakpoint = do
  bkpId <- getFreshBreakpointId
  updateDebugSession $ \das@DAS{..} -> das {breakpointMap = Map.insertWith mappend breakpoint (IS.singleton bkpId) breakpointMap}
  pure bkpId

-- | Generate fresh breakpoint Id.
getFreshBreakpointId :: DebugAdaptor BreakpointId
getFreshBreakpointId = do
  bkpId <- nextFreshBreakpointId <$> getDebugSession
  updateDebugSession $ \s -> s { nextFreshBreakpointId = nextFreshBreakpointId s + 1 }
  pure bkpId
