{-# LANGUAGE OverloadedStrings, OverloadedRecordDot, RecordWildCards, PatternSynonyms #-}
module Development.Debugger.Breakpoints where

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

-- | BreakpointLocations command
commandBreakpointLocations :: DebugAdaptor ()
commandBreakpointLocations = do
  BreakpointLocationsArguments{..} <- getArguments
  file <- fileFromSourcePath breakpointLocationsArgumentsSource

  DidGetBreakpoints mspan <-
    sendSync (GetBreakpointsAt (ModuleBreak file breakpointLocationsArgumentsLine breakpointLocationsArgumentsColumn))

  let locs = case mspan of
        Nothing -> []
        Just SourceSpan {..} ->
          [ BreakpointLocation
            { breakpointLocationLine = startLine
            , breakpointLocationColumn = Just startCol
            , breakpointLocationEndLine = Just endLine
            , breakpointLocationEndColumn = Just endCol
            }
          ]

  sendBreakpointLocationsResponse locs

-- | Execute adaptor command set module breakpoints
commandSetBreakpoints :: DebugAdaptor ()
commandSetBreakpoints = do
  SetBreakpointsArguments {..} <- getArguments
  file <- fileFromSourcePath setBreakpointsArgumentsSource
  let breaks_wanted = fromMaybe [] setBreakpointsArgumentsBreakpoints

  -- Clear existing module breakpoints
  DidClearBreakpoints <- sendSync (ClearModBreakpoints file)

  -- Set requested ones
  breaks <- forM breaks_wanted $ \bp -> do
    DidSetBreakpoint bf <-
      sendSync (SetBreakpoint (ModuleBreak file (DAP.sourceBreakpointLine bp) (DAP.sourceBreakpointColumn bp)))
    registerBreakFound bf

  sendSetBreakpointsResponse (concat breaks)

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

  sendSetFunctionBreakpointsResponse (concat breaks)

-- | Execute adaptor command set exception breakpoints
commandSetExceptionBreakpoints :: DebugAdaptor ()
commandSetExceptionBreakpoints = do
  SetExceptionBreakpointsArguments{..} <- getArguments

  -- Clear old exception breakpoints
  DidRemoveBreakpoint _ <- sendSync (DelBreakpoint OnExceptionsBreak)
  DidRemoveBreakpoint _ <- sendSync (DelBreakpoint OnUncaughtExceptionsBreak)

  let breakOnExceptions = BREAK_ON_EXCEPTION `elem` setExceptionBreakpointsArgumentsFilters
  let breakOnError      = BREAK_ON_ERROR `elem` setExceptionBreakpointsArgumentsFilters

  when breakOnExceptions $ do
    DidSetBreakpoint _ <- sendSync (SetBreakpoint OnExceptionsBreak)
    pure ()

  when breakOnError $ do
    DidSetBreakpoint _ <- sendSync (SetBreakpoint OnUncaughtExceptionsBreak)
    pure ()

  sendSetExceptionBreakpointsResponse
    [ defaultBreakpoint | True <- [breakOnError, breakOnExceptions] ]

--------------------------------------------------------------------------------
-- * Aux
--------------------------------------------------------------------------------

pattern BREAK_ON_EXCEPTION, BREAK_ON_ERROR :: T.Text
pattern BREAK_ON_EXCEPTION = "break-on-exception"
pattern BREAK_ON_ERROR = "break-on-error"

-- | Turn a ghc-debugger 'BreakFound' into a DAP 'Breakpoint'.
--
-- Additionally, gets a fresh Id for the breakpoint and registers it on the breakpoint map
registerBreakFound :: BreakFound -> DebugAdaptor [DAP.Breakpoint]
registerBreakFound b =
  case b of
    ManyBreaksFound bs -> concat <$> mapM registerBreakFound bs
    BreakNotFound -> pure [ DAP.defaultBreakpoint { DAP.breakpointVerified = False } ]
    BreakFoundNoLoc _ch -> pure [ DAP.defaultBreakpoint { DAP.breakpointVerified = True } ]
    BreakFound _ch iid ss -> do
      bid <- registerNewBreakpoint iid
      source <- fileToSource ss.file
      pure [ DAP.defaultBreakpoint
        { DAP.breakpointVerified = True
        , DAP.breakpointSource = Just source
        , DAP.breakpointLine = Just ss.startLine
        , DAP.breakpointEndLine = Just ss.endLine
        , DAP.breakpointColumn = Just ss.startCol
        , DAP.breakpointEndColumn = Just ss.endCol
        , DAP.breakpointId = Just bid
        } ]

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

-- | Get the file from a DAP Source
--
-- TODO: Handles sourceReferences too
fileFromSourcePath :: Source -> DebugAdaptor FilePath
fileFromSourcePath source = do
  let
    file = T.unpack $
            fromMaybe (error "sourceReference unsupported") $
              sourcePath source
  return file
