{-# LANGUAGE CPP, NamedFieldPuns, TupleSections, LambdaCase,
   DuplicateRecordFields, RecordWildCards, TupleSections, ViewPatterns,
   TypeApplications, ScopedTypeVariables, BangPatterns, MultiWayIf #-}
module GHC.Debugger.Stopped where

import Control.Monad
import Control.Monad.Reader
import Data.IORef
import qualified Data.List as L

import GHC
import GHC.Types.Unique.FM
import GHC.Types.Name.Occurrence (sizeOccEnv)
import GHC.ByteCode.Breakpoints
import GHC.Types.Name.Reader
import GHC.Unit.Home.ModInfo
import GHC.Unit.Module.ModDetails
import GHC.Types.TypeEnv
import GHC.Data.Maybe
import GHC.Driver.Env as GHC
import GHC.Runtime.Eval
import GHC.Types.SrcLoc
import GHC.InfoProv
import GHC.Utils.Outputable as Ppr
import qualified GHC.Unit.Home.Graph as HUG

import GHC.Debugger.Stopped.Exception
import GHC.Debugger.Stopped.Variables
import GHC.Debugger.Runtime
import GHC.Debugger.Runtime.Thread
import GHC.Debugger.Runtime.Thread.Stack
import GHC.Debugger.Runtime.Thread.Map
import GHC.Debugger.Monad
import GHC.Debugger.Interface.Messages
import qualified GHC.Debugger.Interface.Messages as DbgStackFrame (DbgStackFrame(..))
import GHC.Debugger.Utils
import qualified GHC.Debugger.Logger as Logger
import qualified GHC.Stack.Types as Stack

{-
Note [Don't crash if not stopped]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Requests such as `stacktrace`, `scopes`, or `variables` may end up
coming after the execution of a program has terminated. For instance,
consider this interleaving:

1. SENT Stopped event         <-- we're stopped
2. RECEIVED StackTrace req    <-- client issues after stopped event
3. RECEIVED Next req          <-- user clicks step-next
4. <program execution resumes and fails>
5. SENT Terminate event       <-- execution failed and we report it to exit cleanly
6. RECEIVED Scopes req        <-- happens as a sequence of 2 that wasn't canceled
7. <used to crash! because we're no longer at a breakpoint>

Now, we simply returned empty responses when these requests come in
while we're no longer at a breakpoint. The client will soon come to a halt
because of the termination event we sent.
-}

--------------------------------------------------------------------------------
-- * Threads
--------------------------------------------------------------------------------

getThreads :: Debugger [DebuggeeThread]
getThreads = do
  -- TODO: we want something more like 'listThreads', but ensure that we only
  -- report the threads of the debuggee (and not the debugger, if they
  -- are the same process). Perhaps the solution is to not allow them to be in
  -- the same process, in which case 'listThreads' would be correct as is by
  -- construction.
  --
  -- For now, we approximate by just listing out the ThreadsMap, under the
  -- assumption the debugger client will only care about threads we've already
  -- stopped at (which are the only ones we've inserted in the threads map),
  -- but for full multi threaded debugging we need the listThreads.
  --
  -- tmap <- liftIO . readIORef =<< asks threadMap
  -- let (t_ids, remote_refs) = unzip (threadMapToList tmap)
  --
  -- Oh, try the listThreads just for fun.
  (t_ids, remote_refs) <- unzip <$> listAllLiveRemoteThreads
  t_labels             <- getRemoteThreadsLabels remote_refs
  let
    _mkDebuggeeThread tid tlbl
      = DebuggeeThread
        { tId = tid
        , tName = tlbl
        }
    _all_threads
      = zipWith _mkDebuggeeThread t_ids t_labels

  -- TODO: We ignore _all_threads and report only the main execution thread for now.
  -- See #138 for progress on Multi-threaded debugging.
  GHC.getResumeContext >>= \case
    [] ->
      -- See Note [Don't crash if not stopped]
      return []
    r:_ -> do
      r_tid <- getRemoteThreadIdFromRemoteContext (GHC.resumeContext r)
      return
        [ DebuggeeThread
          { tId = r_tid
          , tName = Just "Main Thread"
          }
        ]

--------------------------------------------------------------------------------
-- * Stack trace
--------------------------------------------------------------------------------

-- | Get the stack frames at the point we're stopped at
getStacktrace :: RemoteThreadId -> Debugger [DbgStackFrame]
getStacktrace req_tid = do

  tm <- liftIO . readIORef =<< asks threadMap
  let m_f_tid = lookupThreadMap (remoteThreadIntRef req_tid) tm

  hsc_env <- getSession
  let hug = hsc_HUG hsc_env
  decoded_frames <- catMaybes <$> case m_f_tid of
    Nothing -> pure []
    Just f_tid -> do
      -- Try decoding a stack with interpreter continuation frames (RetBCOs)
      -- and use the BRK_FUN src locations.
      ibis <- getRemoteThreadStackCopy f_tid
      forM ibis $ \case
        StackFrameBreakpointInfo ibi -> do
          info_brks <- liftIO $ readIModBreaks hug ibi
          let modl  = getBreakSourceMod ibi info_brks
          srcSpan   <- liftIO $ getBreakLoc (readIModModBreaks hug) ibi info_brks
          decl <- liftIO $ L.intercalate "." <$> getBreakDecls (readIModModBreaks hug) ibi info_brks

          modl_str  <- display modl
          return $ Just DbgStackFrame
            { name = modl_str ++ "." ++ decl
            , sourceSpan = realSrcSpanToSourceSpan $ realSrcSpan srcSpan
            , breakId = Just ibi
            }
        StackFrameIPEInfo ipe -> do
          case srcSpanStringToSourceSpan (ipLoc ipe) of
            Left err -> do
              -- Couldn't parse. The srcLoc may be invalid so just keep this as info, not warning.
              logSDoc Logger.Info $
                text "Couldn't parse StackEntry srcLoc \"" Ppr.<> text (ipLoc ipe)
                                                           Ppr.<> text "\":" <+> text err
              return Nothing
            Right sourceSpan ->
              return $ Just DbgStackFrame
                { name = ipMod ipe ++ "." ++ ipLabel ipe
                , sourceSpan = sourceSpan
                , breakId = Nothing
                }
        StackFrameAnnotation srcLoc ann -> do
            return $ Just DbgStackFrame
              { name = ann
              , sourceSpan = maybe unhelpfulSourceSpan srcLocToSourceSpan srcLoc
              , breakId = Nothing
              }

  -- Add the latest resume context at the head.
  head_frame <- GHC.getResumeContext >>= \case
    [] ->
      -- See Note [Don't crash if not stopped]
      return Nothing
    r:_ -> do
      let resumeSpanR = GHC.resumeSpan r
          mRealSpan   = realSrcSpanToSourceSpan <$> srcSpanToRealSrcSpan resumeSpanR
          firstSpan   = DbgStackFrame.sourceSpan <$> listToMaybe decoded_frames
      r_tid <- getRemoteThreadIdFromRemoteContext (GHC.resumeContext r)
      if r_tid /= req_tid then
        return Nothing
      else case GHC.resumeBreakpointId r of
        Just ibi
          | Just ss <- mRealSpan
          , Just ss /= firstSpan -> do
              -- We're getting the stacktrace for the thread we're stopped at.
              info_brks <- liftIO $ readIModBreaks hug ibi
              let modl  = getBreakSourceMod ibi info_brks
              modl_str  <- display modl
              return $
                Just DbgStackFrame
                  { name = modl_str ++ "." ++ GHC.resumeDecl r
                  , sourceSpan = ss
                  , breakId = Just ibi
                  }
        _ -> do
          mExcSpan <- exceptionSourceSpanFromContext
          case mExcSpan of
            Just sourceSpan ->  return $ Just DbgStackFrame
                                  { name = GHC.resumeDecl r
                                  , sourceSpan
                                  , breakId = Nothing
                                  }
            Nothing -> return Nothing
  return (maybe id (:) head_frame $ decoded_frames)

srcLocToSourceSpan :: Stack.SrcLoc -> SourceSpan
srcLocToSourceSpan srcLoc =
  SourceSpan
    { file = Stack.srcLocFile srcLoc
    , startLine = Stack.srcLocStartLine srcLoc
    , endLine = Stack.srcLocEndLine srcLoc
    , startCol = Stack.srcLocStartCol srcLoc
    , endCol = Stack.srcLocEndCol srcLoc
    }

--------------------------------------------------------------------------------
-- * Scopes
--------------------------------------------------------------------------------

-- | Get the stack frames at the point we're stopped at
getScopes :: RemoteThreadId -> Int -> Debugger [ScopeInfo]
getScopes threadId frameIx = do
  frames <- getStacktrace threadId
  let frame = frames !! frameIx
  let sourceSpan = DbgStackFrame.sourceSpan frame
      localsScope = ScopeInfo
        { kind = LocalVariablesScope
        , expensive = False
        , numVars = Nothing
        , sourceSpan
        }
  if
    | frameIx < length frames
    , Just ibi <- DbgStackFrame.breakId frame
    -> do
      hsc_env   <- getSession
      info_brks <- liftIO $ readIModBreaks (hsc_HUG hsc_env) ibi
      let brk_modl = getBreakSourceMod ibi info_brks
      -- It is /very important/ to report a number of variables (numVars) for
      -- larger scopes. If we just say "Nothing", then all variables of all
      -- scopes will be fetched at every stopped event.
      in_mod   <- getTopEnv brk_modl
      imported <- getTopImported brk_modl
      return
        [ localsScope
        , ScopeInfo { kind = ModuleVariablesScope
                    , expensive = True
                    , numVars = Just (sizeUFM in_mod)
                    , sourceSpan
                    }
        , ScopeInfo { kind = GlobalVariablesScope
                    , expensive = True
                    , numVars = Just (sizeOccEnv imported)
                    , sourceSpan
                    }
        ]
    | otherwise ->
      return [localsScope]

--------------------------------------------------------------------------------
-- * Variables
--------------------------------------------------------------------------------
-- Note [Variables Requests]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~
-- We can receive a Variables request for three different reasons
--
-- 1. To get the variables in a certain scope
-- 2. To inspect the value of a lazy variable
-- 3. To expand the structure of a variable
--
-- The replies are, respectively:
--
-- (VARR)
-- (a) All the variables in the request scope
-- (b) ONLY the variable requested
-- (c) The fields of the variable requested but NOT the original variable

-- | Get variables using a variable/variables reference
--
-- If the Variable Request ends up being case (VARR)(b), then we signal the
-- request forced the variable and return @Left varInfo@. Otherwise, @Right vis@.
--
-- See Note [Variables Requests]
getVariables :: RemoteThreadId -> Int{-stack frame index-} -> VariableReference -> Debugger (Either VarInfo [VarInfo])
getVariables threadId frameIx vk = do
  frames <- getStacktrace threadId
  let frame = frames !! frameIx
  hsc_env <- getSession
  case vk of
    -- Only `seq` the variable when inspecting a specific one (`SpecificVariable`)
    -- (VARR)(b,c)
    SpecificVariable key -> do
      term <- obtainTerm key

      case term of

        -- (VARR)(b)
        Suspension{} -> do

          -- Original Term was a suspension:
          -- It is a "lazy" DAP variable: our reply can ONLY include
          -- this single variable.

          term' <- forceTerm term

          vi <- termToVarInfo key term'

          return (Left vi)

        -- (VARR)(c)
        _ -> Right <$> do

          -- Original Term was already something other than a Suspension;
          -- Meaning the @SpecificVariable@ request means to inspect the structure.
          -- Return ONLY the fields

          termVarFields key term >>= \case
            VarFields vfs -> return vfs

    -- (VARR)(a) from here onwards

    LocalVariables -> fmap Right $ do
      -- bindLocalsAtBreakpoint hsc_env (GHC.resumeApStack r) (GHC.resumeSpan r) (GHC.resumeBreakpointId r)
      mapM tyThingToVarInfo =<< GHC.getBindings

    ModuleVariables
      | frameIx < length frames
      , Just ibi <- DbgStackFrame.breakId frame
      -> Right <$> do
        curr_modl <- liftIO $ getBreakSourceMod ibi <$>
                      readIModBreaks (hsc_HUG hsc_env) ibi
        things <- typeEnvElts <$> getTopEnv curr_modl
        mapM (\tt -> do
          nameStr <- display (getName tt)
          vi <- tyThingToVarInfo tt
          return vi{varName = nameStr}) things

    GlobalVariables
      | frameIx < length frames
      , Just ibi <- DbgStackFrame.breakId frame
      -> Right <$> do
        curr_modl <- liftIO $ getBreakSourceMod ibi <$>
                      readIModBreaks (hsc_HUG hsc_env) ibi
        names <- map greName . globalRdrEnvElts <$> getTopImported curr_modl
        mapM (\n-> do
          nameStr <- display n
          liftIO (GHC.lookupType hsc_env n) >>= \case
            Nothing ->
              return VarInfo
                { varName = nameStr
                , varType = ""
                , varValue = ""
                , isThunk = False
                , varRef = NoVariables
                }
            Just tt -> do
              vi <- tyThingToVarInfo tt
              return vi{varName = nameStr}
          ) names

    NoVariables -> Right <$> do
      return []

    -- Couldn't find ibi or frame
    _otherwise -> Right <$> do
      return []

--------------------------------------------------------------------------------
-- Inspect
--------------------------------------------------------------------------------

-- | All top-level things from a module, including unexported ones.
getTopEnv :: Module -> Debugger TypeEnv
getTopEnv modl = do
  hsc_env <- getSession
  liftIO $ HUG.lookupHugByModule modl (hsc_HUG hsc_env) >>= \case
    Nothing -> return emptyTypeEnv
    Just HomeModInfo
      { hm_details = ModDetails
        { md_types = things
        }
      } -> return things

-- | All bindings imported at a given module
getTopImported :: Module -> Debugger GlobalRdrEnv
getTopImported modl = do
  hsc_env <- getSession
  liftIO $ HUG.lookupHugByModule modl (hsc_HUG hsc_env) >>= \case
    Nothing -> return emptyGlobalRdrEnv
    Just hmi -> mkTopLevImportedEnv hsc_env hmi
