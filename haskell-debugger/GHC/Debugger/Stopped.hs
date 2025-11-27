{-# LANGUAGE CPP, NamedFieldPuns, TupleSections, LambdaCase,
   DuplicateRecordFields, RecordWildCards, TupleSections, ViewPatterns,
   TypeApplications, ScopedTypeVariables, BangPatterns #-}
module GHC.Debugger.Stopped where

import Control.Monad.Reader
import Data.IORef

import GHC
import GHC.Types.Unique.FM
import GHC.Types.Name.Occurrence (sizeOccEnv)
import GHC.ByteCode.Breakpoints
import GHC.Types.Name.Reader
import GHC.Unit.Home.ModInfo
import GHC.Unit.Module.ModDetails
import GHC.Types.TypeEnv
import GHC.Data.Maybe (expectJust)
import GHC.Driver.Env as GHC
import GHC.Runtime.Debugger.Breakpoints as GHC
import GHC.Runtime.Eval
import GHC.Types.SrcLoc
import qualified GHC.Unit.Home.Graph as HUG

import GHC.Debugger.Stopped.Variables
import GHC.Debugger.Runtime
import GHC.Debugger.Runtime.Thread
import GHC.Debugger.Runtime.Thread.Map
import GHC.Debugger.Monad
import GHC.Debugger.Interface.Messages
import GHC.Debugger.Utils

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
  t_labels <- getRemoteThreadsLabels remote_refs
  return $ zipWith
    (\tid tlbl ->
      DebuggeeThread
        { tId = tid
        , tName = tlbl
        }
    ) t_ids t_labels

--------------------------------------------------------------------------------
-- * Stack trace
--------------------------------------------------------------------------------

-- | Get the stack frames at the point we're stopped at
getStacktrace :: RemoteThreadId -> Debugger [StackFrame]
getStacktrace tODO_USE_ME = GHC.getResumeContext >>= \case
  [] ->
    -- See Note [Don't crash if not stopped]
    return []
  r:_
    | Just ss <- srcSpanToRealSrcSpan (GHC.resumeSpan r)
    -> do
      -- debug things:
      tm <- liftIO . readIORef =<< asks threadMap
      case lookupThreadMap (remoteThreadIntRef tODO_USE_ME) tm of
        Nothing -> pure ()
        Just f_tid -> do
          x <- getRemoteThreadStackCopy f_tid
          pprTraceM "WHT" (ppr x)
      return
        [ StackFrame
          { name = GHC.resumeDecl r
          , sourceSpan = realSrcSpanToSourceSpan ss
          }
        ]
    | otherwise ->
        -- No resume span; which should mean we're stopped on an exception.
        -- No info for now.
        return []

--------------------------------------------------------------------------------
-- * Scopes
--------------------------------------------------------------------------------

-- | Get the stack frames at the point we're stopped at
getScopes :: Debugger [ScopeInfo]
getScopes = GHC.getCurrentBreakSpan >>= \case
  Nothing ->
    -- See Note [Don't crash if not stopped]
    return []
  Just span'
    | Just rss <- srcSpanToRealSrcSpan span'
    , let sourceSpan = realSrcSpanToSourceSpan rss
    -> do
      -- It is /very important/ to report a number of variables (numVars) for
      -- larger scopes. If we just say "Nothing", then all variables of all
      -- scopes will be fetched at every stopped event.
      curr_modl <- expectJust <$> getCurrentBreakModule
      in_mod <- getTopEnv curr_modl
      imported <- getTopImported curr_modl
      return
        [ ScopeInfo { kind = LocalVariablesScope
                    , expensive = False
                    , numVars = Nothing
                    , sourceSpan
                    }
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
      -- No resume span; which should mean we're stopped on an exception
      -- TODO: Use exception context to create source span, or at least
      -- return the source span null to have Scopes at least.
      return []

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
getVariables :: VariableReference -> Debugger (Either VarInfo [VarInfo])
getVariables vk = do
  hsc_env <- getSession
  GHC.getResumeContext >>= \case
    [] ->
      -- See Note [Don't crash if not stopped]
      return (Right [])
    r:_ -> case vk of

      -- Only `seq` the variable when inspecting a specific one (`SpecificVariable`)
      -- (VARR)(b,c)
      SpecificVariable i -> do
        lookupVarByReference i >>= \case
          Nothing -> do
            -- lookupVarByReference failed.
            -- This may happen if, in a race, we change scope while asking for
            -- variables of the previous scope.
            -- Somewhat similar to the race in Note [Don't crash if not stopped]
            return (Right [])
          Just key -> do
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

      ModuleVariables -> Right <$> do
        case GHC.resumeBreakpointId r of
          Nothing -> return []
          Just ibi -> do
            curr_modl <- liftIO $ getBreakSourceMod ibi <$>
                          readIModBreaks (hsc_HUG hsc_env) ibi
            things <- typeEnvElts <$> getTopEnv curr_modl
            mapM (\tt -> do
              nameStr <- display (getName tt)
              vi <- tyThingToVarInfo tt
              return vi{varName = nameStr}) things

      GlobalVariables -> Right <$> do
        case GHC.resumeBreakpointId r of
          Nothing -> return []
          Just ibi -> do
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
