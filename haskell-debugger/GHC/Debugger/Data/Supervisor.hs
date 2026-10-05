{-# LANGUAGE LambdaCase #-}

-- | Scoped thread supervision.
--
-- * The thread that calls 'withSupervisor' is the parent. When the body
--   returns, throws, or the parent is killed, every supervised thread is
--   sent 'SupervisorShutdown', and the parent waits up to the grace
--   period for them to finish.
--
-- * Children spawned with 'Propagate' report their failure to the parent
--   as an asynchronous 'ChildFailed'. Every exception counts as a failure
--   except this supervisor's own 'SupervisorShutdown'. Nothing is
--   propagated once shutdown has started.
--
-- * 'registerForShutdown' makes an existing thread get killed on shutdown,
--   without the supervisor waiting for its cleanup or propagating its
--   failures.
--
-- Requires the threaded RTS.
module GHC.Debugger.Data.Supervisor
  ( Supervisor
  , OnFailure (..)
  , ChildFailed (..)
  , SupervisorShutdown (..)
  , withSupervisor
  , spawn
  , registerForShutdown
  , awaitAll
  , throwToParent
  ) where

import Control.Applicative ((<|>))
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.IORef
import qualified Data.Set as Set
import Data.Unique
import System.Mem.Weak (deRefWeak)

------------------------------------------------------------------------
-- Types

data OnFailure = Propagate | Ignore
  deriving (Eq, Show)

data Supervisor = Supervisor
  { supId    :: Unique
  , parent   :: ThreadId
  , stopping :: TVar Bool
    -- ^ shutdown initiated
  , workers  :: TVar (Set.Set ThreadId)
    -- ^ spawned children still alive
  , inFlight :: TVar Int
    -- ^ children currently delivering 'ChildFailed'
  }

-- | Thrown to the parent when a 'Propagate' child fails.
data ChildFailed = ChildFailed Unique ThreadId SomeException

instance Show ChildFailed where
  show (ChildFailed _ t e) = "ChildFailed " ++ show t ++ " " ++ show e

instance Exception ChildFailed where
  toException   = asyncExceptionToException
  fromException = asyncExceptionFromException
  displayException (ChildFailed _ t e) =
    "supervised thread " ++ show t ++ " failed: " ++ displayException e

-- | Thrown to supervised threads on shutdown. Never propagated by the
-- supervisor that sent it.
newtype SupervisorShutdown = SupervisorShutdown Unique

instance Show SupervisorShutdown where
  show _ = "SupervisorShutdown"

instance Exception SupervisorShutdown where
  toException   = asyncExceptionToException
  fromException = asyncExceptionFromException
  displayException _ = "supervisor shutdown"

------------------------------------------------------------------------
-- Lifecycle

-- | Run the body with a fresh supervisor whose parent is the calling
-- thread. The grace period is in microseconds.
withSupervisor :: Maybe Int -> (Supervisor -> IO a) -> IO a
withSupervisor graceMicros =
  bracket newSupervisor (`shutdown` graceMicros)

newSupervisor :: IO Supervisor
newSupervisor =
  Supervisor <$> newUnique <*> myThreadId <*> newTVarIO False
             <*> newTVarIO Set.empty <*> newTVarIO 0

-- | Not exported: runs masked from withSupervisor's bracket.
shutdown :: Supervisor -> Maybe Int -> IO ()
shutdown sup graceMicros = do
  -- uninterruptible: no retry
  tids <- atomically $ do
    writeTVar (stopping sup) True
    Set.toList <$> readTVar (workers sup)
  forM_ tids $ \t -> forkIO (throwTo t (SupervisorShutdown (supId sup)))
  foreignFailure <- newIORef Nothing
  waitDoneWorkers <- mkWaitDone sup graceMicros
  let
      waitDone = do
        -- in-flight propagations always complete promptly, so no deadline
        readTVar (inFlight sup) >>= check . (== 0)
        waitDoneWorkers
      -- interruptible: only resilient to ChildFailed and SupervisorShutdown exceptions
      loop = atomically waitDone `catches`
       [ Handler $ \cf@(ChildFailed u _ _) -> do
         when (u /= supId sup) $
           modifyIORef' foreignFailure (<|> Just (toException cf))
         loop
       , Handler $ \sd@(SupervisorShutdown u) -> do
         when (u /= supId sup) $ -- should never be equal
           modifyIORef' foreignFailure (<|> Just (toException sd))
         loop
       ]
  loop
  readIORef foreignFailure >>= mapM_ throwIO

mkWaitDone :: Supervisor -> Maybe Int -> IO (STM ())
mkWaitDone sup mgraceMicros = do
  withinDeadline <- mkDeadlineWrapper mgraceMicros
  pure $ do
        withinDeadline (readTVar (workers sup) >>= check . Set.null)

mkDeadlineWrapper :: Maybe Int -> IO (STM () -> STM ())
mkDeadlineWrapper Nothing = pure id
mkDeadlineWrapper (Just graceMicros) = do
  deadline <- registerDelay graceMicros
  pure $ (`orElse` (readTVar deadline >>= check))

throwToParent :: Exception e => Supervisor -> e -> IO ()
throwToParent sup e = throwTo (parent sup) e
------------------------------------------------------------------------
-- Children

spawn :: Supervisor -> OnFailure -> IO () -> IO ThreadId
spawn sup onFail action = mask_ $ do
 tid <- forkIOWithUnmask $ \unmask -> do
   tid <- myThreadId
   registered <- atomically $ do
     s <- readTVar (stopping sup)
     unless s $ modifyTVar' (workers sup) (Set.insert tid)
     pure (not s)
   when registered $
     (do r <- try (unmask action)
         case r of
           Left e | onFail == Propagate, not (isOwnShutdown sup e) ->
             propagate sup tid e
           _ -> pure ())
       `finally` atomically (modifyTVar' (workers sup) (Set.delete tid))
 atomically $ do
   s <- readTVar (stopping sup)
   when (not s) $ readTVar (workers sup) >>= check . Set.member tid
 pure tid


-- | Runs the given action after spawning a watcher thread in the given Supervisor.
--
-- The watcher redirects the SupervisorShutdown exception to the current thread,
-- then waits for it to exit this call.
--
-- If the action returns before a supervisor shutdown then the watcher is killed instead.
registerForShutdown :: Supervisor -> IO () -> IO ()
registerForShutdown sup m = do
  tid <- myThreadId
  when (tid == parent sup) $
    throwIO (userError "registerForShutdown: the parent cannot register")
  m_done <- newEmptyMVar
  flip finally (putMVar m_done ()) $ do
    wtid  <- mkWeakThreadId tid
    -- Nothing: undecided; Just True: watcher armed; Just False: disarmed
    armed <- newTVarIO Nothing
    watcher <- spawn sup Ignore $
      (do ok <- atomically $ readTVar armed >>= \case
            Nothing -> writeTVar armed (Just True) >> pure True
            Just _  -> pure False
          when ok $ forever (threadDelay maxBound))
        `finallyNoMask` do
          -- no mask so `m` wins the killThread race
          a <- readTVarIO armed
          when (a == Just True) $ do
            deRefWeak wtid >>= mapM_ (`throwTo` SupervisorShutdown (supId sup)) >> takeMVar m_done
    gaveUp <- atomically $ readTVar armed >>= \case
      Just _  -> pure False
      Nothing -> do
        readTVar (stopping sup) >>= check
        writeTVar armed (Just False)
        pure True
    when gaveUp $ throwIO (SupervisorShutdown (supId sup))
    m `finally` (atomically (writeTVar armed (Just False)) >> killThread watcher)
  where
    finallyNoMask (x :: IO ()) f = do
      x `onException` f
      f

-- | Waits for all threads spawned by this supervisor to terminate.
awaitAll :: Supervisor -> Maybe Int -> IO ()
awaitAll sup graceMicros = join $ atomically <$> mkWaitDone sup graceMicros

------------------------------------------------------------------------
-- Internals

-- Must be called masked. The check of 'stopping' and the 'inFlight'
-- increment are atomic, so shutdown always waits for this delivery.
propagate :: Supervisor -> ThreadId -> SomeException -> IO ()
propagate sup tid e = do
  go <- atomically $ do
    s <- readTVar (stopping sup)
    unless s $ modifyTVar' (inFlight sup) (+ 1)
    pure (not s)
  when go $
    throwTo (parent sup) (ChildFailed (supId sup) tid e)
      `finally` atomically (modifyTVar' (inFlight sup) (subtract 1))

isOwnShutdown :: Supervisor -> SomeException -> Bool
isOwnShutdown sup e = case fromException e of
  Just (SupervisorShutdown u) -> u == supId sup
  Nothing                     -> False
