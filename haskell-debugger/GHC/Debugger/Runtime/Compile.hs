-- | A interface to compiling and loading expressions/values/variables into the
-- debuggee runtime.
--
-- You should prefer this interface to the GHC's 'compileExprRemote' whenever
-- the loaded expression would benefit from being cached and re-used, to avoid
-- recompilation and loading.
--
-- Even better, you should **always prefer** to use the 'RemoteExpr' abstraction
-- for constructing and evaluating expressions on the remote debuggee process.
-- In it, there is finer grained caching and a good interface for constructing
-- (possibly complex) expressions.
module GHC.Debugger.Runtime.Compile
  ( compileVar
  , compileRaw
  )
  where

import Control.Monad.Reader
import Data.IORef
import GHC

import GHC.Debugger.Monad
import GHC.Debugger.Runtime.Compile.Cache

-- | Compile and load a fully qualified external variable, potentially applied
-- to a few type arguments (i.e. by specializing it). E.g. @"GHC.Base" "pure" ["IO"]
--
-- The loaded (specialized) expression is cached.
compileVar :: ModuleName -> String -> [String] -> Debugger ForeignHValue
compileVar mod_name var_name ty_args =
  let raw_expr =
        ("(" ++ moduleNameString mod_name
             ++ "." ++ var_name ++ ") "
             ++ unwords (map ('@':) ty_args))
   in compileRaw raw_expr

-- | Compile and load a raw expression string.
--
-- Note: it is easy to wrongly cache arbitrary compilation strings. You
-- shouldn't use 'copmileRaw' lightly since these arbitrary strings typically
-- are not very cacheable (there won't be many if any hits).
--
-- If you need an expression more complex than a single variable specialized at
-- a few types, or even then, you should just prefer to use @'RemoteExpr'@ to
-- describe and load that remote expression. 'RemoteExpr' will cache the
-- relevant bits (loading of external names) at the right granularity while not
-- caching, say, chains of function applications, which can be trivially
-- executed on the remote process in a single go without any compilation.
--
-- User beware!
compileRaw :: String -> Debugger ForeignHValue
compileRaw raw_expr = do
  compCacheRef <- asks compCache
  compCacheVal <- liftIO $ readIORef compCacheRef

  case lookupCompRaw raw_expr compCacheVal of
    Just fhv -> do
      -- logSDoc Logger.Debug (text "Cache hit! on" <+> text raw_expr)
      return fhv
    Nothing  -> do
      fhv <- compileExprRemote raw_expr
      liftIO $ modifyIORef' compCacheRef (insertCompRaw raw_expr fhv)
      return fhv
