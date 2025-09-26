-- | Options supported by the debugger
--
-- For parsing see 'Development.Debug.Options.Parser'
module Development.Debug.Options
  ( HdbOptions(..) ) where

import GHC.Debugger.Logger

-- | The options `hdb` is invoked in the command line with
data HdbOptions
  -- | @server --port <port>@
  = HdbDAPServer
    { port :: Int
    , verbosity :: Verbosity
    }
  -- | @[--entry-point=<entryPoint>] [--extra-ghc-args="<args>"] [<entryFile>] -- [<entryArgs>]@
  | HdbCLI
    { entryPoint :: String
    , entryFile :: FilePath
    , entryArgs :: [String]
    , extraGhcArgs :: [String]
    , verbosity :: Verbosity
    }

