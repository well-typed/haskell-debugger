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
  -- | @cli [--entry-point=<entryPoint>] [--extra-ghc-args="<args>"] [<entryFile>] -- [<entryArgs>]@
  | HdbCLI
    { entryPoint :: String
    , entryFile :: FilePath
    , entryArgs :: [String]
    , extraGhcArgs :: [String]
    , verbosity :: Verbosity
    }

  -- | @proxy --port <port>@
  --
  -- The proxy command serves as a middle man between the user and the debugger.
  -- It is used implicitly by DAP mode: upon initialization, the debugger
  -- server asks the DAP client to @runInTerminal@ the @hdb proxy --port ...@
  -- command at a port determined by the debugger server.
  --
  -- The proxy mode will forward stdin to the debugger server and will be
  -- forwarded the debuggee's stdout. This essentially enables the user to
  -- observe and interact with the execution of the debugger, in a standalone
  -- terminal.
  --
  -- See #44 for the original ticket
  | HdbProxy
    { port :: Int
    , verbosity :: Verbosity
    }

