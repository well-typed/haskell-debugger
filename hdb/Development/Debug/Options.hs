-- | Options supported by the debugger
--
-- For parsing see 'Development.Debug.Options.Parser'
module Development.Debug.Options
  ( HdbOptions(..) ) where

import Colog.Core (Severity)

-- | The options `hdb` is invoked in the command line with
data HdbOptions
  -- | @server [--internal-interpreter] --port <port>@
  = HdbDAPServer
    { port :: Int
    , verbosity :: Severity
    , internalInterpreter :: Bool
    }
  -- | @cli [--internal-interpreter] [--entry-point=<entryPoint>] [--extra-ghc-args="<args>"] [<entryFile>] -- [<entryArgs>]@
  | HdbCLI
    { entryPoint :: String
    , entryFile :: FilePath
    , entryArgs :: [String]
    , extraGhcArgs :: [String]
    , verbosity :: Severity
    , internalInterpreter :: Bool
    , debuggeeStdin :: Maybe FilePath
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
    , verbosity :: Severity
    }

  -- | Launch the custom-for-the-debugger external interpreter for running the
  -- debuggee process.
  --
  -- Using an external interpreter will guarantee the debugger and debuggee run
  -- on separate processes. This comes with many benefits:
  --  * Debugger vs debuggee threads naturally separated
  --  * Debugger vs debuggee stdin/stdout/stderr naturally separated
  --  * No *** Ignoring breakpoint when debugging the debugger against another program
  --
  --  A custom server is necessary for the custom commands (starting with GHC
  --  9.16) and because the external interpreter in GHC 9.14 is not compiled
  --  with -threaded, which is a requirement for using thread/stack cloning
  --  messages in the external process.
  --
  --  See #169
  | HdbExternalInterpreter
      { writeFd :: Int
      , readFd  :: Int
      }

