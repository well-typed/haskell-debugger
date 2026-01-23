-- | Options parser using optparse-applicative for the debugger options in
-- 'Development.Debug.Options'
module Development.Debug.Options.Parser
  ( -- * Command line options parsing
    parseHdbOptions
  ) where

import Options.Applicative hiding (command)

import Data.Version
import qualified Options.Applicative
import qualified Paths_haskell_debugger as P

import Colog.Core
import Development.Debug.Options

--------------------------------------------------------------------------------
-- Options parser
--------------------------------------------------------------------------------

-- | Parser for 'HdbDAPServer' options
serverParser :: Parser HdbOptions
serverParser = HdbDAPServer
  <$> option auto
      ( long "port"
     <> short 'p'
     <> metavar "PORT"
     <> help "DAP server port" )
  <*> verbosityParser Debug

-- | Parser for 'HdbCLI' options
cliParser :: Parser HdbOptions
cliParser = HdbCLI
  <$> strOption
      ( long "entry-point"
     <> short 'e'
     <> metavar "ENTRY_POINT"
     <> value "main"
     <> help "The name of the function that is called to start execution (default: main)" )
  <*> argument str
      ( metavar "ENTRY_FILE"
     <> help "The relative path from the project root to the file with the entry point for execution" )
  <*> many
        ( argument str
          ( metavar "ENTRY_ARGS..."
         <> help "The arguments passed to the entryPoint. If the entryPoint is main, these arguments are passed as environment arguments (as in getArgs) rather than direct function arguments."
          )
        )
  <*> option (words <$> str)
      ( long "extra-ghc-args"
     <> metavar "GHC_ARGS"
     <> value []
     <> help "Additional flags to pass to the ghc invocation that loads the program for debugging" )
  <*> verbosityParser Warning

-- | Parser for 'HdbProxy' options
proxyParser :: Parser HdbOptions
proxyParser = HdbProxy
  <$> option auto
      ( long "port"
     <> short 'p'
     <> metavar "PORT"
     <> help "proxy port to which the debugger connects" )
  <*> verbosityParser Warning

-- | Parser for @hdb external-interpreter <write-fd> <read-fd>@
-- See Note [Custom external interpreter]
extInterpParser :: Parser HdbOptions
extInterpParser = HdbExternalInterpreter
  <$> argument auto
    ( metavar "WRITE_FD"
   <> help "external interpreter write file descriptor" )
  <*> argument auto
    ( metavar "READ_FD"
   <> help "external interpreter read file descriptor" )

-- | Combined parser for HdbOptions
hdbOptionsParser :: Parser HdbOptions
hdbOptionsParser = hsubparser
  ( Options.Applicative.command "server"
    ( info serverParser
      ( progDesc "Start the Haskell debugger in DAP server mode" ) )
 <> Options.Applicative.command "cli"
    ( info cliParser
      ( progDesc "Debug a Haskell program in CLI mode" ) )
 <> Options.Applicative.command "proxy"
    ( info proxyParser
      ( progDesc "Internal mode used by the DAP server to proxy the stdin/stdout to the DAP client's terminal" ) )
 <> Options.Applicative.command "external-interpreter"
    ( info extInterpParser
      ( progDesc "Start the custom-for-debugger external interpreter" ) )
  )
  <|> cliParser -- Default to CLI mode if no subcommand

-- | Parser for --version flag
versioner :: Parser (a -> a)
versioner = simpleVersioner $ "Haskell Debugger, version " ++ showVersion P.version

-- | Parser for --verbosity 0
--
-- The default verbosity differs by mode (#86):
-- - DAP server mode: DEBUG
-- - CLI mode: WARNING
verbosityParser :: Severity -> Parser Severity
verbosityParser vdef = option verb
    ( long "verbosity"
   <> short 'v'
   <> metavar "VERBOSITY"
   <> value vdef
   <> help "Logger verbosity in [0..3] interval, where 0 is silent and 3 is debug"
    )
  where
    verb = verbNum =<< auto
    verbNum n = case n :: Int of
      0 -> pure Error
      1 -> pure Warning
      2 -> pure Info
      3 -> pure Debug
      _ -> readerAbort (ErrorMsg "Verbosity must be a value in [0..3]")

-- | Main parser info
hdbParserInfo :: ParserInfo HdbOptions
hdbParserInfo = info (hdbOptionsParser <**> versioner <**> helper)
  ( fullDesc
 <> header "Haskell debugger supporting both CLI and DAP modes" )

-- | Parse command line arguments
parseHdbOptions :: IO HdbOptions
parseHdbOptions = customExecParser
  defaultPrefs{prefShowHelpOnError = True, prefShowHelpOnEmpty = True}
  hdbParserInfo
