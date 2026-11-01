-- Prints the debuggee's working directory and environment, to test the
-- "cwd"/"env" launch options and the --cwd/--env CLI flags.
import Data.List (isPrefixOf)
import System.Directory (getCurrentDirectory, doesFileExist)
import System.Environment (lookupEnv)
import System.FilePath (takeFileName)

main :: IO ()
main = do
  cwd <- getCurrentDirectory
  putStrLn ("cwd: " ++ takeFileName cwd)
  marker <- doesFileExist "marker.txt"
  putStrLn ("marker: " ++ show marker)
  var <- lookupEnv "HDB_TEST_VAR"
  putStrLn ("HDB_TEST_VAR: " ++ show var)
  path <- lookupEnv "PATH"
  putStrLn ("PATH stub first: " ++ show (maybe False ("/hdb-stub:" `isPrefixOf`) path))
  putStrLn "done"
