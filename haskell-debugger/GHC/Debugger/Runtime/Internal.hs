-- | A module providing various remote external variables which we want
-- available in any session. It gets compiled in-memory at the start of one.
--
-- See Note [debuggerInternal unit].
{-# LANGUAGE LambdaCase #-}
module GHC.Debugger.Runtime.Internal
  ( module GHC.Debugger.Runtime.Internal
  , GHC.evalWrapper
  , Prelude.concat
  , Prelude.putStrLn
  )
  where

import GHC.GHCi.Helpers qualified as GHC
import GHCi.RemoteTypes
import qualified Unsafe.Coerce
import GHC.Exts.Heap.Closures
import Data.Maybe
import Data.List
import GHC.Base (returnIO)
import qualified System.IO

-- | Some extensions can mess with [] and (:) syntax, so we setup these plain
-- function aliases.
nil :: [a]
nil = []

-- | See @nil@.
cons :: a -> [a] -> [a]
cons = (:)


-- Need to be careful not to create extra thunks in the returned `HValue`s, but
-- also avoid forcing the inside of a `Box`.
-- See Note [Forcing debuggee's thunks].
unpackStackField :: StackField -> IO HValue
unpackStackField x = case x of
  (StackBox (Box a)) -> returnIO (HValue a)
  (StackWord w) -> returnIO (HValue (Unsafe.Coerce.unsafeCoerce w))

unpackStackFields :: [StackField] -> Maybe [Int] -> IO [HValue]
unpackStackFields fs = \case
  Nothing -> mapM unpackStackField fs
  Just xs -> flip mapM xs $ \ i ->
       unpackStackField (fromMaybe (error ("Looking up StackField: " ++ show i)) (fs !? i))

-- | We @setInteractivePrintName@ with this so REPL results are forced but not
-- already printed to debug console.
noPrintConstant :: a -> IO ()
noPrintConstant x = x `seq` return ()

setLineBuffering :: IO ()
setLineBuffering = do
 System.IO.hSetBuffering System.IO.stdout System.IO.LineBuffering
 System.IO.hSetBuffering System.IO.stderr System.IO.LineBuffering
