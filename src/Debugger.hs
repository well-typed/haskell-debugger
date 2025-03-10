{-# LANGUAGE CPP, NamedFieldPuns, TupleSections, LambdaCase #-}
module Debugger where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe

import System.Exit

import qualified GHC
import qualified GHC.Driver.DynFlags as GHC
import qualified GHC.Driver.Phases as GHC
import qualified GHC.Driver.Pipeline as GHC
import qualified GHC.Driver.Config.Logger as GHC
import qualified GHC.Driver.Session.Units as GHC
import qualified GHC.Driver.Session.Mode as GHC
import qualified GHC.Driver.Session.Lint as GHC
import qualified GHC.Utils.Monad as GHC
import qualified GHC.Utils.Logger as GHC
import qualified GHC.Types.Unique.Supply as GHC
import qualified GHC.Runtime.Loader as GHC
import qualified Data.List.NonEmpty as NE
import qualified Data.List as List

import Debugger.Monad

-- | Evaluate expression. Includes context of breakpoint if stopped at one.
doEval :: String -> Debugger ()
doEval = undefined

