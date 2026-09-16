{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module GHC.Debugger.Session.Interactive where

import Control.Monad.IO.Class

import GHC
import GHC.Driver.Env
import GHC.Iface.Env (lookupNameCache)
import GHC.Debugger.Session.Builtin
import GHC.Plugins (mkVarOcc, Definite (Definite), GenUnit (RealUnit))

-- | Defines a strict @noPrintConstant :: a -> IO ()@ while taking care not to introduce its OccName into scope.
defineNoPrint :: Ghc Name
defineNoPrint = do
    hsc_env <- getSession
    let debuggerInternalUnit = RealUnit (Definite debuggerInternalUnitId)
    liftIO $ lookupNameCache (hsc_NC hsc_env) (mkModule debuggerInternalUnit debuggerRuntimeInternalModName)
       (mkVarOcc "noPrintConstant")
