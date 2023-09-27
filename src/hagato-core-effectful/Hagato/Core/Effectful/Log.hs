{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hagato.Core.Effectful.Log
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- A logging effect for the effectful ecosystem based on the 'Logger' type.
-----------------------------------------------------------------------------
module Hagato.Core.Effectful.Log
  ( Log
  , runLog
  , log
  , logIO
  , module Hagato.Core.Log
  ) where

-- base
import Data.Kind      (Type)
import Prelude hiding (log)

-- effectful-core
import Effectful                 (Dispatch(..), DispatchOf, Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Static (SideEffects(..), StaticRep, evalStaticRep, getStaticRep, unsafeEff_)

-- hagato:with-core
import Hagato.Core.Log

-- | The logging effect.
data Log (s :: Type) :: Effect

type instance DispatchOf (Log s) = Static WithSideEffects

newtype instance StaticRep (Log s) = Log (Logger s)

-- | Runs the t'Log' effect.
runLog :: IOE :> es => Logger s -> Eff (Log s : es) a -> Eff es a
runLog = evalStaticRep . Log

-- | Logs a value of type @s@.
log :: Log s :> es => LogLevel -> s -> Eff es ()
log level s = do
  Log (Logger f) <- getStaticRep
  unsafeEff_ $ f level s

-- | An 'IO'-based version of 'log'.
logIO :: Log s :> es => LogLevel -> IO s -> Eff es ()
logIO level m = do
  Log (Logger f) <- getStaticRep
  unsafeEff_ $ m >>= f level
