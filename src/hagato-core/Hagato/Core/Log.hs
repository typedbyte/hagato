-----------------------------------------------------------------------------
-- |
-- Module      : Hagato.Core.Log
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Types and functions for a generic logging mechanism.
-----------------------------------------------------------------------------
module Hagato.Core.Log
  ( -- * Logging Strategy
    Logger(..)
  , LogLevel(..)
  , stdoutLogger
  , voidLogger
    -- * Strategy Converter
  , mapLogger
  , mapLoggerIO
  , showLogger
  ) where

-- base
import Control.Monad (when)
import Data.Char     (toUpper)

-- | Represents the importance of a logging action.
data LogLevel
  = Debug
  | Info
  | Warning
  | Error
  deriving (Eq, Ord, Show)

-- | Represents a strategy for logging values of type @s@.
newtype Logger s = Logger { runLogger :: LogLevel -> s -> IO () }

-- | Converts a logging strategy for @b@ values into a logging strategy for @a@ values.
mapLogger :: (a -> b) -> Logger b -> Logger a
mapLogger func (Logger f) =
  Logger $ \level a ->
    f level (func a)

-- | An 'IO'-based version of 'mapLogger'.
mapLoggerIO :: (a -> IO b) -> Logger b -> Logger a
mapLoggerIO func (Logger f) =
  Logger $ \level a ->
    func a >>= f level

-- | Converts a 'String'-based logging strategy into a 'Show'-based logging strategy.
showLogger :: Show s => Logger String -> Logger s
showLogger = mapLogger show

-- | Smart constructor for a standard stdout-based logging strategy.
--
-- The specified log level is the minimum importance of logging actions, i.e.
-- all logging actions with an importance lower than the specified log level
-- are discarded.
stdoutLogger :: LogLevel -> Logger String
stdoutLogger logLevel =
  Logger $ \level txt ->
    when (level >= logLevel) $
      putStrLn $ fmap toUpper (show level) ++ (':' : ' ' : txt)

-- | Smart constructor for a logging strategy that discards all logging actions.
voidLogger :: Logger s
voidLogger =
  Logger $ \_ _ ->
    pure ()