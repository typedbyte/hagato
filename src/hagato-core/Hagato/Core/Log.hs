module Hagato.Core.Log where

-- base
import Control.Monad (when)
import Data.Char     (toUpper)

data LogLevel
  = Debug
  | Info
  | Warning
  | Error
  deriving (Eq, Ord, Show)

newtype Logger s = Logger { runLogger :: LogLevel -> s -> IO () }

mapLogger :: (a -> b) -> Logger b -> Logger a
mapLogger func (Logger f) =
  Logger $ \level a ->
    f level (func a)

mapLoggerIO :: (a -> IO b) -> Logger b -> Logger a
mapLoggerIO func (Logger f) =
  Logger $ \level a ->
    func a >>= f level

showLogger :: Show s => Logger String -> Logger s
showLogger = mapLogger show

stdoutLogger :: LogLevel -> Logger String
stdoutLogger logLevel =
  Logger $ \level txt ->
    when (level >= logLevel) $
      putStrLn $ fmap toUpper (show level) ++ (':' : ' ' : txt)

voidLogger :: Logger s
voidLogger =
  Logger $ \_ _ ->
    pure ()