module Hagato.Core.Loop where

-- base
import Control.Monad          (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)

-- time
import Data.Time (diffUTCTime, getCurrentTime)

loop :: MonadIO m => s -> (Float -> s -> m (Bool, s)) -> m ()
loop initState f = do
  startTime <- liftIO getCurrentTime
  go startTime initState
    where
      go lastTime lastState = do
        now <- liftIO getCurrentTime
        let elapsedTime = realToFrac $ now `diffUTCTime` lastTime
        (done, newState) <- f elapsedTime lastState
        unless done $ go now newState

loopUntil :: MonadIO m => (s -> Bool) -> s -> (Float -> s -> m s) -> m ()
loopUntil done initState f = do
  startTime <- liftIO getCurrentTime
  go startTime initState
    where
      go lastTime lastState = do
        now <- liftIO getCurrentTime
        let elapsedTime = realToFrac $ now `diffUTCTime` lastTime
        newState <- f elapsedTime lastState
        unless (done newState) $ go now newState