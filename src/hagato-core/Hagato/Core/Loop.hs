-----------------------------------------------------------------------------
-- |
-- Module      : Hagato.Core.Loop
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Utility functions for handling the game loop.
-----------------------------------------------------------------------------
module Hagato.Core.Loop where

-- base
import Control.Monad          (void)
import Control.Monad.IO.Class (MonadIO, liftIO)

-- time
import Data.Time (diffUTCTime, getCurrentTime)

-- | Loops an action until it signals an end.
loop
  :: MonadIO m
  => s
  -- ^ Initial state of the loop.
  -> (Float -> s -> m (Bool, s))
  -- ^ Function which performs one loop iteration. Takes the elapsed time since
  -- the last iteration (in seconds) and the current state as input, and produces
  -- an ending signal ('False' to continue, 'True' to terminate) and a new state.
  -> m s
  -- ^ Final state after ending the loop.
loop initState f = do
  startTime <- liftIO getCurrentTime
  go startTime initState
    where
      go lastTime lastState = do
        now <- liftIO getCurrentTime
        let elapsedTime = realToFrac $ now `diffUTCTime` lastTime
        (done, newState) <- f elapsedTime lastState
        if done then
          pure newState
        else
          go now newState

-- | A version of 'loop' which ignores the final state.
loop_ :: MonadIO m => s -> (Float -> s -> m (Bool, s)) -> m ()
loop_ initState f =
  void $
    loop initState f

-- | Loops an action until the specified function signals an end.
loopUntil
  :: MonadIO m
  => (s -> Bool)
  -- ^ Function which produces an ending signal ('False' to continue, 'True' to terminate).
  -> s
  -- ^ Initial state of the loop.
  -> (Float -> s -> m s)
  -- ^ Function which performs one loop iteration. Takes the elapsed time since
  -- the last iteration (in seconds) and the current state as input, and produces
  -- a new state.
  -> m s
  -- ^ Final state after ending the loop.
loopUntil done initState f =
  if done initState then
    pure initState
  else
    loop initState $ \dt s -> do
      newState <- f dt s
      pure (done newState, newState)

-- | A version of 'loopUntil' which ignores the final state.
loopUntil_ :: MonadIO m => (s -> Bool) -> s -> (Float -> s -> m s) -> m ()
loopUntil_ done initState f =
  void $
    loopUntil done initState f