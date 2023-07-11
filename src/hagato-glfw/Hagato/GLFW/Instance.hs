-----------------------------------------------------------------------------
-- |
-- Module      : Hagato.GLFW.Instance
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Types and functions for initializing and terminating GLFW.
-----------------------------------------------------------------------------
module Hagato.GLFW.Instance (GLFW, initialize, terminate, withGLFW) where

-- base
import Control.Exception      (bracket)
import Control.Monad          (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)

-- GLFW-b
import Graphics.UI.GLFW qualified as GLFW

-- unliftio-core
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)

-- | Represents a proof that GLFW has been initialized successfully.
data GLFW = GLFW

-- | Initializes GLFW.
initialize :: MonadIO m => m GLFW
initialize =
  liftIO $
    GLFW.init
      >>= flip unless (fail "Could not initialize GLFW.")
      >> pure GLFW

-- | Terminates GLFW. Any 'GLFW' proof value is invalid after this call.
terminate :: MonadIO m => m ()
terminate = liftIO GLFW.terminate

-- | A convenient wrapper around 'initialize' and 'terminate'.
withGLFW :: MonadUnliftIO m => (GLFW -> m a) -> m a
withGLFW f =
  withRunInIO $ \run ->
    bracket
      ( initialize )
      ( const terminate )
      ( run . f )
