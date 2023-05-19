module Hagato.Vulkan.Window where

-- base
import Control.Exception      (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)

-- hagato
import Hagato.Core.Input (Event(TickEvent), Input(events))

-- unliftio-core
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)

-- vulkan
import Vulkan qualified as Vk

type WindowWidth  = Int
type WindowHeight = Int
type WindowTitle  = String

data WindowStrategy w =
  WindowStrategy
    { _createWindow   :: WindowWidth -> WindowHeight -> WindowTitle -> IO w
    , _destroyWindow  :: w -> IO ()
    , _getExtensions  :: w -> IO [String]
    , _createSurface  :: w -> Vk.Instance -> IO Vk.SurfaceKHR
    , _destroySurface :: w -> Vk.Instance -> Vk.SurfaceKHR -> IO ()
    , _resized        :: w -> IO Bool
    , _poll           :: w -> IO Input
    }

createWindow :: MonadIO m => WindowStrategy w -> WindowWidth -> WindowHeight -> WindowTitle -> m w
createWindow strategy width height title =
  liftIO $
    strategy._createWindow width height title

destroyWindow :: MonadIO m => WindowStrategy w -> w -> m ()
destroyWindow strategy window =
  liftIO $
    strategy._destroyWindow window

withWindow :: MonadUnliftIO m => WindowStrategy w -> WindowWidth -> WindowHeight -> WindowTitle -> (w -> m a) -> m a
withWindow strategy width height title f =
  withRunInIO $ \run ->
    bracket
      ( strategy._createWindow width height title )
      ( strategy._destroyWindow )
      ( run . f )

getExtensions :: MonadIO m => WindowStrategy w -> w -> m [String]
getExtensions strategy window =
  liftIO $
    strategy._getExtensions window

createSurface :: MonadIO m => WindowStrategy w -> w -> Vk.Instance -> m Vk.SurfaceKHR
createSurface strategy window vk =
  liftIO $
    strategy._createSurface window vk

destroySurface :: MonadIO m => WindowStrategy w -> w -> Vk.Instance -> Vk.SurfaceKHR -> m ()
destroySurface strategy window vk surface =
  liftIO $
    strategy._destroySurface window vk surface

withSurface :: MonadUnliftIO m => WindowStrategy w -> w -> Vk.Instance -> (Vk.SurfaceKHR -> m a) -> m a
withSurface strategy window vk f =
  withRunInIO $ \run ->
    bracket
      ( strategy._createSurface window vk )
      ( strategy._destroySurface window vk )
      ( run . f )

resized :: MonadIO m => WindowStrategy w -> w -> m Bool
resized strategy window =
  liftIO $
    strategy._resized window
{-# INLINE resized #-}

poll :: MonadIO m => WindowStrategy w -> w -> m Input
poll strategy window =
  liftIO $
    strategy._poll window
{-# INLINE poll #-}

tickPoll :: MonadIO m => WindowStrategy w -> Float -> w -> m Input
tickPoll strategy dt window =
  liftIO $ do
    input <- strategy._poll window
    pure $ input { events = TickEvent dt : input.events }
{-# INLINE tickPoll #-}