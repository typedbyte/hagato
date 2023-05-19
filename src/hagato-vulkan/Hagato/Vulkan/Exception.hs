module Hagato.Vulkan.Exception
  ( HagatoVulkanException(..)
  , throwIO
  , throwVk
  , module Vulkan.Exception
  ) where

-- base
import Control.Exception      qualified as E
import Control.Monad.IO.Class (MonadIO, liftIO)

-- vulkan
import Vulkan qualified as Vk
import Vulkan.Exception

data HagatoVulkanException
  = InternalError
  | NoGraphicsQueueFamily
  | NoPresentationQueueFamily
  | NoSuitableDevice
  | NoSuitablePhysicalDevice
  | NoSuitableQueue
  | NoSurfaceFormat
  deriving (Eq, Ord, Show)

instance E.Exception HagatoVulkanException

throwIO :: (E.Exception e, MonadIO m) => e -> m a
throwIO = liftIO . E.throwIO

throwVk :: MonadIO m => Vk.Result -> m a
throwVk = throwIO . VulkanException
