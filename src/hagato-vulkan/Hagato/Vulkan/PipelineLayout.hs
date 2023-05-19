module Hagato.Vulkan.PipelineLayout
  ( createPipelineLayout
  , destroyPipelineLayout
  , withPipelineLayout
  , module Vulkan.Core10.PipelineLayout
  ) where

-- base
import Control.Exception      (bracket)
import Control.Monad.IO.Class (MonadIO)

-- unliftio-core
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)

-- vulkan
import Vulkan                       qualified as Vk
import Vulkan.Core10.PipelineLayout hiding (createPipelineLayout, destroyPipelineLayout, withPipelineLayout)

createPipelineLayout :: MonadIO m => Vk.Device -> Vk.PipelineLayoutCreateInfo -> m Vk.PipelineLayout
createPipelineLayout device info =
  Vk.createPipelineLayout device info Nothing

destroyPipelineLayout :: MonadIO m => Vk.Device -> Vk.PipelineLayout -> m ()
destroyPipelineLayout device layout =
  Vk.destroyPipelineLayout device layout Nothing

withPipelineLayout
  :: MonadUnliftIO m
  => Vk.Device
  -> Vk.PipelineLayoutCreateInfo
  -> (Vk.PipelineLayout -> m a)
  -> m a
withPipelineLayout device info f =
  withRunInIO $ \run ->
    bracket
      ( createPipelineLayout device info )
      ( destroyPipelineLayout device )
      ( run . f )
