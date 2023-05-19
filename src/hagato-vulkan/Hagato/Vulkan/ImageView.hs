module Hagato.Vulkan.ImageView
  ( createImageView
  , destroyImageView
  , withImageView
  , module Vulkan.Core10.ImageView
  ) where

-- base
import Control.Exception      (bracket)
import Control.Monad.IO.Class (MonadIO)

-- unliftio-core
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)

-- vulkan
import Vulkan                  qualified as Vk
import Vulkan.Core10.ImageView hiding (createImageView, destroyImageView, withImageView)
import Vulkan.CStruct.Extends  qualified as Vk

createImageView
  :: (Vk.Extendss Vk.ImageViewCreateInfo a, Vk.PokeChain a, MonadIO m)
  => Vk.Device -> Vk.ImageViewCreateInfo a -> m Vk.ImageView
createImageView device info =
  Vk.createImageView device info Nothing

destroyImageView :: MonadIO m => Vk.Device -> Vk.ImageView -> m ()
destroyImageView device imageView =
  Vk.destroyImageView device imageView Nothing

withImageView
  :: (Vk.Extendss Vk.ImageViewCreateInfo a, Vk.PokeChain a, MonadUnliftIO m)
  => Vk.Device -> Vk.ImageViewCreateInfo a -> (Vk.ImageView -> m b) -> m b
withImageView device info f =
  withRunInIO $ \run ->
    bracket
      ( createImageView device info )
      ( destroyImageView device )
      ( run . f )
