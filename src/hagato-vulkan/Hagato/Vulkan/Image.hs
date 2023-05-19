module Hagato.Vulkan.Image
  ( createImage
  , destroyImage
  , withImage
  , module Vulkan.Core10.Image
  ) where

-- base
import Control.Exception      (bracket)
import Control.Monad.IO.Class (MonadIO)

-- unliftio-core
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)

-- vulkan
import Vulkan                 qualified as Vk
import Vulkan.Core10.Image    hiding (createImage, destroyImage, withImage)
import Vulkan.CStruct.Extends qualified as Vk

createImage
  :: (Vk.Extendss Vk.ImageCreateInfo a, Vk.PokeChain a, MonadIO m)
  => Vk.Device -> Vk.ImageCreateInfo a -> m Vk.Image
createImage device info =
  Vk.createImage device info Nothing

destroyImage :: MonadIO m => Vk.Device -> Vk.Image -> m ()
destroyImage device image =
  Vk.destroyImage device image Nothing

withImage
  :: (Vk.Extendss Vk.ImageCreateInfo a, Vk.PokeChain a, MonadUnliftIO m)
  => Vk.Device -> Vk.ImageCreateInfo a -> (Vk.Image -> m b) -> m b
withImage device info f =
  withRunInIO $ \run ->
    bracket
      ( createImage device info )
      ( destroyImage device )
      ( run . f )
