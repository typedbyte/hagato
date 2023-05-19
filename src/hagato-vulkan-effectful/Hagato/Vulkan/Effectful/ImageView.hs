module Hagato.Vulkan.Effectful.ImageView
  ( allocateImageView
  , manageImageView
  , module Hagato.Vulkan.ImageView
  ) where

-- effectful-core
import Effectful (Eff, (:>))

-- hagato:with-vulkan
import Hagato.Vulkan qualified as Vk
import Hagato.Vulkan.ImageView

-- resource-effectful
import Effectful.Resource (Key, Resource, allocate)

allocateImageView
  :: (Vk.Extendss Vk.ImageViewCreateInfo a, Vk.PokeChain a, Resource :> es)
  => Vk.Device -> Vk.ImageViewCreateInfo a -> Eff es (Vk.ImageView, Key)
allocateImageView device info =
  allocate
    ( Vk.createImageView device info )
    ( Vk.destroyImageView device )

manageImageView
  :: (Vk.Extendss Vk.ImageViewCreateInfo a, Vk.PokeChain a, Resource :> es)
  => Vk.Device -> Vk.ImageViewCreateInfo a -> Eff es Vk.ImageView
manageImageView device info =
  fst <$> allocateImageView device info
