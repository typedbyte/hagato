module Hagato.Vulkan.Effectful.Image
  ( allocateImage
  , manageImage
  , module Hagato.Vulkan.Image
  ) where

-- effectful-core
import Effectful (Eff, (:>))

-- hagato:with-vulkan
import Hagato.Vulkan qualified as Vk
import Hagato.Vulkan.Image

-- resource-effectful
import Effectful.Resource (Key, Resource, allocate)

allocateImage
  :: (Vk.Extendss Vk.ImageCreateInfo a, Vk.PokeChain a, Resource :> es)
  => Vk.Device -> Vk.ImageCreateInfo a -> Eff es (Vk.Image, Key)
allocateImage device info =
  allocate
    ( Vk.createImage device info )
    ( Vk.destroyImage device )

manageImage
  :: (Vk.Extendss Vk.ImageCreateInfo a, Vk.PokeChain a, Resource :> es)
  => Vk.Device -> Vk.ImageCreateInfo a -> Eff es Vk.Image
manageImage device info =
  fst <$> allocateImage device info
