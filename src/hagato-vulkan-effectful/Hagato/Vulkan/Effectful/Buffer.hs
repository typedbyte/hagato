module Hagato.Vulkan.Effectful.Buffer
  ( allocateBuffer
  , manageBuffer
  , module Hagato.Vulkan.Buffer
  ) where

-- effectful-core
import Effectful (Eff, (:>))

-- hagato:with-vulkan
import Hagato.Vulkan qualified as Vk
import Hagato.Vulkan.Buffer

-- resource-effectful
import Effectful.Resource (Key, Resource, allocate)

allocateBuffer
  :: (Vk.Extendss Vk.BufferCreateInfo a, Vk.PokeChain a, Resource :> es)
  => Vk.Device -> Vk.BufferCreateInfo a -> Eff es (Vk.Buffer, Key)
allocateBuffer device info =
  allocate
    ( Vk.createBuffer device info )
    ( Vk.destroyBuffer device )

manageBuffer
  :: (Vk.Extendss Vk.BufferCreateInfo a, Vk.PokeChain a, Resource :> es)
  => Vk.Device -> Vk.BufferCreateInfo a -> Eff es Vk.Buffer
manageBuffer device info =
  fst <$> allocateBuffer device info
