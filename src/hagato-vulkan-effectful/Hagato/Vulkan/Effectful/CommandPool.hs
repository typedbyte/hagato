module Hagato.Vulkan.Effectful.CommandPool
  ( allocateCommandPool
  , manageCommandPool
  , module Hagato.Vulkan.CommandPool
  ) where

-- effectful-core
import Effectful (Eff, (:>))

-- hagato:with-vulkan
import Hagato.Vulkan qualified as Vk
import Hagato.Vulkan.CommandPool

-- resource-effectful
import Effectful.Resource (Key, Resource, allocate)

allocateCommandPool :: Resource :> es => Vk.Device -> Vk.CommandPoolCreateInfo -> Eff es (Vk.CommandPool, Key)
allocateCommandPool device info =
  allocate
    ( Vk.createCommandPool device info )
    ( Vk.destroyCommandPool device )

manageCommandPool :: Resource :> es => Vk.Device -> Vk.CommandPoolCreateInfo -> Eff es Vk.CommandPool
manageCommandPool device info =
  fst <$> allocateCommandPool device info
