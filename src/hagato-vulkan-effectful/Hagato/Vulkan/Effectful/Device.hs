module Hagato.Vulkan.Effectful.Device
  ( allocateDevice
  , manageDevice
  , module Hagato.Vulkan.Device
  ) where

-- effectful-core
import Effectful (Eff, (:>))

-- hagato:with-vulkan
import Hagato.Vulkan qualified as Vk
import Hagato.Vulkan.Device

-- resource-effectful
import Effectful.Resource (Key, Resource, allocate, manage)

allocateDevice :: Resource :> es => Vk.PhysicalDevice -> Vk.DeviceBuilder [Vk.QueueID] -> Eff es (Vk.Device, [Vk.Queue], Key)
allocateDevice physicalDevice builder = do
  ((device, queues), key) <-
    allocate
      ( Vk.createDevice physicalDevice builder )
      ( Vk.destroyDevice . fst )
  pure (device, queues, key)

manageDevice :: Resource :> es => Vk.PhysicalDevice -> Vk.DeviceBuilder [Vk.QueueID] -> Eff es (Vk.Device, [Vk.Queue])
manageDevice physicalDevice builder =
  manage
    ( Vk.createDevice physicalDevice builder )
    ( Vk.destroyDevice . fst )
