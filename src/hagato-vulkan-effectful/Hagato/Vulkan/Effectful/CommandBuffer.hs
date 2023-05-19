module Hagato.Vulkan.Effectful.CommandBuffer
  ( allocateCommandBuffers
  , manageCommandBuffers
  , module Hagato.Vulkan.CommandBuffer
  ) where

-- effectful-core
import Effectful (Eff, (:>))

-- hagato:with-vulkan
import Hagato.Vulkan               qualified as Vk
import Hagato.Vulkan.CommandBuffer hiding (allocateCommandBuffers)

-- resource-effectful
import Effectful.Resource (Key, Resource, allocate)

-- vector
import Data.Vector qualified as V

allocateCommandBuffers
  :: Resource :> es
  => Vk.Device
  -> Vk.CommandBufferAllocateInfo
  -> Eff es (V.Vector Vk.CommandBuffer, Key)
allocateCommandBuffers device info =
  allocate
    ( Vk.allocateCommandBuffers device info )
    ( Vk.freeCommandBuffers device (Vk.commandPool info) )

manageCommandBuffers
  :: Resource :> es
  => Vk.Device
  -> Vk.CommandBufferAllocateInfo
  -> Eff es (V.Vector Vk.CommandBuffer)
manageCommandBuffers device info =
  fst <$> allocateCommandBuffers device info
