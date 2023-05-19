{-# LANGUAGE DataKinds #-}
module Hagato.Vulkan.Effectful.MemoryAllocator
  ( runMemory
  , module Hagato.Vulkan.MemoryAllocator
  , module VulkanMemoryAllocator
  ) where

-- effectful-core
import Effectful (Eff, IOE, (:>))

-- hagato:with-vma
import Hagato.Vulkan.MemoryAllocator (memoryStrategy)

-- hagato:with-vulkan-effectful
import Hagato.Vulkan.Effectful qualified as Vk

-- VulkanMemoryAllocator
import VulkanMemoryAllocator

runMemory :: IOE :> es => AllocationCreateFlagBits -> Eff (Vk.Memory Allocator Allocation : es) a -> Eff es a
runMemory = Vk.runMemory . memoryStrategy
