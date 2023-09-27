{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hagato.Vulkan.MemoryAllocator
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- A GPU memory management strategy based on the Vulkan Memory Allocator (VMA).
-----------------------------------------------------------------------------
module Hagato.Vulkan.MemoryAllocator
  ( memoryStrategy
  , module VulkanMemoryAllocator
  ) where

-- base
import Data.Coerce (coerce)
import Foreign.Ptr (castFunPtr, castPtr)

-- hagato:with-vulkan
import Hagato.Vulkan qualified as Vk

-- vulkan
import Vulkan.Dynamic qualified as Vk

-- VulkanMemoryAllocator
import VulkanMemoryAllocator
import VulkanMemoryAllocator qualified as VMA

-- | Creates a GPU memory management strategy based on the Vulkan Memory
-- Allocator (VMA).
memoryStrategy :: VMA.AllocationCreateFlagBits -> Vk.MemoryStrategy VMA.Allocator VMA.Allocation
memoryStrategy allocFlags =
  Vk.MemoryStrategy
    { _createMemoryAllocator =
        \vk physicalDevice device ->
          fmap coerce . VMA.createAllocator $
            Vk.zero
              { VMA.physicalDevice  = physicalDevice.physicalDeviceHandle
              , VMA.device          = device.deviceHandle
              , VMA.instance'       = vk.instanceHandle
              , VMA.vulkanFunctions =
                  Just $
                  Vk.zero
                    { VMA.vkGetInstanceProcAddr =
                        castFunPtr $
                         physicalDevice.instanceCmds.pVkGetInstanceProcAddr
                    , VMA.vkGetDeviceProcAddr = 
                        castFunPtr $
                         device.deviceCmds.pVkGetDeviceProcAddr
                    }
              }
    , _destroyMemoryAllocator =
        VMA.destroyAllocator . coerce
    , _createBufferMemory =
        \allocator buffer flags -> do
          (alloc, info) <-
            VMA.allocateMemoryForBuffer (coerce allocator) buffer $
              Vk.zero
                { VMA.flags         = allocFlags
                , VMA.requiredFlags = flags
                }
          pure $
            Vk.Allocation
              { key    = alloc
              , handle = info.deviceMemory
              , offset = info.offset
              , size   = info.size
              }
    , _createImageMemory =
        \allocator image flags -> do
          (alloc, info) <-
            VMA.allocateMemoryForImage (coerce allocator) image $
              Vk.zero
                { VMA.flags         = allocFlags
                , VMA.requiredFlags = flags
                }
          pure $
            Vk.Allocation
              { key    = alloc
              , handle = info.deviceMemory
              , offset = info.offset
              , size   = info.size
              }
    , _destroyBufferMemory =
        \allocator allocation ->
          VMA.freeMemory (coerce allocator) allocation.key
    , _destroyImageMemory =
        \allocator allocation ->
          VMA.freeMemory (coerce allocator) allocation.key
    , _bindBufferMemory =
        \allocator buffer allocation ->
          VMA.bindBufferMemory (coerce allocator) allocation.key buffer
    , _bindImageMemory =
        \allocator image allocation ->
          VMA.bindImageMemory (coerce allocator) allocation.key image
    , _mapBufferMemory =
        \allocator allocation ->
          castPtr <$>
            VMA.mapMemory (coerce allocator) allocation.key
    , _mapImageMemory =
        \allocator allocation ->
          castPtr <$>
            VMA.mapMemory (coerce allocator) allocation.key
    , _unmapBufferMemory =
        \allocator allocation ->
          VMA.unmapMemory (coerce allocator) allocation.key
    , _unmapImageMemory =
        \allocator allocation ->
          VMA.unmapMemory (coerce allocator) allocation.key
    }
