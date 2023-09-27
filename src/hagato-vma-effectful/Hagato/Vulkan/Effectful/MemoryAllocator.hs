{-# LANGUAGE DataKinds #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hagato.Vulkan.Effectful.MemoryAllocator
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- A handler for the 'Vk.Memory' effect using the Vulkan Memory Allocator (VMA).
-----------------------------------------------------------------------------
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

-- | Runs the 'Vk.Memory' effect using the Vulkan Memory Allocator (VMA).
runMemory :: IOE :> es => AllocationCreateFlagBits -> Eff (Vk.Memory Allocator Allocation : es) a -> Eff es a
runMemory = Vk.runMemory . memoryStrategy
