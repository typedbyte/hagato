-----------------------------------------------------------------------------
-- |
-- Module      : Hagato.Vulkan.CommandBuffer
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Convenience functions and re-exports for handling Vulkan command buffers.
-----------------------------------------------------------------------------
module Hagato.Vulkan.CommandBuffer
  ( withCommandBuffers
  , module Vulkan.Core10.CommandBuffer
  ) where

-- base
import Control.Exception (bracket)

-- unliftio-core
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)

-- vector
import Data.Vector qualified as V

-- vulkan
import Vulkan                      qualified as Vk
import Vulkan.Core10.CommandBuffer hiding (withCommandBuffers)

-- | A convenient wrapper around 'allocateCommandBuffers' and 'freeCommandBuffers'.
withCommandBuffers
  :: MonadUnliftIO m
  => Vk.Device
  -> Vk.CommandBufferAllocateInfo
  -> (V.Vector Vk.CommandBuffer -> m a)
  -> m a
withCommandBuffers device info f =
  withRunInIO $ \run ->
    bracket
      ( allocateCommandBuffers device info )
      ( freeCommandBuffers device (Vk.commandPool info) )
      ( run . f )
