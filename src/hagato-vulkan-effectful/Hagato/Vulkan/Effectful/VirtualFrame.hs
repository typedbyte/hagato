{-# LANGUAGE RecordWildCards #-}
module Hagato.Vulkan.Effectful.VirtualFrame
  ( allocateVirtualFrames
  , manageVirtualFrames
  , recreateVirtualFrames
  , getNextVirtualFrame
  , module Hagato.Vulkan.VirtualFrame
  ) where

-- base
import Data.Word (Word32)

-- effectful-core
import Effectful                    (Eff, IOE, (:>))
import Effectful.State.Static.Local (State, stateM)

-- hagato:with-vulkan
import Hagato.Vulkan              qualified as Vk
import Hagato.Vulkan.VirtualFrame hiding (getNextVirtualFrame)

-- resource-effectful
import Effectful.Resource (Key, Resource, freeAll)

-- vector
import Data.Vector qualified as V

import Hagato.Vulkan.Effectful.CommandBuffer (manageCommandBuffers)
import Hagato.Vulkan.Effectful.Fence         (manageFence)
import Hagato.Vulkan.Effectful.Semaphore     (allocateSemaphore, manageSemaphore)

allocateVirtualFrames
  :: Resource :> es
  => Vk.Device
  -> Vk.CommandPool
  -> Word32
  -> Eff es (V.Vector Vk.VirtualFrame, V.Vector Key)
allocateVirtualFrames device pool frameCount = do
  buffers <-
    manageCommandBuffers device $
      Vk.zero
        { Vk.commandPool        = pool
        , Vk.level              = Vk.COMMAND_BUFFER_LEVEL_PRIMARY
        , Vk.commandBufferCount = frameCount
        }
  fmap V.unzip $
    V.forM buffers $ \commandBuffer -> do
      (acquireSemaphore, key) <- allocateSemaphore device
      renderSemaphore         <- manageSemaphore device
      renderFence             <- manageFence device
      pure (Vk.VirtualFrame{..}, key)

manageVirtualFrames
  :: Resource :> es
  => Vk.Device
  -> Vk.CommandPool
  -> Word32
  -> Eff es (V.Vector Vk.VirtualFrame)
manageVirtualFrames device pool frameCount =
  fst <$> allocateVirtualFrames device pool frameCount

recreateVirtualFrames
  :: Resource :> es
  => Vk.Device
  -> V.Vector Vk.VirtualFrame
  -> V.Vector Key
  -> Eff es (V.Vector Vk.VirtualFrame, V.Vector Key)
recreateVirtualFrames device frames keys = do
  freeAll keys
  fmap V.unzip $
    V.forM frames $ \frame -> do
      (acquireSemaphore, key) <- allocateSemaphore device
      pure (frame { acquireSemaphore = acquireSemaphore }, key)

getNextVirtualFrame
  :: (State Vk.FrameIndex :> es, IOE :> es)
  => Vk.Device
  -> V.Vector Vk.VirtualFrame
  -> Eff es Vk.VirtualFrame
getNextVirtualFrame device frames =
  stateM $ Vk.getNextVirtualFrame device frames
{-# INLINE getNextVirtualFrame #-}
