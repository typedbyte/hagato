-----------------------------------------------------------------------------
-- |
-- Module      : Hagato.Vulkan.Buffer
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Convenience functions and re-exports for handling Vulkan commands.
-----------------------------------------------------------------------------
module Hagato.Vulkan.Command
  ( pushStorableConstants
  , module Vulkan.Core10.CommandBufferBuilding
  ) where

-- base
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Word              (Word32)
import Foreign.Marshal.Array  (withArrayLen)
import Foreign.Ptr            (castPtr)
import Foreign.Storable       (Storable, sizeOf)

-- vulkan
import Vulkan qualified as Vk
import Vulkan.Core10.CommandBufferBuilding

-- | Updating the values of push constants using a list of values with a 'Storable' instance.
pushStorableConstants
  :: (Storable a, MonadIO m)
  => Vk.CommandBuffer
  -- ^ The command buffer in which the push constant update will be recorded.
  -> Vk.PipelineLayout
  -- ^ The pipeline layout used to program the push constant updates.
  -> Vk.ShaderStageFlags
  -- ^ The shader stages that will use the push constants in the updated range.
  -> Word32
  -- ^ The start offset of the push constant range to update, in units of bytes.
  -> [a]
  -- ^ A list containing the new push constant values.
  -> m ()
pushStorableConstants buffer layout flags offset values =
  liftIO $
    withArrayLen values $ \n ptr ->
      Vk.cmdPushConstants
        ( buffer )
        ( layout )
        ( flags )
        ( offset )
        ( fromIntegral $ n * sizeOf (values !! 0) )
        ( castPtr ptr )
{-# INLINE pushStorableConstants #-}