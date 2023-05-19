{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists       #-}
module Hagato.Vulkan.Effectful.Command where

-- base
import Data.Int    (Int32)
import Data.Word   (Word32)
import Foreign     (Storable)
import Foreign.Ptr (Ptr)

-- effectful-core
import Effectful               (Eff, IOE, (:>))
import Effectful.Reader.Static (Reader, ask, runReader)

-- hagato:with-vulkan
import Hagato.Vulkan qualified as Vk

-- vector
import Data.Vector qualified as V

-- vulkan
import Vulkan                      qualified as Vk
import Vulkan.Core10.CommandBuffer qualified as Vb
import Vulkan.Core10.OtherTypes    qualified as Vo

record
  :: IOE :> es
  => Vk.CommandBuffer
  -> Vk.CommandBufferUsageFlags
  -> Eff (Reader Vk.CommandBuffer : es) a
  -> Eff es a
record buffer flags m = do
  Vk.beginCommandBuffer buffer $ Vk.zero { Vb.flags = flags }
  a <- runReader buffer m
  Vk.endCommandBuffer buffer
  pure a

beginRenderPass
  :: (Vk.Extendss Vk.RenderPassBeginInfo a, Vk.PokeChain a, Reader Vk.CommandBuffer :> es, IOE :> es)
  => Vk.SubpassContents -> Vk.RenderPassBeginInfo a -> Eff es ()
beginRenderPass contents info = do
  buffer <- ask
  Vk.cmdBeginRenderPass buffer info contents

bindDescriptorSets
  :: (Reader Vk.CommandBuffer :> es, IOE :> es)
  => Vk.PipelineBindPoint
  -> Vk.PipelineLayout
  -> Word32
  -> V.Vector Vk.DescriptorSet
  -> V.Vector Word32
  -> Eff es ()
bindDescriptorSets bindPoint layout firstSet sets offsets = do
  buffer <- ask
  Vk.cmdBindDescriptorSets buffer bindPoint layout firstSet sets offsets
{-# INLINE bindDescriptorSets #-}

bindIndexBuffer
  :: (Reader Vk.CommandBuffer :> es, IOE :> es)
  => Vk.Buffer
  -> Vk.DeviceSize
  -> Vk.IndexType
  -> Eff es ()
bindIndexBuffer buffer offset ixType = do
  cmdBuffer <- ask
  Vk.cmdBindIndexBuffer cmdBuffer buffer offset ixType
{-# INLINE bindIndexBuffer #-}

bindPipeline
  :: (Reader Vk.CommandBuffer :> es, IOE :> es)
  => Vk.PipelineBindPoint
  -> Vk.Pipeline
  -> Eff es ()
bindPipeline bindPoint pipeline = do
  buffer <- ask
  Vk.cmdBindPipeline buffer bindPoint pipeline
{-# INLINE bindPipeline #-}

bindGraphicsPipeline
  :: (Reader Vk.CommandBuffer :> es, IOE :> es)
  => Vk.Pipeline
  -> Eff es ()
bindGraphicsPipeline = bindPipeline Vk.PIPELINE_BIND_POINT_GRAPHICS
{-# INLINE bindGraphicsPipeline #-}

bindVertexBuffers
  :: (Reader Vk.CommandBuffer :> es, IOE :> es)
  => Word32
  -> V.Vector Vk.Buffer
  -> V.Vector Vk.DeviceSize
  -> Eff es ()
bindVertexBuffers firstBinding buffers offsets = do
  buffer <- ask
  Vk.cmdBindVertexBuffers buffer firstBinding buffers offsets
{-# INLINE bindVertexBuffers #-}

copyBuffer
  :: (Reader Vk.CommandBuffer :> es, IOE :> es)
  => Vk.Buffer
  -> Vk.Buffer
  -> V.Vector Vk.BufferCopy
  -> Eff es ()
copyBuffer src dst regions = do
  buffer <- ask
  Vk.cmdCopyBuffer buffer src dst regions

copyBufferToImage
  :: (Reader Vk.CommandBuffer :> es, IOE :> es)
  => Vk.Buffer
  -> Vk.Image
  -> Vk.ImageLayout
  -> V.Vector Vk.BufferImageCopy
  -> Eff es ()
copyBufferToImage src dst dstLayout regions = do
  buffer <- ask
  Vk.cmdCopyBufferToImage buffer src dst dstLayout regions

copyImage
  :: (Reader Vk.CommandBuffer :> es, IOE :> es)
  => Vk.Image
  -> Vk.ImageLayout
  -> Vk.Image
  -> Vk.ImageLayout
  -> V.Vector Vk.ImageCopy
  -> Eff es ()
copyImage src srcLayout dst dstLayout regions = do
  buffer <- ask
  Vk.cmdCopyImage buffer src srcLayout dst dstLayout regions

draw
  :: (Reader Vk.CommandBuffer :> es, IOE :> es)
  => Word32
  -> Word32
  -> Word32
  -> Word32
  -> Eff es ()
draw vertexCount instanceCount firstVertex firstInstance = do
  buffer <- ask
  Vk.cmdDraw buffer vertexCount instanceCount firstVertex firstInstance
{-# INLINE draw #-}

drawIndexed
  :: (Reader Vk.CommandBuffer :> es, IOE :> es)
  => Word32
  -> Word32
  -> Word32
  -> Int32
  -> Word32
  -> Eff es ()
drawIndexed indexCount instanceCount firstIndex vertexOffset firstInstance = do
  buffer <- ask
  Vk.cmdDrawIndexed buffer indexCount instanceCount firstIndex vertexOffset firstInstance
{-# INLINE drawIndexed #-}

endRenderPass :: (Reader Vk.CommandBuffer :> es, IOE :> es) => Eff es ()
endRenderPass = do
  buffer <- ask
  Vk.cmdEndRenderPass buffer

executeCommands
  :: (Reader Vk.CommandBuffer :> es, IOE :> es)
  => V.Vector Vk.CommandBuffer
  -> Eff es ()
executeCommands buffers = do
  buffer <- ask
  Vk.cmdExecuteCommands buffer buffers
{-# INLINE executeCommands #-}

nextSubpass :: (Reader Vk.CommandBuffer :> es, IOE :> es) => Vk.SubpassContents -> Eff es ()
nextSubpass contents = do
  buffer <- ask
  Vk.cmdNextSubpass buffer contents
{-# INLINE nextSubpass #-}

pipelineBarrier
  :: (Reader Vk.CommandBuffer :> es, IOE :> es)
  => Vk.PipelineStageFlags
  -> Vk.PipelineStageFlags
  -> Vk.DependencyFlags
  -> V.Vector Vk.MemoryBarrier
  -> V.Vector Vk.BufferMemoryBarrier
  -> V.Vector (Vk.SomeStruct Vk.ImageMemoryBarrier)
  -> Eff es ()
pipelineBarrier srcMask dstMask dependencyFlags memBarriers bufferMemBarriers imgBarriers = do
  buffer <- ask
  Vk.cmdPipelineBarrier buffer srcMask dstMask dependencyFlags memBarriers bufferMemBarriers imgBarriers
{-# INLINE pipelineBarrier #-}

presentToDrawBarrier
  :: (Reader Vk.CommandBuffer :> es, IOE :> es)
  => Vk.QueueFamily -> Vk.QueueFamily -> Vk.Image -> Eff es ()
presentToDrawBarrier pFamily gFamily image =
  if pFamily == gFamily then
    pure ()
  else
    pipelineBarrier
      Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
      Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
      Vk.zero
      []
      []
      [ Vk.SomeStruct $
          Vk.zero
            { Vo.srcAccessMask       = Vk.ACCESS_MEMORY_READ_BIT
            , Vo.dstAccessMask       = Vk.ACCESS_MEMORY_READ_BIT
            , Vo.oldLayout           = Vk.IMAGE_LAYOUT_PRESENT_SRC_KHR
            , Vo.newLayout           = Vk.IMAGE_LAYOUT_PRESENT_SRC_KHR
            , Vo.srcQueueFamilyIndex = pFamily
            , Vo.dstQueueFamilyIndex = gFamily
            , Vo.image               = image
            , Vo.subresourceRange    =
                Vk.zero
                  { Vk.aspectMask     = Vk.IMAGE_ASPECT_COLOR_BIT
                  , Vk.levelCount     = 1
                  , Vk.layerCount     = 1
                  , Vk.baseArrayLayer = 0
                  , Vk.baseMipLevel   = 0
                  }
            }
      ]
{-# INLINE presentToDrawBarrier #-}

drawToPresentBarrier
  :: (Reader Vk.CommandBuffer :> es, IOE :> es)
  => Vk.QueueFamily -> Vk.QueueFamily -> Vk.Image -> Eff es ()
drawToPresentBarrier gFamily pFamily image =
  if gFamily == pFamily then
    pure ()
  else
    pipelineBarrier
      Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
      Vk.PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT
      Vk.zero
      []
      []
      [ Vk.SomeStruct $
          Vk.zero
            { Vo.srcAccessMask       = Vk.ACCESS_MEMORY_READ_BIT
            , Vo.dstAccessMask       = Vk.ACCESS_MEMORY_READ_BIT
            , Vo.oldLayout           = Vk.IMAGE_LAYOUT_PRESENT_SRC_KHR
            , Vo.newLayout           = Vk.IMAGE_LAYOUT_PRESENT_SRC_KHR
            , Vo.srcQueueFamilyIndex = gFamily
            , Vo.dstQueueFamilyIndex = pFamily
            , Vo.image               = image
            , Vo.subresourceRange    =
                Vk.zero
                  { Vk.aspectMask     = Vk.IMAGE_ASPECT_COLOR_BIT
                  , Vk.levelCount     = 1
                  , Vk.layerCount     = 1
                  , Vk.baseArrayLayer = 0
                  , Vk.baseMipLevel   = 0
                  }
            }
      ]
{-# INLINE drawToPresentBarrier #-}

pushConstants
  :: (Reader Vk.CommandBuffer :> es, IOE :> es)
  => Vk.PipelineLayout
  -> Vk.ShaderStageFlags
  -> Word32
  -> Word32
  -> Ptr ()
  -> Eff es ()
pushConstants layout flags offset size values = do
  buffer <- ask
  Vk.cmdPushConstants buffer layout flags offset size values
{-# INLINE pushConstants #-}

pushStorableConstants
  :: (Storable a, Reader Vk.CommandBuffer :> es, IOE :> es)
  => Vk.PipelineLayout
  -> Vk.ShaderStageFlags
  -> Word32
  -> [a]
  -> Eff es ()
pushStorableConstants layout flags offset values = do
  buffer <- ask
  Vk.pushStorableConstants buffer layout flags offset values
{-# INLINE pushStorableConstants #-}

setBlendConstants
  :: (Reader Vk.CommandBuffer :> es, IOE :> es)
  => Float
  -> Float
  -> Float
  -> Float
  -> Eff es ()
setBlendConstants r g b a = do
  buffer <- ask
  Vk.cmdSetBlendConstants buffer (r, g, b, a)
{-# INLINE setBlendConstants #-}

setDepthBias
  :: (Reader Vk.CommandBuffer :> es, IOE :> es)
  => Float
  -> Float
  -> Float
  -> Eff es ()
setDepthBias constantFactor clamp slopeFactor = do
  buffer <- ask
  Vk.cmdSetDepthBias buffer constantFactor clamp slopeFactor
{-# INLINE setDepthBias #-}

setLineWidth :: (Reader Vk.CommandBuffer :> es, IOE :> es) => Float -> Eff es ()
setLineWidth width = do
  buffer <- ask
  Vk.cmdSetLineWidth buffer width
{-# INLINE setLineWidth #-}

setScissor
  :: (Reader Vk.CommandBuffer :> es, IOE :> es)
  => Word32
  -> V.Vector Vk.Rect2D
  -> Eff es ()
setScissor firstScissor scissors = do
  buffer <- ask
  Vk.cmdSetScissor buffer firstScissor scissors
{-# INLINE setScissor #-}

setViewport
  :: (Reader Vk.CommandBuffer :> es, IOE :> es)
  => Word32
  -> V.Vector Vk.Viewport
  -> Eff es ()
setViewport firstViewport viewports = do
  buffer <- ask
  Vk.cmdSetViewport buffer firstViewport viewports
{-# INLINE setViewport #-}
