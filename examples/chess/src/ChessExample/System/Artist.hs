{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE MultiWayIf         #-}
{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE RecordWildCards    #-}
module ChessExample.System.Artist (render) where

-- apecs-effectful
import Apecs.Effectful (ECS, cmapM_, get, global, set, tryGet)

-- base
import Prelude hiding (null)

-- effectful-core
import Effectful                    (Eff, IOE)
import Effectful.Reader.Static      (Reader)
import Effectful.State.Static.Local (State)

-- hagato:with-core
import Hagato.Core (ColumnMajor(..), Mat4, fromTRS, multiply, poke, setTranslation, sizeOf, viewMatrix)

-- hagato:with-core-effectful
import Hagato.Core.Effectful (Log)

-- hagato:with-vulkan
import Hagato.Vulkan hiding (get)

-- hagato:with-vulkan-effectful
import Hagato.Vulkan.Effectful        qualified as Hk
import Hagato.Vulkan.Effectful.Memory (Memory)

-- resource-effectful
import Effectful.Resource (Resource)

-- vector
import Data.Vector qualified as V

-- vulkan
import Vulkan      qualified as Vk
import Vulkan.Zero qualified as Vk

import ChessExample.Component.Camera      (Camera(Camera))
import ChessExample.Component.Environment (Environment(..))
import ChessExample.Component.Focus       (Focus(..))
import ChessExample.Component.Mesh        (Geometry(..), Material(..), Mesh(..), Primitive(..))
import ChessExample.Component.Screen      (Screen(dirty))
import ChessExample.Component.Transform   (Transform(Transform))
import ChessExample.System.World          (World)
import ChessExample.Vulkan.Renderer       (Renderer(..), recreateRenderer)
import ChessExample.Vulkan.Setup          (RenderSetup(..))
import Effectful.Extra                    (type (<:))

-- The artist system renders the chess scene on the screen.

-- Renders the entities of the world.
render
  :: ( es <: IOE
     , es <: ECS World
     , es <: Log Swapchain
     , es <: Memory a k
     , es <: Resource
     , es <: State FrameIndex
     )
  => Hk.MemoryAllocator a k -> Renderer -> Eff es Renderer
render allocator renderer@Renderer{..} = do
  (screen, e) <- get global
  if
    -- in case of a window resize, we recreate the renderer.
    | screen.dirty -> do
        set e $ screen { dirty = False }
        recreateRenderer allocator renderer
    -- in case of a minimized window (i.e., we have no swapchain), we do not render.
    | null swapchain ->
        pure renderer
    -- otherwise: let's try rendering ("try" because it may fail: there may be no
    -- frame ready to be rendered to).
    | otherwise -> do
        -- 1. Get the next frame and its associated data.
        maybeFrame <- Hk.getNextFrame swapchain virtualFrames
        case maybeFrame of
          Nothing ->
            recreateRenderer allocator renderer
          Just frame -> do
            -- 2. If we got a frame, render into it.
            Hk.record
              frame.commandBuffer
              Vk.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT $ do
                Hk.presentToDrawBarrier
                  setup.pQueue.familyIndex
                  setup.gQueue.familyIndex
                  frame.image
                renderScene renderer frame
                Hk.drawToPresentBarrier
                  setup.gQueue.familyIndex
                  setup.pQueue.familyIndex
                  frame.image
            resetFence setup.device frame.renderFence
            submitOne setup.gQueue frame.renderFence $
              Vk.zero
                { Vk.waitSemaphores   = [frame.acquireSemaphore]
                , Vk.waitDstStageMask = [Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT]
                , Vk.commandBuffers   = [frame.commandBuffer.commandBufferHandle]
                , Vk.signalSemaphores = [frame.renderSemaphore]
                }
            -- 3. Present the frame.
            result <-
              Vk.queuePresentKHR setup.pQueue.handle $
                Vk.zero
                  { Vk.waitSemaphores = [frame.renderSemaphore]
                  , Vk.swapchains     = [swapchain.handle]
                  , Vk.imageIndices   = [fromIntegral frame.imageIndex]
                  }
            -- 4. Check if presentation was successful: it is possible that the
            -- current swapchain is not fully compatible with the current Vulkan
            -- surface anymore, in which case we recreate the renderer.
            case result of
              Vk.SUCCESS        -> pure renderer
              Vk.SUBOPTIMAL_KHR -> recreateRenderer allocator renderer
              errorResult       -> throwVk errorResult

-- Helper function which does the actual rendering of the 3D objects.
renderScene
  :: ( es <: IOE
     , es <: Reader Vk.CommandBuffer
     , es <: ECS World
     )
  => Renderer -> Frame -> Eff es ()
renderScene Renderer{..} frame =
  cmapM_ $ \(Camera camera _, environment :: Environment) -> do
    let
      -- extract needed data from the frame.
      framebuffer    = framebuffers  V.! frame.imageIndex
      cameraBuffer   = cameraBuffers V.! frame.virtualIndex
      cameraSet      = cameraSets    V.! frame.virtualIndex
      skyboxBuffer   = skyboxBuffers V.! frame.virtualIndex
      skyboxSet      = skyboxSets    V.! frame.virtualIndex
      -- calculate needed matrices for vertex transformations.
      projection     = projectionMatrix camera
      view           = viewMatrix camera
      skyboxView     = setTranslation 0 view
      projView       = projection `multiply` view
      skyboxProjView = projection `multiply` skyboxView
    -- copy the matrices into the uniform buffers. We could optimize this of
    -- course (like copying them only if the camera is marked dirty, etc.).
    poke cameraBuffer (ColumnMajor projView)
    poke skyboxBuffer (ColumnMajor skyboxProjView)
    let
      -- initialize the render pass attachments. Note that in contrast to many
      -- Vulkan tutorials we initialize the depth to 0 instead of 1 because we
      -- are going to do reverse depth.
      black = Vk.Color $ Vk.Float32 0 0 0 1
      depth = Vk.DepthStencil $ Vk.ClearDepthStencilValue 0 0
    Hk.beginRenderPass Vk.SUBPASS_CONTENTS_INLINE $
      Vk.zero
        { Vk.renderPass  = renderPass.handle
        , Vk.framebuffer = framebuffer
        , Vk.renderArea  = toRect2D swapchain
        , Vk.clearValues = [black, black, depth]
        }
    Hk.setViewport 0 [toViewport 0 1 swapchain]
    Hk.setScissor 0 [toRect2D swapchain]
    -- bind the graphics pipeline for rendering the chess board and pieces.
    Hk.bindGraphicsPipeline chessPipeline
    Hk.bindDescriptorSets
      Vk.PIPELINE_BIND_POINT_GRAPHICS
      pipelineLayout
      0
      [cameraSet, environment.textureSet]
      []
    let
      -- focus is converted to an integer and pushed to the shaders where the
      -- coloring happens depending on the integer value.
      toInt = \case
        Nothing         -> 0
        Just Selected   -> 1
        Just Threatened -> 2
        Just Lost       -> 3
      -- a mesh is rendered by binding its buffers and pushing the texture index
      -- and focus (as integer) to the shaders.
      renderMesh mesh focus =
        V.forM_ mesh.primitives $ \Primitive{..} -> do
          let Geometry{..} = geometry
          case material of
            Nothing ->
              Hk.bindVertexBuffers 0 [vertexBuffer] [0]
            Just mat -> do
              Hk.bindVertexBuffers
                0
                [vertexBuffer, normalBuffer, mat.texCoordBuffer]
                [0, 0, 0]
              Hk.pushStorableConstants
                pipelineLayout
                Vk.SHADER_STAGE_FRAGMENT_BIT
                (sizeOf @(ColumnMajor Mat4))
                [mat.texIndex, toInt focus]
          case indexInfo of
            Nothing ->
              Hk.draw vertexCount 1 0 0
            Just (buffer, count, ixType) -> do
              Hk.bindIndexBuffer buffer 0 ixType
              Hk.drawIndexed count 1 0 0 0
    -- every entity that has a mesh and a 3D position will be rendered.
    cmapM_ $ \(mesh :: Mesh, Transform t r s, e) -> do
      focus <- tryGet e
      -- model matrix is passed to shaders via push constant, other matrices
      -- via uniform buffers (see above).
      Hk.pushStorableConstants
          pipelineLayout
          Vk.SHADER_STAGE_VERTEX_BIT
          0
          [ColumnMajor $ fromTRS t r s]
      renderMesh mesh focus
    -- bind the graphics pipeline for rendering the background (skybox).
    Hk.bindGraphicsPipeline skyboxPipeline
    Hk.bindDescriptorSets
      Vk.PIPELINE_BIND_POINT_GRAPHICS
      pipelineLayout
      0
      [skyboxSet, environment.skyboxSet]
      []
    renderMesh environment.skyboxMesh Nothing
    Hk.endRenderPass
