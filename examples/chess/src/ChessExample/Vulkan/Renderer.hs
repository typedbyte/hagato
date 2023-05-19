{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
module ChessExample.Vulkan.Renderer
  ( Renderer(..)
  , allocateRenderer
  , recreateRenderer
  ) where

-- base
import Data.Bits       ((.|.))
import Data.Word       (Word32)
import Foreign         (Ptr)
import Foreign.C.Types (CInt)
import Prelude hiding  (log, null)

-- effectful-core
import Effectful                     (Eff, IOE)
import Effectful.Writer.Static.Local (runWriter, tell)

-- hagato:with-core
import Hagato.Core (ColumnMajor(..), Mat4, sizeOf)

-- hagato:with-core-effectful
import Hagato.Core.Effectful (Log, LogLevel(Debug), log)

-- hagato:with-vulkan
import Hagato.Vulkan hiding (get)

-- hagato:with-vulkan-effectful
import Hagato.Vulkan.Effectful        qualified as Hk
import Hagato.Vulkan.Effectful.Memory (Memory)

-- resource-effectful
import Effectful.Resource (Key, Resource, freeAll)

-- vector
import Data.Vector qualified as V

-- vulkan
import Vulkan                       qualified as Vk
import Vulkan.Core10.Pass           qualified as Vp
import Vulkan.Core10.Pipeline       qualified as Vp
import Vulkan.Core10.PipelineLayout qualified as Vp
import Vulkan.CStruct.Extends       qualified as Vk
import Vulkan.Zero                  qualified as Vk

import ChessExample.Vulkan.Setup (RenderSetup(..))
import Effectful.Extra           (type (<:))

-- The renderer holds all Vulkan-related resources needed for rendering the chess
-- scene. The actual rendering is done by the artist system. The keys stored in the
-- renderer are used to freeing GPU resources manually, which is necessary when
-- recreating the renderer on window resize. Note that we use a self-defined amount
-- of frames ("virtual frames") instead of the ones given by the hardware in order
-- to keep the amount of rendering resources independent of the concrete hardware setup.
data Renderer = Renderer
  { setup            :: RenderSetup
  , swapchain        :: Swapchain
  , swapchainKey     :: Key
  , sampleCount      :: Vk.SampleCountFlagBits
  , renderPass       :: RenderPass
  , renderPassKey    :: Key
  , framebuffers     :: V.Vector Vk.Framebuffer
  , framebufferKeys  :: V.Vector Key
  , virtualFrames    :: V.Vector VirtualFrame
  , virtualFrameKeys :: V.Vector Key
  , pipelineLayout   :: Vk.PipelineLayout
  , chessPipeline    :: Vk.Pipeline
  , skyboxPipeline   :: Vk.Pipeline
  , setLayout        :: Vk.DescriptorSetLayout
  , setAllocator     :: DescriptorSetAllocator
  , cameraBuffers    :: V.Vector (Ptr (ColumnMajor Mat4))
  , cameraSets       :: V.Vector Vk.DescriptorSet
  , skyboxBuffers    :: V.Vector (Ptr (ColumnMajor Mat4))
  , skyboxSets       :: V.Vector Vk.DescriptorSet
  }

-- Creates the renderer. In order to thoroughly understand all parts of this function,
-- you will need to understand the Vulkan API. Only highlights are commented below.
allocateRenderer
  :: ( es <: IOE
     , es <: Memory a k
     , es <: Resource
     )
  => RenderSetup
  -> Hk.MemoryAllocator a k
  -> Word32
  -> Eff es Renderer
allocateRenderer setup@RenderSetup{..} allocator frameCount = do
  (swapchain, swapchainKey) <-
    Hk.allocateSwapchain surface physicalDevice device [gQueue, pQueue] $
      preferImageCount frameCount
  sampleCount <- getMaxSampleCount physicalDevice
  (renderPass, renderPassKey) <-
    Hk.allocateRenderPass swapchain $ \s -> do
      sample <- defineAttachment $
        Vk.zero
          { Vp.format         = s.format
          , Vp.samples        = sampleCount
          , Vp.loadOp         = Vk.ATTACHMENT_LOAD_OP_CLEAR
          , Vp.storeOp        = Vk.ATTACHMENT_STORE_OP_STORE
          , Vp.stencilLoadOp  = Vk.ATTACHMENT_LOAD_OP_DONT_CARE
          , Vp.stencilStoreOp = Vk.ATTACHMENT_STORE_OP_DONT_CARE
          , Vp.initialLayout  = Vk.IMAGE_LAYOUT_UNDEFINED
          , Vp.finalLayout    = Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
          }
      color <- defineAttachment $
        Vk.zero
          { Vp.format         = s.format
          , Vp.samples        = Vk.SAMPLE_COUNT_1_BIT
          , Vp.loadOp         = Vk.ATTACHMENT_LOAD_OP_CLEAR
          , Vp.storeOp        = Vk.ATTACHMENT_STORE_OP_STORE
          , Vp.stencilLoadOp  = Vk.ATTACHMENT_LOAD_OP_DONT_CARE
          , Vp.stencilStoreOp = Vk.ATTACHMENT_STORE_OP_DONT_CARE
          , Vp.initialLayout  = Vk.IMAGE_LAYOUT_UNDEFINED
          , Vp.finalLayout    = Vk.IMAGE_LAYOUT_PRESENT_SRC_KHR
          }
      depth <- defineAttachment $
        Vk.zero
          { Vp.format         = Vk.FORMAT_D32_SFLOAT
          , Vp.samples        = sampleCount
          , Vp.loadOp         = Vk.ATTACHMENT_LOAD_OP_CLEAR
          , Vp.storeOp        = Vk.ATTACHMENT_STORE_OP_DONT_CARE
          , Vp.stencilLoadOp  = Vk.ATTACHMENT_LOAD_OP_DONT_CARE
          , Vp.stencilStoreOp = Vk.ATTACHMENT_STORE_OP_DONT_CARE
          , Vp.initialLayout  = Vk.IMAGE_LAYOUT_UNDEFINED
          , Vp.finalLayout    = Vk.IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
          }
      subpass <- defineSubpass $
        Vk.zero
          { Vp.pipelineBindPoint =
              Vk.PIPELINE_BIND_POINT_GRAPHICS
          , Vp.colorAttachments =
              [sample `withLayout` Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL]
          , Vp.resolveAttachments =
              [color `withLayout` Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL]
          , Vp.depthStencilAttachment =
              Just $ depth `withLayout` Vk.IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
          }
      defineDependency $
        Vk.zero
          { Vp.srcSubpass      = Vk.SUBPASS_EXTERNAL
          , Vp.dstSubpass      = subpass.index
          , Vp.srcStageMask    = Vk.PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT
          , Vp.dstStageMask    = Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
          , Vp.srcAccessMask   = Vk.ACCESS_MEMORY_READ_BIT
          , Vp.dstAccessMask   = Vk.ACCESS_COLOR_ATTACHMENT_WRITE_BIT
          , Vp.dependencyFlags = Vk.DEPENDENCY_BY_REGION_BIT
          }
      defineDependency $
        Vk.zero
          { Vp.srcSubpass      = Vk.SUBPASS_EXTERNAL
          , Vp.dstSubpass      = subpass.index
          , Vp.srcStageMask    = Vk.PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT
                             .|. Vk.PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT
          , Vp.dstStageMask    = Vk.PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT
                             .|. Vk.PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT
          , Vp.dstAccessMask   = Vk.ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT
          , Vp.dependencyFlags = Vk.DEPENDENCY_BY_REGION_BIT
          }
      defineDependency $
        Vk.zero
          { Vp.srcSubpass      = subpass.index
          , Vp.dstSubpass      = Vk.SUBPASS_EXTERNAL
          , Vp.srcStageMask    = Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
          , Vp.dstStageMask    = Vk.PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT
          , Vp.srcAccessMask   = Vk.ACCESS_COLOR_ATTACHMENT_WRITE_BIT
          , Vp.dstAccessMask   = Vk.ACCESS_MEMORY_READ_BIT
          , Vp.dependencyFlags = Vk.DEPENDENCY_BY_REGION_BIT
          }
  (framebuffers, framebufferKeys) <-
    allocateFramebuffers swapchain renderPass allocator sampleCount
  setLayout <-
    Hk.manageDescriptorSetLayout device $
      Vk.zero
        { Vk.next =
            ( -- here we indicate that we want to have dynamic texture arrays
              Vk.zero
                { Vk.bindingFlags =
                    [ Vk.zero
                    , Vk.DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT
                    ]
                }
            , ())
        , Vk.bindings =
            [ Vk.zero
                { Vk.binding         = 0
                , Vk.descriptorType  = Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER
                , Vk.descriptorCount = 1
                , Vk.stageFlags      = Vk.SHADER_STAGE_VERTEX_BIT
                }
            , Vk.zero
                { Vk.binding         = 1
                , Vk.descriptorType  = Vk.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
                , Vk.descriptorCount = 15 -- but we still need a texture count upper limit
                , Vk.stageFlags      = Vk.SHADER_STAGE_FRAGMENT_BIT
                }
            ]
        }
  setAllocator <-
    Hk.manageDescriptorSetAllocator device 15
      [ (Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER, 1.0)
      , (Vk.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, 1.0)
      ]
  pipelineLayout <-
    Hk.managePipelineLayout device $
      Vk.zero
        { Vp.setLayouts = [setLayout, setLayout]
        , Vp.pushConstantRanges =
            [ -- model matrix
              Vk.zero
                { Vp.stageFlags = Vk.SHADER_STAGE_VERTEX_BIT
                , Vp.offset     = 0
                , Vp.size       = sizeOf @(ColumnMajor Mat4)
                }
              -- one int for texure index, one for focus
            , Vk.zero
                { Vp.stageFlags = Vk.SHADER_STAGE_FRAGMENT_BIT
                , Vp.offset     = sizeOf @(ColumnMajor Mat4)
                , Vp.size       = 2 * sizeOf @CInt
                }
            ]
        }
  pipelines <-
    withShaderFile "examples/chess/shaders/chess.vert.spv" device $ \chessVertShader ->
    withShaderFile "examples/chess/shaders/chess.frag.spv" device $ \chessFragShader ->
    withShaderFile "examples/chess/shaders/cube.vert.spv"  device $ \cubeVertShader ->
    withShaderFile "examples/chess/shaders/cube.frag.spv"  device $ \cubeFragShader ->
      Hk.manageGraphicsPipelines device Vk.NULL_HANDLE $
        [ Vk.SomeStruct $
            Vk.zero
              { Vp.stages = V.map Vk.SomeStruct
                  [ Vk.zero
                      { Vp.stage   = Vp.SHADER_STAGE_VERTEX_BIT
                      , Vp.module' = chessVertShader
                      , Vp.name    = "main"
                      }
                  , Vk.zero
                      { Vp.stage   = Vp.SHADER_STAGE_FRAGMENT_BIT
                      , Vp.module' = chessFragShader
                      , Vp.name    = "main"
                      }
                  ]
              , Vp.vertexInputState = Just $ Vk.SomeStruct $
                  Vk.zero
                    { Vp.vertexBindingDescriptions =
                        [ Vk.zero
                            { Vp.binding   = 0
                            , Vp.stride    = 3 * sizeOf @Float -- (x,y,z) position
                            , Vp.inputRate = Vp.VERTEX_INPUT_RATE_VERTEX
                            }
                        , Vk.zero
                            { Vp.binding   = 1
                            , Vp.stride    = 3 * sizeOf @Float -- (x,y,z) normal
                            , Vp.inputRate = Vp.VERTEX_INPUT_RATE_VERTEX
                            }
                        , Vk.zero
                            { Vp.binding   = 2
                            , Vp.stride    = 2 * sizeOf @Float -- (u,v) tex coord
                            , Vp.inputRate = Vp.VERTEX_INPUT_RATE_VERTEX
                            }
                        ]
                    , Vp.vertexAttributeDescriptions =
                        [ Vk.zero
                            { Vp.location = 0
                            , Vp.binding  = 0
                            , Vp.format   = Vk.FORMAT_R32G32B32_SFLOAT
                            , Vp.offset   = 0
                            }
                        , Vk.zero
                            { Vp.location = 1
                            , Vp.binding  = 1
                            , Vp.format   = Vk.FORMAT_R32G32B32_SFLOAT
                            , Vp.offset   = 0
                            }
                        , Vk.zero
                            { Vp.location = 2
                            , Vp.binding  = 2
                            , Vp.format   = Vk.FORMAT_R32G32_SFLOAT
                            , Vp.offset   = 0
                            }
                        ]
                    }
              , Vp.inputAssemblyState = Just $
                  Vk.zero
                    { Vp.topology = Vp.PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
                    }
              , Vp.viewportState = Just $ Vk.SomeStruct $
                  Vk.zero
                    { Vp.viewportCount = 1
                    , Vp.scissorCount  = 1
                    }
              , Vp.rasterizationState = Just $ Vk.SomeStruct $
                  Vk.zero
                    { Vp.depthClampEnable        = False
                    , Vp.rasterizerDiscardEnable = False
                    , Vp.polygonMode             = Vp.POLYGON_MODE_FILL
                    , Vp.cullMode                = Vp.CULL_MODE_BACK_BIT
                    , Vp.frontFace               = Vp.FRONT_FACE_COUNTER_CLOCKWISE
                    , Vp.depthBiasEnable         = False
                    , Vp.lineWidth               = 1
                    }
              , Vp.multisampleState = Just $ Vk.SomeStruct $
                  Vk.zero
                    { Vp.rasterizationSamples  = sampleCount
                    , Vp.sampleShadingEnable   = False
                    , Vp.minSampleShading      = 1
                    , Vp.alphaToCoverageEnable = False
                    , Vp.alphaToOneEnable      = False
                    }
              , Vp.depthStencilState = Just $
                  Vk.zero
                    { Vp.depthTestEnable  = True
                    , Vp.depthWriteEnable = True
                    , Vp.depthCompareOp   = Vp.COMPARE_OP_GREATER_OR_EQUAL
                    }
              , Vp.colorBlendState = Just $ Vk.SomeStruct $
                  Vk.zero
                    { Vp.logicOpEnable  = False
                    , Vp.logicOp        = Vp.LOGIC_OP_COPY
                    , Vp.blendConstants = (0, 0, 0, 0)
                    , Vp.attachments    =
                        [ Vk.zero
                            { Vp.blendEnable         = False
                            , Vp.srcColorBlendFactor = Vp.BLEND_FACTOR_ONE
                            , Vp.dstColorBlendFactor = Vp.BLEND_FACTOR_ZERO
                            , Vp.colorBlendOp        = Vp.BLEND_OP_ADD
                            , Vp.srcAlphaBlendFactor = Vp.BLEND_FACTOR_ONE
                            , Vp.dstAlphaBlendFactor = Vp.BLEND_FACTOR_ZERO
                            , Vp.alphaBlendOp        = Vp.BLEND_OP_ADD
                            , Vp.colorWriteMask      = Vp.COLOR_COMPONENT_R_BIT
                                                   .|. Vp.COLOR_COMPONENT_G_BIT
                                                   .|. Vp.COLOR_COMPONENT_B_BIT
                                                   .|. Vp.COLOR_COMPONENT_A_BIT
                            }
                        ]
                    }
              , Vp.dynamicState = Just $
                  Vk.zero
                    { Vp.dynamicStates =
                        [ Vp.DYNAMIC_STATE_VIEWPORT
                        , Vp.DYNAMIC_STATE_SCISSOR
                        ]
                    }
              , Vp.layout     = pipelineLayout
              , Vp.renderPass = renderPass.handle
              , Vp.subpass    = 0
              }
        , Vk.SomeStruct $
            Vk.zero
              { Vp.stages = V.map Vk.SomeStruct
                  [ Vk.zero
                      { Vp.stage   = Vp.SHADER_STAGE_VERTEX_BIT
                      , Vp.module' = cubeVertShader
                      , Vp.name    = "main"
                      }
                  , Vk.zero
                      { Vp.stage   = Vp.SHADER_STAGE_FRAGMENT_BIT
                      , Vp.module' = cubeFragShader
                      , Vp.name    = "main"
                      }
                  ]
              , Vp.vertexInputState = Just $ Vk.SomeStruct $
                  Vk.zero
                    { Vp.vertexBindingDescriptions =
                        [ Vk.zero
                            { Vp.binding   = 0
                            , Vp.stride    = 3 * sizeOf @Float -- (x,y,z) position
                            , Vp.inputRate = Vp.VERTEX_INPUT_RATE_VERTEX
                            }
                        ]
                    , Vp.vertexAttributeDescriptions =
                        [ Vk.zero
                            { Vp.location = 0
                            , Vp.binding  = 0
                            , Vp.format   = Vk.FORMAT_R32G32B32_SFLOAT
                            , Vp.offset   = 0
                            }
                        ]
                    }
              , Vp.inputAssemblyState = Just $
                  Vk.zero
                    { Vp.topology = Vp.PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
                    }
              , Vp.viewportState = Just $ Vk.SomeStruct $
                  Vk.zero
                    { Vp.viewportCount = 1
                    , Vp.scissorCount  = 1
                    }
              , Vp.rasterizationState = Just $ Vk.SomeStruct $
                  Vk.zero
                    { Vp.depthClampEnable        = False
                    , Vp.rasterizerDiscardEnable = False
                    , Vp.polygonMode             = Vp.POLYGON_MODE_FILL
                    , Vp.cullMode                = Vp.CULL_MODE_BACK_BIT
                    , Vp.frontFace               = Vp.FRONT_FACE_CLOCKWISE
                    , Vp.depthBiasEnable         = False
                    , Vp.lineWidth               = 1
                    }
              , Vp.multisampleState = Just $ Vk.SomeStruct $
                  Vk.zero
                    { Vp.rasterizationSamples  = sampleCount
                    , Vp.sampleShadingEnable   = False
                    , Vp.minSampleShading      = 1
                    , Vp.alphaToCoverageEnable = False
                    , Vp.alphaToOneEnable      = False
                    }
              , Vp.depthStencilState = Just $
                  Vk.zero
                    { Vp.depthTestEnable  = True
                    , Vp.depthWriteEnable = True
                    , Vp.depthCompareOp   = Vp.COMPARE_OP_GREATER_OR_EQUAL
                    }
              , Vp.colorBlendState = Just $ Vk.SomeStruct $
                  Vk.zero
                    { Vp.logicOpEnable  = False
                    , Vp.logicOp        = Vp.LOGIC_OP_COPY
                    , Vp.blendConstants = (0, 0, 0, 0)
                    , Vp.attachments    =
                        [ Vk.zero
                            { Vp.blendEnable         = False
                            , Vp.srcColorBlendFactor = Vp.BLEND_FACTOR_ONE
                            , Vp.dstColorBlendFactor = Vp.BLEND_FACTOR_ZERO
                            , Vp.colorBlendOp        = Vp.BLEND_OP_ADD
                            , Vp.srcAlphaBlendFactor = Vp.BLEND_FACTOR_ONE
                            , Vp.dstAlphaBlendFactor = Vp.BLEND_FACTOR_ZERO
                            , Vp.alphaBlendOp        = Vp.BLEND_OP_ADD
                            , Vp.colorWriteMask      = Vp.COLOR_COMPONENT_R_BIT
                                                   .|. Vp.COLOR_COMPONENT_G_BIT
                                                   .|. Vp.COLOR_COMPONENT_B_BIT
                                                   .|. Vp.COLOR_COMPONENT_A_BIT
                            }
                        ]
                    }
              , Vp.dynamicState = Just $
                  Vk.zero
                    { Vp.dynamicStates =
                        [ Vp.DYNAMIC_STATE_VIEWPORT
                        , Vp.DYNAMIC_STATE_SCISSOR
                        ]
                    }
              , Vp.layout     = pipelineLayout
              , Vp.renderPass = renderPass.handle
              , Vp.subpass    = 0
              }
        ]
  let
    chessPipeline  = pipelines V.! 0
    skyboxPipeline = pipelines V.! 1
  (virtualFrames, virtualFrameKeys) <-
    Hk.allocateVirtualFrames device gCommandPool frameCount
  let iFrameCount = fromIntegral frameCount
  (buffers, sets) <-
    fmap V.unzip $
      -- for every virtual frame, we have ...
      -- a) one uniform buffer for the projection matrix
      -- b) one descriptor set pointing to the uniform buffer
      -- ... and everything twice because we need these resources for both the
      -- world and the skybox.
      V.replicateM (2 * iFrameCount) $ do
        buffer <-
          Hk.manageBuffer device $
            Vk.zero
              { Vk.size  = sizeOf @(ColumnMajor Mat4)
              , Vk.usage = Vk.BUFFER_USAGE_UNIFORM_BUFFER_BIT
              }
        memory <-
          Hk.manageBoundBufferMemory allocator buffer
              $ Vk.MEMORY_PROPERTY_HOST_VISIBLE_BIT
            .|. Vk.MEMORY_PROPERTY_HOST_COHERENT_BIT
        ptr <-
          Hk.manageMappedBuffer allocator memory
        set <-
          Hk.allocateDescriptorSet setAllocator setLayout
        Vk.updateDescriptorSets
          device
          [ Vk.SomeStruct $
              Vk.zero
                { Vk.dstSet          = set
                , Vk.dstBinding      = 0
                , Vk.descriptorCount = 1
                , Vk.descriptorType  = Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER
                , Vk.bufferInfo      =
                    [ Vk.DescriptorBufferInfo 
                        { Vk.buffer = buffer
                        , Vk.offset = 0
                        , Vk.range  = Vk.WHOLE_SIZE
                        }
                    ]
                }
          ]
          []
        pure (ptr, set)
  let
    cameraBuffers = V.take iFrameCount buffers
    cameraSets    = V.take iFrameCount sets
    skyboxBuffers = V.drop iFrameCount buffers
    skyboxSets    = V.drop iFrameCount sets
  pure $ Renderer{..}

-- Creates framebuffers per swapchain image. They can be destroyed manually using
-- the returned keys.
allocateFramebuffers
  :: ( es <: IOE
     , es <: Memory a k
     , es <: Resource
     )
  => Swapchain
  -> RenderPass
  -> Hk.MemoryAllocator a k
  -> Vk.SampleCountFlagBits
  -> Eff es (V.Vector Vk.Framebuffer, V.Vector Key)
allocateFramebuffers swapchain renderPass allocator sampleCount =
  runWriter $
    V.forM swapchain.images $ \image -> do
      let width  = swapchain.extent.width
      let height = swapchain.extent.height
      (sampleImage, siKey) <-
        Hk.allocateImage swapchain.device $
          Vk.zero
            { Vk.imageType     = Vk.IMAGE_TYPE_2D
            , Vk.format        = swapchain.format
            , Vk.extent        = Vk.Extent3D width height 1
            , Vk.mipLevels     = 1
            , Vk.arrayLayers   = 1
            , Vk.samples       = sampleCount
            , Vk.tiling        = Vk.IMAGE_TILING_OPTIMAL
            , Vk.sharingMode   = Vk.SHARING_MODE_EXCLUSIVE
            , Vk.initialLayout = Vk.IMAGE_LAYOUT_UNDEFINED
            , Vk.usage         = Vk.IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT
                             .|. Vk.IMAGE_USAGE_COLOR_ATTACHMENT_BIT
            }
      (_, smKey) <-
        Hk.allocateBoundImageMemory
          allocator
          sampleImage
          Vk.MEMORY_PROPERTY_DEVICE_LOCAL_BIT
      (sampleImageView, svKey) <-
        Hk.allocateImageView swapchain.device $
          Vk.zero
            { Vk.image    = sampleImage
            , Vk.viewType = Vk.IMAGE_VIEW_TYPE_2D
            , Vk.format   = swapchain.format
            , Vk.subresourceRange =
                Vk.zero
                  { Vk.aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT
                  , Vk.levelCount = 1
                  , Vk.layerCount = 1
                  }
            }
      (depthImage, diKey) <-
        Hk.allocateImage swapchain.device $
          Vk.zero
            { Vk.imageType     = Vk.IMAGE_TYPE_2D
            , Vk.format        = Vk.FORMAT_D32_SFLOAT
            , Vk.extent        = Vk.Extent3D width height 1
            , Vk.mipLevels     = 1
            , Vk.arrayLayers   = 1
            , Vk.samples       = sampleCount
            , Vk.tiling        = Vk.IMAGE_TILING_OPTIMAL
            , Vk.sharingMode   = Vk.SHARING_MODE_EXCLUSIVE
            , Vk.initialLayout = Vk.IMAGE_LAYOUT_UNDEFINED
            , Vk.usage         = Vk.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT
            }
      (_, dmKey) <-
        Hk.allocateBoundImageMemory
          allocator
          depthImage
          Vk.MEMORY_PROPERTY_DEVICE_LOCAL_BIT
      (depthImageView, dvKey) <-
        Hk.allocateImageView swapchain.device $
          Vk.zero
            { Vk.image    = depthImage
            , Vk.viewType = Vk.IMAGE_VIEW_TYPE_2D
            , Vk.format   = Vk.FORMAT_D32_SFLOAT
            , Vk.subresourceRange =
                Vk.zero
                  { Vk.aspectMask = Vk.IMAGE_ASPECT_DEPTH_BIT
                  , Vk.levelCount = 1
                  , Vk.layerCount = 1
                  }
            }
      (swapImageView, sivKey) <-
        Hk.allocateImageView swapchain.device $
          Vk.zero
            { Vk.image    = image
            , Vk.viewType = Vk.IMAGE_VIEW_TYPE_2D
            , Vk.format   = swapchain.format
            , Vk.subresourceRange =
                Vk.zero
                  { Vk.aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT
                  , Vk.levelCount = 1
                  , Vk.layerCount = 1
                  }
            }
      (framebuffer, fKey) <-
        Hk.allocateFramebuffer swapchain.device $
          Vk.zero
            { Vk.renderPass  = renderPass.handle
            , Vk.attachments = [sampleImageView, swapImageView, depthImageView]
            , Vk.width       = width
            , Vk.height      = height
            , Vk.layers      = 1
            }
      tell $ V.fromListN 8 [siKey, smKey, svKey, diKey, dmKey, dvKey, sivKey, fKey]
      pure framebuffer

-- Recreating the renderer on window resize means destroying the keyed resources
-- manually and allocating new resources, either by reallocating or using specialized
-- recreate*-functions provided by hagato.
recreateRenderer
  :: ( es <: IOE
     , es <: Log Swapchain
     , es <: Memory a k
     , es <: Resource
     )
  => Hk.MemoryAllocator a k -> Renderer -> Eff es Renderer
recreateRenderer allocator renderer@Renderer{..} = do
  Vk.deviceWaitIdle setup.device
  freeAll framebufferKeys
  (newSwapchain, sKey) <-
    Hk.recreateSwapchain swapchain swapchainKey
  (newRenderPass, rKey) <-
    Hk.recreateRenderPass newSwapchain renderPass renderPassKey
  (newFramebuffers, fbKeys) <-
    allocateFramebuffers newSwapchain newRenderPass allocator sampleCount
  (newFrames, fKeys) <-
    Hk.recreateVirtualFrames setup.device virtualFrames virtualFrameKeys
  log Debug newSwapchain
  pure $
    renderer
      { swapchain        = newSwapchain
      , swapchainKey     = sKey
      , renderPass       = newRenderPass
      , renderPassKey    = rKey
      , framebuffers     = newFramebuffers
      , framebufferKeys  = fbKeys
      , virtualFrames    = newFrames
      , virtualFrameKeys = fKeys
      }