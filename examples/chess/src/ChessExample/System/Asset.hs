{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
module ChessExample.System.Asset (loadScene) where

-- apecs-effectful
import Apecs.Effectful (ECS, global, newEntity, newEntity_, set)

-- base
import Control.Monad (forM)
import Data.Bits     ((.|.))
import Data.Char     (digitToInt, ord)
import Data.Coerce   (Coercible, coerce)
import Data.Foldable (foldrM)
import Foreign.Ptr   (plusPtr)

-- bytestring
import Data.ByteString qualified as BS

-- chessica
import Chess qualified as Chess
import Chess (Piece, Some(Some))

-- containers
import Data.Map.Strict qualified as M

-- effectful-core
import Effectful      (Eff, IOE, MonadIO, liftIO)
import Effectful.Fail (Fail, runFailIO)

-- hagato:with-core
import Hagato.Core (Frustum(..), Vec2(Vec2), Vec3(Vec3), toTRS)
import Hagato.Core qualified as Core

-- hagato:with-gltf-effectful
import Hagato.GLTF.Effectful qualified as GLTF

-- hagato:with-gltf-vulkan
import Hagato.GLTF.Vulkan (toIndexType, toSamplerInfo)

-- hagato:with-vulkan
import Hagato.Vulkan qualified as Vk

-- hagato:with-vulkan-effectful
import Hagato.Vulkan.Effectful qualified as Vk

-- JuicyPixels
import Codec.Picture qualified as P

-- resource-effectful
import Effectful.Resource (Resource, withRegion)

-- text
import Data.Text (Text, pack)

-- vector
import Data.Vector qualified as V

-- vulkan
import Vulkan                              qualified as Vu
import Vulkan.Core10.CommandBufferBuilding qualified as Vb
import Vulkan.Core10.OtherTypes            qualified as Vo

import ChessExample.Component.Camera      (Camera(Camera))
import ChessExample.Component.Environment (Environment(Environment))
import ChessExample.Component.Index       (emptyIndex, insertField)
import ChessExample.Component.Mesh        (Geometry(..), Material(..), Mesh(Mesh), MeshFactory, Primitive(..))
import ChessExample.Component.Screen      (Screen(Screen))
import ChessExample.Component.Transform   (Transform(Transform))
import ChessExample.System.World          (World)
import ChessExample.Vulkan.Renderer       (Renderer(..))
import ChessExample.Vulkan.Setup          (RenderSetup(..))
import Effectful.Extra                    (type (<:))

-- The asset system loads 3D models. In this example, this is done by importing
-- GLB files, which can be exported from Blender, for example.

-- When loading objects from a GLB file, some of them are static (they never
-- move and always exist, like the chess board), some of them are dynamic (they
-- are not fixed, are instantiated and can move, like the chess pieces), and
-- some of them can be targeted (the chess board fields in this example).
data ObjectType
  = Static
  | Target Chess.Position
  | Dynamic (Some Piece)

-- A lookup list which corresponds names of 3D objects in the GLB file with their
-- object types. The names are known because they are modelled this way in Blender.
objects :: [(Text, ObjectType)]
objects =
  -- board
  [("Board"  , Static)] ++
  -- labels
  [([c,'-',l], Static) | c <- ['B','W'], l <- ['1'..'8'] ++ ['a'..'h']] ++
  -- fields
  [ ([c,r], Target position)
    | c <- ['A'..'H']
    , r <- ['1'..'8']
    , let position = Chess.boundedPosition (digitToInt r - 1) (ord c - ord 'A')
  ] ++
  -- pieces
  [ (pack $ c:t, Dynamic piece)
    | t <- ["Pawn1", "RookLeft", "KnightLeft", "BishopLeft", "Queen", "King"]
    , c <- ['W','B']
    , let type' = toType t
    , let color = toColor c
    , let piece = Chess.fromSome type' color
  ]
  where
    toType = \case
      "RookLeft"   -> Some Chess.Rook
      "KnightLeft" -> Some Chess.Knight
      "BishopLeft" -> Some Chess.Bishop
      "Queen"      -> Some Chess.Queen
      "King"       -> Some Chess.King
      "Pawn1"      -> Some Chess.Pawn
      _            -> error "Unknown piece type"
    toColor = \case
      'W' -> Chess.White
      'B' -> Chess.Black
      _   -> error "Unknown color"

-- Loads the chess game scene which was exported from Blender using GLB files.
loadScene
  :: ( es <: ECS World
     , es <: IOE
     , es <: Vk.Memory a k
     , es <: Resource
     )
  => Renderer -> Vk.MemoryAllocator a k -> Float -> Float -> Eff es MeshFactory
loadScene Renderer{setup, setAllocator, setLayout} allocator width height =
  -- get a command buffer to perform memory transfers
  Vk.withCommandBuffers setup.device
    ( Vk.zero
        { Vk.commandPool        = setup.tCommandPool
        , Vk.level              = Vk.COMMAND_BUFFER_LEVEL_PRIMARY
        , Vk.commandBufferCount = 1
        }
    )
    $ \tBuffers -> runFailIO $ do
        let transferBuffer = tBuffers V.! 0
        -- load background/skybox asset
        skyboxMesh <-
          GLTF.runGLTF (GLTF.FromFile "examples/chess/assets/cube.glb") $ do
            node     <- GLTF.getNode "Cube"
            meshIx   <- GLTF.fromMaybe "Cube Mesh" node.mesh
            meshGltf <- GLTF.fetch meshIx
            fmap Mesh $
              V.forM meshGltf.primitives $ \primitive -> do
                geometry <- loadGeometry setup allocator transferBuffer primitive
                pure $ Primitive { geometry = geometry , material = Nothing }
        skyboxView <- makeCubemap setup allocator transferBuffer
        skyboxSampler <-
          Vk.manageSampler setup.device $
            Vk.zero
              { Vk.magFilter    = Vk.FILTER_LINEAR
              , Vk.minFilter    = Vk.FILTER_LINEAR
              , Vk.mipmapMode   = Vk.SAMPLER_MIPMAP_MODE_LINEAR
              , Vk.addressModeU = Vk.SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
              , Vk.addressModeV = Vk.SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
              , Vk.addressModeW = Vk.SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
              }
        skyboxSet <- Vk.allocateDescriptorSet setAllocator setLayout
        Vk.updateDescriptorSets setup.device
          [ Vk.SomeStruct $
              Vk.zero
                { Vk.dstSet          = skyboxSet
                , Vk.dstBinding      = 1
                , Vk.dstArrayElement = 0
                , Vk.descriptorCount = 1
                , Vk.descriptorType  = Vk.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
                , Vk.imageInfo       =
                    [ Vk.zero
                        { Vk.sampler     = skyboxSampler
                        , Vk.imageView   = skyboxView 
                        , Vk.imageLayout = Vk.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
                        }
                    ]
                }
          ]
          []
        -- load chess-related assets
        GLTF.runGLTF (GLTF.FromFile "examples/chess/assets/chess.glb") $ do
          gltf <- GLTF.ask
          -- textures are all loaded into one big array (an array of combined
          -- image samplers) and referenced from shaders via their indices.
          images <-
            V.forM gltf.asset.images $ \image ->
              GLTF.loadImage image >>=
                makeImageView
                  setup
                  allocator
                  transferBuffer
          samplerDefault <-
            Vk.manageSampler setup.device (toSamplerInfo GLTF.defaultSampler)
          samplers <-
            V.forM gltf.asset.samplers
              (Vk.manageSampler setup.device . toSamplerInfo)
          let
            textures =
              flip V.map gltf.asset.textures $ \texture ->
                let
                  sampler   = maybe samplerDefault (getFrom samplers) texture.sampler
                  imageView = maybe Vu.NULL_HANDLE (getFrom images) texture.source
                in
                  Vk.zero
                    { Vk.sampler     = sampler
                    , Vk.imageView   = imageView 
                    , Vk.imageLayout = Vk.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
                    }
          textureSet <- Vk.allocateDescriptorSet setAllocator setLayout
          Vk.updateDescriptorSets setup.device
            [ Vk.SomeStruct $
                Vk.zero
                  { Vk.dstSet          = textureSet
                  , Vk.dstBinding      = 1
                  , Vk.dstArrayElement = 0
                  , Vk.descriptorCount = fromIntegral $ V.length textures
                  , Vk.descriptorType  = Vk.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
                  , Vk.imageInfo       = textures
                  }
            ]
            []
          -- load meshes
          let fold start list f = foldrM f start list
          (meshFactory, index) <-
            fold (M.empty, emptyIndex) objects $ \(name, type') (factory, index) -> do
              node     <- GLTF.getNode name
              meshIx   <- GLTF.fromMaybe "Chess Mesh" node.mesh
              meshGltf <- GLTF.fetch meshIx
              mesh     <- fmap Mesh $
                V.forM meshGltf.primitives $ \primitive -> do
                  geometry    <- loadGeometry setup allocator transferBuffer primitive
                  materialIx  <- GLTF.fromMaybe "Material" primitive.material
                  material    <- GLTF.fetch materialIx
                  texInfo     <- GLTF.fromMaybe "Texture" material.roughness.baseColorTexture
                  coordIx     <- GLTF.fromMaybe "Coords" $ GLTF.getAttribute texInfo.texCoord primitive
                  coordAcc    <- GLTF.fetch coordIx
                  coordBuffer <-
                    GLTF.loadAccessor coordAcc >>=
                      makeBuffer
                        setup
                        allocator
                        transferBuffer
                        Vk.BUFFER_USAGE_VERTEX_BUFFER_BIT
                  pure $
                    Primitive
                      { geometry = geometry
                      , material = Just $
                          Material
                            { texIndex       = fromIntegral $ texInfo.index.value
                            , texCoordBuffer = coordBuffer
                            }
                      }
              -- depending on the object type (see further above), we either
              -- directly place loaded meshes in the world (Static), or we collect
              -- them in a factory to be instantiated multiple times (Dynamic), or
              -- we place them and populate a helper index for selectable objects (Target).
              case type' of
                Static -> do
                  newEntity_ (mesh, toTransform node.transform)
                  pure (factory, index)
                Target position -> do
                  e <- newEntity (mesh, toTransform node.transform)
                  pure (factory, insertField position e index)
                Dynamic piece ->
                  pure (M.insert piece mesh factory, index)
          let
            camera =
              Core.Camera
                { position = Vec3 0 (-10) 4
                , target   = Vec3 0 0 0
                , up       = Vec3 0 0 1
                , frustum  = Perspective 0.1 Nothing (pi/3) (width/height)
                          -- others for testing:
                          -- Perspective 0.1 (Just 1000) (pi/3) (width/height)
                          -- Orthographic 0.1 1000 10 (width / height)
                }
          set global
            ( Environment skyboxMesh skyboxSet textureSet
            , Camera camera 0
            , index
            , Screen Nothing (Vec2 width height) False
            )
          pure meshFactory

-- Load vertex-related data of a 3D object from a GLB file.
loadGeometry
  :: ( es <: IOE
     , es <: Fail
     , es <: GLTF.GLTF
     , es <: Vk.Memory a k
     , es <: Resource
     )
  => RenderSetup
  -> Vk.MemoryAllocator a k
  -> Vk.CommandBuffer
  -> GLTF.Primitive
  -> Eff es Geometry
loadGeometry setup allocator transferBuffer primitive = do
  positionIx <-
    GLTF.fromMaybe "Position" $
      GLTF.getAttribute GLTF.Position primitive
  positionAcc    <- GLTF.fetch positionIx
  positionBuffer <-
    GLTF.loadAccessor positionAcc >>=
      makeBuffer
        setup
        allocator
        transferBuffer
        Vk.BUFFER_USAGE_VERTEX_BUFFER_BIT
  indexInfo <-
    forM primitive.indices $ \ix -> do
      accessor <- GLTF.fetch ix
      ixBuffer <-
        GLTF.loadAccessor accessor >>=
          makeBuffer
            setup
            allocator
            transferBuffer
            Vk.BUFFER_USAGE_INDEX_BUFFER_BIT
      pure
        ( ixBuffer
        , fromIntegral $ accessor.count
        , toIndexType  $ accessor.componentType
        )
  normalIx <-
    GLTF.fromMaybe "Normal" $
      GLTF.getAttribute GLTF.Normal primitive
  normalAcc <- GLTF.fetch normalIx
  normalBuffer <-
    GLTF.loadAccessor normalAcc >>=
      makeBuffer
        setup
        allocator
        transferBuffer
        Vk.BUFFER_USAGE_VERTEX_BUFFER_BIT
  pure $
    Geometry
      { vertexBuffer = positionBuffer
      , vertexCount  = fromIntegral positionAcc.count
      , indexInfo    = indexInfo
      , normalBuffer = normalBuffer
      }

-- Helper function for creating a GPU cubemap and populating it with data from the skybox.
makeCubemap
  :: ( es <: IOE
     , es <: Vk.Memory a k
     , es <: Resource
     )
  => RenderSetup
  -> Vk.MemoryAllocator a k
  -> Vk.CommandBuffer
  -> Eff es Vk.ImageView
makeCubemap setup allocator tBuffer = do
  faces <-
    V.forM ["right", "left", "front", "back", "top", "bottom"] $ \file -> do
      bytes <- liftIO $ BS.readFile $ "examples/chess/assets/cube/" ++ file ++ ".jpg"
      image <- decodeImage bytes
      pure $ P.convertRGBA8 image
  let
    bytes  = fmap (.imageData) faces
    face   = faces V.! 0
    width  = fromIntegral $ P.imageWidth face
    height = fromIntegral $ P.imageHeight face
    size   = fromIntegral $ sum $ fmap Vk.vectorByteSize bytes
    layer  = fromIntegral $ size `div` 6
    extent = Vu.Extent3D width height 1
    range  =
      Vk.zero
        { Vk.aspectMask     = Vu.IMAGE_ASPECT_COLOR_BIT
        , Vk.levelCount     = 1
        , Vk.layerCount     = 6
        , Vk.baseArrayLayer = 0
        , Vk.baseMipLevel   = 0
        }
  image <-
    Vk.manageImage setup.device $
      Vk.zero
        { Vk.flags         = Vk.IMAGE_CREATE_CUBE_COMPATIBLE_BIT
        , Vk.imageType     = Vk.IMAGE_TYPE_2D
        , Vk.format        = Vk.FORMAT_R8G8B8A8_SRGB
        , Vk.extent        = extent
        , Vk.mipLevels     = 1
        , Vk.arrayLayers   = 6
        , Vk.samples       = Vk.SAMPLE_COUNT_1_BIT
        , Vk.tiling        = Vk.IMAGE_TILING_OPTIMAL
        , Vk.usage         = Vk.IMAGE_USAGE_TRANSFER_DST_BIT
                         .|. Vk.IMAGE_USAGE_SAMPLED_BIT
        , Vk.sharingMode   = Vk.SHARING_MODE_EXCLUSIVE
        , Vk.initialLayout = Vk.IMAGE_LAYOUT_UNDEFINED
        }
  Vk.manageBoundImageMemory_
    allocator
    image
    Vk.MEMORY_PROPERTY_DEVICE_LOCAL_BIT
  -- do staging, i.e., transfer texture to GPU memory that is shared between CPU
  -- and GPU ("stage buffer"), then transfer from there to the GPU-only memory.
  -- We do this in a region, which releases the staging-related resources on exit.
  withRegion $ do
    -- shared buffer
    stageBuffer <-
      Vk.manageBuffer setup.device $
        Vk.zero
          { Vk.size        = size
          , Vk.sharingMode = Vk.SHARING_MODE_EXCLUSIVE
          , Vk.usage       = Vk.BUFFER_USAGE_TRANSFER_SRC_BIT
          }
    -- memory for the shared buffer
    stageMemory <-
      Vk.manageBoundBufferMemory
        allocator
        stageBuffer
        Vk.MEMORY_PROPERTY_HOST_VISIBLE_BIT
    -- write to the shared buffer
    dstPtr <- Vk.manageMappedBuffer allocator stageMemory
    V.iforM_ bytes $ \i b ->
      Vk.writeVector (dstPtr `plusPtr` (i * layer)) b
    -- allocate resources for GPU transfer and synchronizations
    toTransfer <- Vk.manageSemaphore setup.device
    toShader   <- Vk.manageSemaphore setup.device
    gBuffers   <-
      Vk.manageCommandBuffers setup.device $
        Vk.zero
          { Vk.commandPool        = setup.gCommandPool
          , Vk.level              = Vk.COMMAND_BUFFER_LEVEL_PRIMARY
          , Vk.commandBufferCount = 2
          }
    let toTransferBuffer = gBuffers V.! 0
        toShaderBuffer   = gBuffers V.! 1
    -- barrier required for performing the memory transfer to the GPU-only image
    Vk.record toTransferBuffer Vk.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT $
      Vk.pipelineBarrier
        Vk.PIPELINE_STAGE_TOP_OF_PIPE_BIT
        Vk.PIPELINE_STAGE_TRANSFER_BIT
        Vk.zero
        []
        []
        [ Vk.SomeStruct $
            Vk.zero
              { Vo.srcAccessMask    = Vk.ACCESS_NONE
              , Vo.dstAccessMask    = Vk.ACCESS_TRANSFER_WRITE_BIT
              , Vo.oldLayout        = Vk.IMAGE_LAYOUT_UNDEFINED
              , Vo.newLayout        = Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
              , Vo.image            = image
              , Vo.subresourceRange = range
              }
        ]
    Vk.submitOne setup.gQueue Vu.NULL_HANDLE $
      Vk.zero
        { Vk.commandBuffers   = [toTransferBuffer.commandBufferHandle]
        , Vk.signalSemaphores = [toTransfer]
        }
    -- do the actual transfer from shared to GPU-only memory
    Vk.record tBuffer Vk.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT $ do
      Vk.copyBufferToImage
        stageBuffer
        image
        Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
        [ Vk.zero
            { Vb.bufferOffset      = 0
            , Vb.bufferRowLength   = 0
            , Vb.bufferImageHeight = 0
            , Vb.imageSubresource  =
                Vk.zero
                  { Vb.aspectMask     = Vu.IMAGE_ASPECT_COLOR_BIT
                  , Vb.mipLevel       = 0
                  , Vb.baseArrayLayer = 0
                  , Vb.layerCount     = 6
                  }
            , Vb.imageOffset = Vk.zero
            , Vb.imageExtent = extent
            }
        ]
    Vk.submitOne setup.tQueue Vu.NULL_HANDLE $
      Vk.zero
        { Vk.commandBuffers   = [tBuffer.commandBufferHandle]
        , Vk.waitSemaphores   = [toTransfer]
        , Vk.waitDstStageMask = [Vk.PIPELINE_STAGE_TRANSFER_BIT]
        , Vk.signalSemaphores = [toShader]
        }
    -- barrier required to make the GPU-only image readable by shaders
    Vk.record toShaderBuffer Vk.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT $
      Vk.pipelineBarrier
        Vk.PIPELINE_STAGE_TRANSFER_BIT
        Vk.PIPELINE_STAGE_FRAGMENT_SHADER_BIT
        Vk.zero
        []
        []
        [ Vk.SomeStruct $
            Vk.zero
              { Vo.srcAccessMask    = Vk.ACCESS_TRANSFER_WRITE_BIT
              , Vo.dstAccessMask    = Vk.ACCESS_SHADER_READ_BIT
              , Vo.oldLayout        = Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
              , Vo.newLayout        = Vk.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
              , Vo.image            = image
              , Vo.subresourceRange = range
              }
        ]
    Vk.submitOne setup.gQueue Vu.NULL_HANDLE $
      Vk.zero
        { Vk.commandBuffers   = [toShaderBuffer.commandBufferHandle]
        , Vk.waitSemaphores   = [toShader]
        , Vk.waitDstStageMask = [Vk.PIPELINE_STAGE_FRAGMENT_SHADER_BIT]
        }
    -- wait for copy to finish, can be optimized (fence, etc.)
    Vk.deviceWaitIdle setup.device
  Vk.manageImageView setup.device $
    Vk.zero
      { Vk.image            = image
      , Vk.viewType         = Vk.IMAGE_VIEW_TYPE_CUBE
      , Vk.format           = Vk.FORMAT_R8G8B8A8_SRGB
      , Vk.subresourceRange = range
      }

-- Helper function for creating a GPU buffer and populating it with data from a bytestring.
makeBuffer
  :: ( es <: IOE
     , es <: Vk.Memory a k
     , es <: Resource
     )
  => RenderSetup
  -> Vk.MemoryAllocator a k
  -> Vk.CommandBuffer
  -> Vk.BufferUsageFlags
  -> BS.ByteString
  -> Eff es Vk.Buffer
makeBuffer setup allocator transferBuffer flags bytes = do
  let size = fromIntegral $ BS.length bytes
  resultBuffer <-
    Vk.manageBuffer setup.device $
      Vk.zero
        { Vk.size        = size
        , Vk.sharingMode = Vk.SHARING_MODE_EXCLUSIVE
        , Vk.usage       = Vk.BUFFER_USAGE_TRANSFER_DST_BIT .|. flags
        }
  Vk.manageBoundBufferMemory_
    allocator
    resultBuffer
    Vk.MEMORY_PROPERTY_DEVICE_LOCAL_BIT
  -- do staging, i.e., transfer bytestring to GPU memory that is shared between CPU
  -- and GPU ("stage buffer"), then transfer from there to the GPU-only memory.
  -- We do this in a region, which releases the staging-related resources on exit.
  withRegion $ do
    -- shared buffer
    stageBuffer <-
      Vk.manageBuffer setup.device $
        Vk.zero
          { Vk.size        = size
          , Vk.sharingMode = Vk.SHARING_MODE_EXCLUSIVE
          , Vk.usage       = Vk.BUFFER_USAGE_TRANSFER_SRC_BIT .|. flags
          }
    -- memory for the shared buffer
    stageMemory <-
      Vk.manageBoundBufferMemory
        allocator
        stageBuffer
        Vk.MEMORY_PROPERTY_HOST_VISIBLE_BIT
    -- write to the shared buffer
    dstPtr <- Vk.manageMappedBuffer allocator stageMemory
    Vk.writeBytes dstPtr bytes
    -- do the actual transfer from shared to GPU-only memory
    Vk.record transferBuffer Vk.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT $
      Vk.copyBuffer stageBuffer resultBuffer [Vk.zero { Vb.size = size }]
    Vk.submitOne setup.tQueue Vu.NULL_HANDLE $
      Vk.zero { Vk.commandBuffers = [transferBuffer.commandBufferHandle] }
    -- wait for copy to finish, can be optimized (fence, etc.)
    Vk.deviceWaitIdle setup.device
  pure resultBuffer

-- Helper function for creating a GPU image and populating it with data from a bytestring.
makeImageView
  :: ( es <: IOE
     , es <: Vk.Memory a k
     , es <: Resource
     )
  => RenderSetup
  -> Vk.MemoryAllocator a k
  -> Vk.CommandBuffer
  -> BS.ByteString
  -> Eff es Vk.ImageView
makeImageView setup allocator tBuffer raw = do
  decodedImage <- decodeImage raw
  let
    rgba   = P.convertRGBA8 decodedImage
    size   = fromIntegral $ Vk.vectorByteSize rgba.imageData
    width  = fromIntegral $ P.imageWidth rgba
    height = fromIntegral $ P.imageHeight rgba
    extent = Vu.Extent3D width height 1
    range  =
      Vk.zero
        { Vk.aspectMask     = Vu.IMAGE_ASPECT_COLOR_BIT
        , Vk.levelCount     = 1
        , Vk.layerCount     = 1
        , Vk.baseArrayLayer = 0
        , Vk.baseMipLevel   = 0
        }
  image <-
    Vk.manageImage setup.device $
      Vk.zero
        { Vk.imageType     = Vk.IMAGE_TYPE_2D
        , Vk.format        = Vk.FORMAT_R8G8B8A8_SRGB
        , Vk.extent        = extent
        , Vk.mipLevels     = 1
        , Vk.arrayLayers   = 1
        , Vk.samples       = Vk.SAMPLE_COUNT_1_BIT
        , Vk.tiling        = Vk.IMAGE_TILING_OPTIMAL
        , Vk.usage         = Vk.IMAGE_USAGE_TRANSFER_DST_BIT
                         .|. Vk.IMAGE_USAGE_SAMPLED_BIT
        , Vk.sharingMode   = Vk.SHARING_MODE_EXCLUSIVE
        , Vk.initialLayout = Vk.IMAGE_LAYOUT_UNDEFINED
        }
  Vk.manageBoundImageMemory_
    allocator
    image
    Vk.MEMORY_PROPERTY_DEVICE_LOCAL_BIT
  -- do staging, i.e., transfer bytestring to GPU memory that is shared between CPU
  -- and GPU ("stage buffer"), then transfer from there to the GPU-only memory.
  -- We do this in a region, which releases the staging-related resources on exit.
  withRegion $ do
    -- shared buffer
    stageBuffer <-
      Vk.manageBuffer setup.device $
        Vk.zero
          { Vk.size        = size
          , Vk.sharingMode = Vk.SHARING_MODE_EXCLUSIVE
          , Vk.usage       = Vk.BUFFER_USAGE_TRANSFER_SRC_BIT
          }
    -- memory for the shared buffer
    stageMemory <-
      Vk.manageBoundBufferMemory
        allocator
        stageBuffer
        Vk.MEMORY_PROPERTY_HOST_VISIBLE_BIT
    -- write to the shared buffer
    dstPtr <- Vk.manageMappedBuffer allocator stageMemory
    Vk.writeVector dstPtr rgba.imageData
    -- allocate resources for GPU transfer and synchronizations
    toTransfer <- Vk.manageSemaphore setup.device
    toShader   <- Vk.manageSemaphore setup.device
    gBuffers   <-
      Vk.manageCommandBuffers setup.device $
        Vk.zero
          { Vk.commandPool        = setup.gCommandPool
          , Vk.level              = Vk.COMMAND_BUFFER_LEVEL_PRIMARY
          , Vk.commandBufferCount = 2
          }
    let toTransferBuffer = gBuffers V.! 0
        toShaderBuffer   = gBuffers V.! 1
    -- barrier required for performing the memory transfer to the GPU-only image
    Vk.record toTransferBuffer Vk.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT $
      Vk.pipelineBarrier
        Vk.PIPELINE_STAGE_TOP_OF_PIPE_BIT
        Vk.PIPELINE_STAGE_TRANSFER_BIT
        Vk.zero
        []
        []
        [ Vk.SomeStruct $
            Vk.zero
              { Vo.srcAccessMask    = Vk.ACCESS_NONE
              , Vo.dstAccessMask    = Vk.ACCESS_TRANSFER_WRITE_BIT
              , Vo.oldLayout        = Vk.IMAGE_LAYOUT_UNDEFINED
              , Vo.newLayout        = Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
              , Vo.image            = image
              , Vo.subresourceRange = range
              }
        ]
    Vk.submitOne setup.gQueue Vu.NULL_HANDLE $
      Vk.zero
        { Vk.commandBuffers   = [toTransferBuffer.commandBufferHandle]
        , Vk.signalSemaphores = [toTransfer]
        }
    -- do the actual transfer from shared to GPU-only memory
    Vk.record tBuffer Vk.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT $ do
      Vk.copyBufferToImage
        stageBuffer
        image
        Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
        [ Vk.zero
            { Vb.bufferOffset      = 0
            , Vb.bufferRowLength   = 0
            , Vb.bufferImageHeight = 0
            , Vb.imageSubresource  =
                Vk.zero
                  { Vb.aspectMask     = Vu.IMAGE_ASPECT_COLOR_BIT
                  , Vb.mipLevel       = 0
                  , Vb.baseArrayLayer = 0
                  , Vb.layerCount     = 1
                  }
            , Vb.imageOffset = Vk.zero
            , Vb.imageExtent = extent
            }
        ]
    Vk.submitOne setup.tQueue Vu.NULL_HANDLE $
      Vk.zero
        { Vk.commandBuffers   = [tBuffer.commandBufferHandle]
        , Vk.waitSemaphores   = [toTransfer]
        , Vk.waitDstStageMask = [Vk.PIPELINE_STAGE_TRANSFER_BIT]
        , Vk.signalSemaphores = [toShader]
        }
    -- barrier required to make the GPU-only image readable by shaders
    Vk.record toShaderBuffer Vk.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT $
      Vk.pipelineBarrier
        Vk.PIPELINE_STAGE_TRANSFER_BIT
        Vk.PIPELINE_STAGE_FRAGMENT_SHADER_BIT
        Vk.zero
        []
        []
        [ Vk.SomeStruct $
            Vk.zero
              { Vo.srcAccessMask    = Vk.ACCESS_TRANSFER_WRITE_BIT
              , Vo.dstAccessMask    = Vk.ACCESS_SHADER_READ_BIT
              , Vo.oldLayout        = Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
              , Vo.newLayout        = Vk.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
              , Vo.image            = image
              , Vo.subresourceRange = range
              }
        ]
    Vk.submitOne setup.gQueue Vu.NULL_HANDLE $
      Vk.zero
        { Vk.commandBuffers   = [toShaderBuffer.commandBufferHandle]
        , Vk.waitSemaphores   = [toShader]
        , Vk.waitDstStageMask = [Vk.PIPELINE_STAGE_FRAGMENT_SHADER_BIT]
        }
    -- wait for copy to finish, can be optimized (fence, etc.)
    Vk.deviceWaitIdle setup.device
  Vk.manageImageView setup.device $
    Vk.zero
      { Vk.image            = image
      , Vk.viewType         = Vk.IMAGE_VIEW_TYPE_2D
      , Vk.format           = Vk.FORMAT_R8G8B8A8_SRGB
      , Vk.subresourceRange = range
      }

-- Helper function for indexing a vector with an integer-like index.
getFrom :: Coercible i Int => V.Vector a -> i -> a
getFrom vector = (vector V.!) . coerce

-- Helper functoin for creating an image from a bytestring.
decodeImage :: MonadIO m => BS.ByteString -> m P.DynamicImage
decodeImage bytes =
  liftIO $
    case P.decodeImage bytes of
      Left err    -> fail err
      Right image -> pure image

-- Helper function from GLTF transform to own transform component.
toTransform :: GLTF.Transform -> Transform
toTransform = \case
  GLTF.MatrixTransform matrix -> Transform t r s
    where (t, r, s) = toTRS matrix
  GLTF.RotateScaleTranslate r s t -> Transform t r s
