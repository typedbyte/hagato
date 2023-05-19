{-# LANGUAGE OverloadedStrings #-}
module Hagato.GLTF.Asset where

-- aeson
import Data.Aeson (FromJSON(..), Object, Value, (.:), (.:?), (.!=), eitherDecode, withObject)

-- binary
import Data.Binary.Get (Get, getLazyByteString, getWord32le)

-- vector
import Data.Vector qualified as V

-- text
import Data.Text qualified as T

import Hagato.GLTF.Accessor   (Accessor)
import Hagato.GLTF.Animation  (Animation)
import Hagato.GLTF.AssetInfo  (AssetInfo)
import Hagato.GLTF.Buffer     (Buffer)
import Hagato.GLTF.BufferView (BufferView)
import Hagato.GLTF.Camera     (Camera)
import Hagato.GLTF.Image      (Image)
import Hagato.GLTF.Index
import Hagato.GLTF.Material   (Material)
import Hagato.GLTF.Mesh       (Mesh)
import Hagato.GLTF.Node       (Node)
import Hagato.GLTF.Sampler    (Sampler)
import Hagato.GLTF.Scene      (Scene)
import Hagato.GLTF.Skin       (Skin)
import Hagato.GLTF.Texture    (Texture)

-- | The root object for a glTF asset.
data Asset = Asset
  { usedExtensions     :: V.Vector T.Text
  , requiredExtensions :: V.Vector T.Text
  , accessors          :: V.Vector Accessor
  , animations         :: V.Vector Animation
  , info               :: AssetInfo
  , buffers            :: V.Vector Buffer
  , bufferViews        :: V.Vector BufferView
  , cameras            :: V.Vector Camera
  , images             :: V.Vector Image
  , materials          :: V.Vector Material
  , meshes             :: V.Vector Mesh
  , nodes              :: V.Vector Node
  , samplers           :: V.Vector Sampler
  , scene              :: Maybe SceneIx
  , scenes             :: V.Vector Scene
  , skins              :: V.Vector Skin
  , textures           :: V.Vector Texture
  , extensions         :: Maybe Object
  , extras             :: Maybe Value
  }
  deriving (Eq, Ord, Show)

instance FromJSON Asset where
  parseJSON = withObject "Asset" $ \v ->
    Asset
      <$> v .:? "extensionsUsed"     .!= V.empty
      <*> v .:? "extensionsRequired" .!= V.empty
      <*> v .:? "accessors"          .!= V.empty
      <*> v .:? "animations"         .!= V.empty
      <*> v .:  "asset"
      <*> v .:? "buffers"            .!= V.empty
      <*> v .:? "bufferViews"        .!= V.empty
      <*> v .:? "cameras"            .!= V.empty
      <*> v .:? "images"             .!= V.empty
      <*> v .:? "materials"          .!= V.empty
      <*> v .:? "meshes"             .!= V.empty
      <*> v .:? "nodes"              .!= V.empty
      <*> v .:? "samplers"           .!= V.empty
      <*> v .:? "scene"
      <*> v .:? "scenes"             .!= V.empty
      <*> v .:? "skins"              .!= V.empty
      <*> v .:? "textures"           .!= V.empty
      <*> v .:? "extensions"
      <*> v .:? "extras"

instance Index AccessorIx Asset Accessor where
  get i = get i . (.accessors)
  {-# INLINE get #-}

instance Index AnimationIx Asset Animation where
  get i = get i . (.animations)
  {-# INLINE get #-}

instance Index BufferIx Asset Buffer where
  get i = get i . (.buffers)
  {-# INLINE get #-}

instance Index BufferViewIx Asset BufferView where
  get i = get i . (.bufferViews)
  {-# INLINE get #-}

instance Index CameraIx Asset Camera where
  get i = get i . (.cameras)
  {-# INLINE get #-}

instance Index ImageIx Asset Image where
  get i = get i . (.images)
  {-# INLINE get #-}

instance Index MaterialIx Asset Material where
  get i = get i . (.materials)
  {-# INLINE get #-}

instance Index MeshIx Asset Mesh where
  get i = get i . (.meshes)
  {-# INLINE get #-}

instance Index NodeIx Asset Node where
  get i = get i . (.nodes)
  {-# INLINE get #-}

instance Index SamplerIx Asset Sampler where
  get i = get i . (.samplers)
  {-# INLINE get #-}

instance Index TextureIx Asset Texture where
  get i = get i . (.textures)
  {-# INLINE get #-}

getAsset :: Get Asset
getAsset = do
  dataLength <- getWord32le
  0x4E4F534A <- getWord32le
  dataBytes  <- getLazyByteString (fromIntegral dataLength)
  case eitherDecode dataBytes of
    Left err    -> fail err
    Right asset -> pure asset
