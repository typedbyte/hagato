{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hagato.GLTF.Asset
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Types and functions for handling the actual scenes and models found in
-- glTF files.
-----------------------------------------------------------------------------
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

-- | Represents the root object for a glTF asset.
data Asset = Asset
  { usedExtensions :: V.Vector T.Text
    -- ^ The names of glTF extensions used in this asset.
  , requiredExtensions :: V.Vector T.Text
    -- ^ Names of glTF extensions required to properly load this asset.
  , accessors :: V.Vector Accessor
    -- ^ A vector of accessors.
  , animations :: V.Vector Animation
    -- ^ A vector of keyframe animations.
  , info :: AssetInfo
    -- ^ Metadata about the glTF asset.
  , buffers :: V.Vector Buffer
    -- ^ A vector of buffers.
  , bufferViews :: V.Vector BufferView
    -- ^ A vector of buffer views.
  , cameras :: V.Vector Camera
    -- ^ A vector of cameras.
  , images :: V.Vector Image
    -- ^ A vector of images.
  , materials :: V.Vector Material
    -- ^ A vector of materials.
  , meshes :: V.Vector Mesh
    -- ^ A vector of meshes.
  , nodes :: V.Vector Node
    -- ^ A vector of nodes.
  , samplers :: V.Vector Sampler
    -- ^ A vector of samplers.
  , scene :: Maybe SceneIx
    -- ^ The index of the default scene.
  , scenes :: V.Vector Scene
    -- ^ A vector of scenes.
  , skins :: V.Vector Skin
    -- ^ A vector of skins.
  , textures :: V.Vector Texture
    -- ^ A vector of textures.
  , extensions :: Maybe Object
    -- ^ A JSON object with extension-specific objects.
  , extras :: Maybe Value
    -- ^ Application-specific data.
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

-- | Reads the asset of a binary glTF file.
getAsset :: Get Asset
getAsset = do
  dataLength <- getWord32le
  0x4E4F534A <- getWord32le
  dataBytes  <- getLazyByteString (fromIntegral dataLength)
  case eitherDecode dataBytes of
    Left err    -> fail err
    Right asset -> pure asset
