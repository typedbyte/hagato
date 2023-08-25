{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hagato.GLTF.Material
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Types and functions for handling materials found in glTF files.
-----------------------------------------------------------------------------
module Hagato.GLTF.Material where

-- aeson
import Data.Aeson (FromJSON(..), Object, Value, (.:?), (.!=), withObject, withText)

-- hagato:with-core
import Hagato.Core (Vec3(Vec3))

-- text
import Data.Text qualified as T

-- vector
import Data.Vector qualified as V

import Hagato.GLTF.Aeson             (failWithContext)
import Hagato.GLTF.Index             (Index, MaterialIx(value), get)
import Hagato.GLTF.MetallicRoughness (MetallicRoughness, defaultRoughness)
import Hagato.GLTF.TextureInfo       (NormalScale, Strength, TextureInfo)

-- | Represents the alpha rendering mode of a material.
data AlphaMode
  = Opaque
    -- ^ The alpha value is ignored, and the rendered output is fully opaque.
  | Mask
    -- ^ The rendered output is either fully opaque or fully transparent depending
    -- on the alpha value and the specified 'Hagato.GLTF.Material.alphaCutoff' value.
  | Blend
    -- ^ The alpha value is used to composite the source and destination areas.
  deriving (Eq, Ord, Show)

instance FromJSON AlphaMode where
  parseJSON = withText "AlphaMode" $ \v ->
    case v of
      "OPAQUE" -> pure Opaque
      "MASK"   -> pure Mask
      "BLEND"  -> pure Blend
      invalid  -> failWithContext "AlphaMode" invalid

-- | Represents the material appearance of a primitive.
data Material = Material
  { name :: Maybe T.Text
    -- ^ The name of the material.
  , extensions :: Maybe Object
    -- ^ A JSON object with extension-specific objects.
  , extras :: Maybe Value
    -- ^ Application-specific data.
  , roughness :: MetallicRoughness
    -- ^ The metallic roughness of the material.
  , normalTexture :: Maybe (TextureInfo NormalScale)
    -- ^ The tangent space normal texture of the material.
  , occlusionTexture :: Maybe (TextureInfo Strength)
    -- ^ The occlusion texture of the material.
  , emissiveTexture :: Maybe (TextureInfo ())
    -- ^ The emissive texture of the material.
  , emissiveFactor :: Vec3
    -- ^ The factors for the emissive color of the material.
  , alphaMode :: AlphaMode
    -- ^ The alpha rendering mode of the material.
  , alphaCutoff :: Float
    -- ^ The alpha cutoff value of the material.
  , doubleSided :: Bool
    -- ^ Specifies whether the material is double sided.
  }
  deriving (Eq, Ord, Show)

instance FromJSON Material where
  parseJSON = withObject "Material" $ \v ->
    Material
      <$> v .:? "name"
      <*> v .:? "extensions"
      <*> v .:? "extras"
      <*> v .:? "pbrMetallicRoughness" .!= defaultRoughness
      <*> v .:? "normalTexture"
      <*> v .:? "occlusionTexture"
      <*> v .:? "emissiveTexture"
      <*> (toVec3 <$> v .:? "emissiveFactor" .!= (0, 0, 0))
      <*> v .:? "alphaMode"   .!= Opaque
      <*> v .:? "alphaCutoff" .!= 0.5
      <*> v .:? "doubleSided" .!= False
    where
      toVec3 (a, b, c) = Vec3 a b c

instance Index MaterialIx (V.Vector Material) Material where
  get i vec = vec V.! i.value
  {-# INLINE get #-}