{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hagato.GLTF.MetallicRoughness
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Types and functions for handling metallic-roughness found in glTF files.
-----------------------------------------------------------------------------
module Hagato.GLTF.MetallicRoughness where

-- aeson
import Data.Aeson (FromJSON(..), Object, Value, (.:?), (.!=), withObject)

-- hagato:with-core
import Hagato.Core (Vec4(Vec4))

import Hagato.GLTF.TextureInfo (TextureInfo)

-- | Represents a set of parameter values that are used to define the metallic-roughness
-- material model from Physically-Based Rendering (PBR) methodology.
data MetallicRoughness = MetallicRoughness
  { baseColorFactor :: Vec4
    -- ^ The factors for the base color of the material.
  , baseColorTexture :: Maybe (TextureInfo ())
    -- ^ The base color texture.
  , metallicFactor :: Float
    -- ^ The factor for the metalness of the material.
  , roughnessFactor :: Float
    -- ^ The factor for the roughness of the material.
  , roughnessTexture :: Maybe (TextureInfo ())
    -- ^ The metallic-roughness texture.
  , extensions :: Maybe Object
    -- ^ A JSON object with extension-specific objects.
  , extras :: Maybe Value
    -- ^ Application-specific data.
  }
  deriving (Eq, Ord, Show)

instance FromJSON MetallicRoughness where
  parseJSON = withObject "MetallicRoughness" $ \v ->
    MetallicRoughness
      <$> (toVec4 <$> v .:? "baseColorFactor" .!= (1, 1, 1, 1))
      <*> v .:? "baseColorTexture"
      <*> v .:? "metallicFactor"  .!= 1
      <*> v .:? "roughnessFactor" .!= 1
      <*> v .:? "metallicRoughnessTexture"
      <*> v .:? "extensions"
      <*> v .:? "extras"
    where
      toVec4 (a, b, c, d) = Vec4 a b c d

-- | Default metallic-roughness if it is not defined explicitly.
defaultRoughness :: MetallicRoughness
defaultRoughness =
  MetallicRoughness
    (Vec4 1 1 1 1)
    Nothing
    1
    1
    Nothing
    Nothing
    Nothing