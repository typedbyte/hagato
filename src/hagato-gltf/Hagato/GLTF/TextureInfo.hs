{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hagato.GLTF.TextureInfo
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Types and functions for handling references to textures found in glTF files.
-----------------------------------------------------------------------------
module Hagato.GLTF.TextureInfo where

-- aeson
import Data.Aeson (FromJSON(..), Object, Value, (.:), (.:?), (.!=), withObject)

import Hagato.GLTF.Attribute (Attribute(Texcoord))
import Hagato.GLTF.Index     (TextureIx)

-- | Represents a scalar parameter applied to each normal vector of a normal texture.
newtype NormalScale = NormalScale { value :: Float }
  deriving (Eq, Ord, Show, Num, FromJSON)

-- | Represents a scalar multiplier controlling the amount of occlusion applied.
newtype Strength = Strength { value :: Float }
  deriving (Eq, Ord, Show, Num, FromJSON)

-- | Represents a reference to a texture.
data TextureInfo a = TextureInfo
  { index :: TextureIx
    -- ^ The index of the texture.
  , texCoord :: Attribute
    -- ^ The attribute used for texture coordinate mapping.
  , modifier :: a
    -- ^ An optional modifier which controls normal vectors (see 'NormalScale') or
    -- occlusion (see 'Strength').
  , extensions :: Maybe Object
    -- ^ A JSON object with extension-specific objects.
  , extras :: Maybe Value
    -- ^ Application-specific data.
  }
  deriving (Eq, Ord, Show)

instance FromJSON (TextureInfo ()) where
  parseJSON = withObject "TextureInfo" $ \v ->
    TextureInfo
      <$> v .: "index"
      <*> (Texcoord <$> v .:? "texCoord" .!= 0)
      <*> pure ()
      <*> v .:? "extensions"
      <*> v .:? "extras"

instance FromJSON (TextureInfo NormalScale) where
  parseJSON = withObject "TextureInfo" $ \v ->
    TextureInfo
      <$> v .: "index"
      <*> (Texcoord <$> v .:? "texCoord" .!= 0)
      <*> v .:? "scale" .!= 1
      <*> v .:? "extensions"
      <*> v .:? "extras"

instance FromJSON (TextureInfo Strength) where
  parseJSON = withObject "TextureInfo" $ \v ->
    TextureInfo
      <$> v .: "index"
      <*> (Texcoord <$> v .:? "texCoord" .!= 0)
      <*> v .:? "strength" .!= 1
      <*> v .:? "extensions"
      <*> v .:? "extras"