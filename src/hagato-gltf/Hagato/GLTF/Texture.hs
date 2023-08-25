{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hagato.GLTF.Texture
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Types and functions for handling textures found in glTF files.
-----------------------------------------------------------------------------
module Hagato.GLTF.Texture where

-- aeson
import Data.Aeson (FromJSON(..), Object, Value, (.:?), withObject)

-- text
import Data.Text qualified as T

-- vector
import Data.Vector qualified as V

import Hagato.GLTF.Index (ImageIx, Index, SamplerIx, TextureIx(value), get)

-- | Represents a texture and its sampler.
data Texture = Texture
  { sampler :: Maybe SamplerIx
    -- ^ The index of the sampler used by this texture.
  , source :: Maybe ImageIx
    -- ^ The index of the image used by this texture.
  , name :: Maybe T.Text
    -- ^ The name of the texture.
  , extensions :: Maybe Object
    -- ^ A JSON object with extension-specific objects.
  , extras :: Maybe Value
    -- ^ Application-specific data.
  }
  deriving (Eq, Ord, Show)
  
instance FromJSON Texture where
  parseJSON = withObject "Texture" $ \v ->
    Texture
      <$> v .:? "sampler"
      <*> v .:? "source"
      <*> v .:? "name"
      <*> v .:? "extensions"
      <*> v .:? "extras"

instance Index TextureIx (V.Vector Texture) Texture where
  get i vec = vec V.! i.value
  {-# INLINE get #-}