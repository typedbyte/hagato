{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hagato.GLTF.Buffer
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Types and functions for handling buffers found in glTF files.
-----------------------------------------------------------------------------
module Hagato.GLTF.Buffer where

-- aeson
import Data.Aeson (FromJSON(parseJSON), Object, Value, (.:), (.:?), withObject)

-- text
import Data.Text qualified as T

-- vector
import Data.Vector qualified as V

import Hagato.GLTF.Index (BufferIx(value), Index, get)
import Hagato.GLTF.URI   (URI)

-- | Represents a buffer which points to binary geometry, animation, or skins.
data Buffer = Buffer
  { uri :: Maybe URI
    -- ^ The URI of the buffer.
  , byteLength :: Int
    -- ^ The length of the buffer in bytes.
  , name :: Maybe T.Text
    -- ^ The name of the buffer.
  , extensions :: Maybe Object
    -- ^ A JSON object with extension-specific objects.
  , extras :: Maybe Value
    -- ^ Application-specific data.
  }
  deriving (Eq, Ord, Show)

instance FromJSON Buffer where
  parseJSON = withObject "Buffer" $ \v ->
    Buffer
      <$> v .:? "uri"
      <*> v .:  "byteLength"
      <*> v .:? "name"
      <*> v .:? "extensions"
      <*> v .:? "extras"

instance Index BufferIx (V.Vector Buffer) Buffer where
  get i vec = vec V.! i.value
  {-# INLINE get #-}