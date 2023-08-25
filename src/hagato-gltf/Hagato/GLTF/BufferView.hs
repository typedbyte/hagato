{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hagato.GLTF.BufferView
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Types and functions for handling buffer views found in glTF files.
-----------------------------------------------------------------------------
module Hagato.GLTF.BufferView where

-- aeson
import Data.Aeson (FromJSON(..), Object, Value, (.:), (.:?), (.!=), withObject)

-- text
import Data.Text qualified as T

-- vector
import Data.Vector qualified as V

import Hagato.GLTF.Aeson (failWithContext)
import Hagato.GLTF.Index (BufferIx, BufferViewIx(value), Index, get)

-- | A hint representing the intended GPU buffer type to use with a buffer view.
data BufferViewTarget
  = ArrayBuffer
  | ElementArrayBuffer
  deriving (Eq, Ord, Show)

instance FromJSON BufferViewTarget where
  parseJSON value = do
    number <- parseJSON value
    case number :: Int of
      34962   -> pure ArrayBuffer
      34963   -> pure ElementArrayBuffer
      invalid -> failWithContext "BufferViewTarget" invalid

-- | Represents a view into a buffer, which can be seen as a subset of the buffer.
data BufferView = BufferView
  { buffer :: BufferIx
    -- ^ The index of the buffer.
  , byteOffset :: Int
    -- ^ The offset into the buffer in bytes.
  , byteLength :: Int
    -- ^ The length of the bufferView in bytes.
  , byteStride :: Maybe Int
    -- ^ The stride, in bytes.
  , target :: Maybe BufferViewTarget
    -- ^ A hint representing the intended GPU buffer type to use with this buffer view.
  , name :: Maybe T.Text
    -- ^ The name of the buffer view.
  , extensions :: Maybe Object
    -- ^ A JSON object with extension-specific objects.
  , extras :: Maybe Value
    -- ^ Application-specific data.
  }
  deriving (Eq, Ord, Show)

instance FromJSON BufferView where
  parseJSON = withObject "BufferView" $ \v ->
    BufferView
      <$> v .:  "buffer"
      <*> v .:? "byteOffset" .!= 0
      <*> v .:  "byteLength"
      <*> v .:? "byteStride"
      <*> v .:? "target"
      <*> v .:? "name"
      <*> v .:? "extensions"
      <*> v .:? "extras"

instance Index BufferViewIx (V.Vector BufferView) BufferView where
  get i vec = vec V.! i.value
  {-# INLINE get #-}