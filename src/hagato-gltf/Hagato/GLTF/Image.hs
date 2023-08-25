{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hagato.GLTF.Image
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Types and functions for handling images found in glTF files.
-----------------------------------------------------------------------------
module Hagato.GLTF.Image where

-- aeson
import Data.Aeson (FromJSON(..), Object, Value, (.:), (.:?), withObject, withText)

-- base
import Control.Applicative ((<|>))

-- text
import Data.Text qualified as T

-- vector
import Data.Vector qualified as V

import Hagato.GLTF.Aeson (failWithContext)
import Hagato.GLTF.Index (BufferViewIx, ImageIx(value), Index, get)
import Hagato.GLTF.URI   (URI)

-- | Represents the image's media type.
data MimeType
  = JPEG
  | PNG
  deriving (Eq, Ord, Show)

instance FromJSON MimeType where
  parseJSON = withText "MimeType" $ \v ->
    case v of
      "image/jpeg" -> pure JPEG
      "image/png"  -> pure PNG
      invalid      -> failWithContext "MimeType" invalid

-- | Represents the payload of the image data.
data ImageData
  = ImageBuffer BufferViewIx MimeType
    -- ^ The index of the buffer view holding the payload.
  | ImageURI URI (Maybe MimeType)
    -- ^ The URI of the image. Relative paths are relative to the current glTF asset.
  deriving (Eq, Ord, Show)

-- | Represents the image data used to create a texture.
data Image = Image
  { imageData :: ImageData
    -- ^ The payload of the image.
  , name :: Maybe T.Text
    -- ^ The name of the object.
  , extensions :: Maybe Object
    -- ^ A JSON object with extension-specific objects.
  , extras :: Maybe Value
    -- ^ Application-specific data.
  }
  deriving (Eq, Ord, Show)

instance FromJSON Image where
  parseJSON = withObject "Image" $ \v ->
    Image
      <$> (toImageBuffer v <|> toImageURI v)
      <*> v .:? "name"
      <*> v .:? "extensions"
      <*> v .:? "extras"
    where
      toImageBuffer v =
        ImageBuffer
          <$> v .: "bufferView"
          <*> v .: "mimeType"
      toImageURI v =
        ImageURI
          <$> v .:  "uri"
          <*> v .:? "mimeType"

instance Index ImageIx (V.Vector Image) Image where
  get i vec = vec V.! i.value
  {-# INLINE get #-}