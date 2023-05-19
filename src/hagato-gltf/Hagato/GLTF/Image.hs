{-# LANGUAGE OverloadedStrings #-}
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

-- | The image's media type.
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

-- | The payload of the image data.
data ImageData
  = ImageBuffer BufferViewIx MimeType
  | ImageURI URI (Maybe MimeType)
  deriving (Eq, Ord, Show)

-- | Image data used to create a texture.
data Image = Image
  { imageData  :: ImageData
  , name       :: Maybe T.Text
  , extensions :: Maybe Object
  , extras     :: Maybe Value
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