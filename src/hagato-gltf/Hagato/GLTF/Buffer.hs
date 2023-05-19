{-# LANGUAGE OverloadedStrings #-}
module Hagato.GLTF.Buffer where

-- aeson
import Data.Aeson (FromJSON(parseJSON), Object, Value, (.:), (.:?), withObject)

-- text
import Data.Text qualified as T

-- vector
import Data.Vector qualified as V

import Hagato.GLTF.Index (BufferIx(value), Index, get)
import Hagato.GLTF.URI   (URI)

-- | A buffer points to binary geometry, animation, or skins.
data Buffer = Buffer
  { uri        :: Maybe URI
  , byteLength :: Int
  , name       :: Maybe T.Text
  , extensions :: Maybe Object
  , extras     :: Maybe Value
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