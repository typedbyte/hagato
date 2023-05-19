{-# LANGUAGE OverloadedStrings #-}
module Hagato.GLTF.Texture where

-- aeson
import Data.Aeson (FromJSON(..), Object, Value, (.:?), withObject)

-- text
import Data.Text qualified as T

-- vector
import Data.Vector qualified as V

import Hagato.GLTF.Index (ImageIx, Index, SamplerIx, TextureIx(value), get)

-- | A texture and its sampler.
data Texture = Texture
  { sampler    :: Maybe SamplerIx
  , source     :: Maybe ImageIx
  , name       :: Maybe T.Text
  , extensions :: Maybe Object
  , extras     :: Maybe Value
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