{-# LANGUAGE OverloadedStrings #-}
module Hagato.GLTF.TextureInfo where

-- aeson
import Data.Aeson (FromJSON(..), Object, Value, (.:), (.:?), (.!=), withObject)

import Hagato.GLTF.Attribute (Attribute(Texcoord))
import Hagato.GLTF.Index     (TextureIx)

-- | The scalar parameter applied to each normal vector of the normal texture.
newtype NormalScale = NormalScale { value :: Float }
  deriving (Eq, Ord, Show, Num, FromJSON)

-- | A scalar multiplier controlling the amount of occlusion applied.
newtype Strength = Strength { value :: Float }
  deriving (Eq, Ord, Show, Num, FromJSON)

-- | Reference to a texture.
data TextureInfo a = TextureInfo
  { index      :: TextureIx
  , texCoord   :: Attribute
  , modifier   :: a
  , extensions :: Maybe Object
  , extras     :: Maybe Value
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