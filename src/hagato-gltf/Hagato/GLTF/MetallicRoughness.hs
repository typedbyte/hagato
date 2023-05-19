{-# LANGUAGE OverloadedStrings #-}
module Hagato.GLTF.MetallicRoughness where

-- aeson
import Data.Aeson (FromJSON(..), Object, Value, (.:?), (.!=), withObject)

-- hagato:with-core
import Hagato.Core (Vec4(Vec4))

import Hagato.GLTF.TextureInfo (TextureInfo)

data MetallicRoughness = MetallicRoughness
  { baseColorFactor  :: Vec4
  , baseColorTexture :: Maybe (TextureInfo ())
  , metallicFactor   :: Float
  , roughnessFactor  :: Float
  , roughnessTexture :: Maybe (TextureInfo ())
  , extensions       :: Maybe Object
  , extras           :: Maybe Value
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