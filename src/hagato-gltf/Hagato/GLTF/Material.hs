{-# LANGUAGE OverloadedStrings #-}
module Hagato.GLTF.Material where

-- aeson
import Data.Aeson (FromJSON(..), Object, Value, (.:?), (.!=), withObject, withText)

-- hagato:with-core
import Hagato.Core (Vec3(Vec3))

-- text
import Data.Text qualified as T

-- vector
import Data.Vector qualified as V

import Hagato.GLTF.Aeson             (failWithContext)
import Hagato.GLTF.Index             (Index, MaterialIx(value), get)
import Hagato.GLTF.MetallicRoughness (MetallicRoughness, defaultRoughness)
import Hagato.GLTF.TextureInfo       (NormalScale, Strength, TextureInfo)

-- | The alpha rendering mode of a material.
data AlphaMode
  = Opaque
  | Mask
  | Blend
  deriving (Eq, Ord, Show)

instance FromJSON AlphaMode where
  parseJSON = withText "AlphaMode" $ \v ->
    case v of
      "OPAQUE" -> pure Opaque
      "MASK"   -> pure Mask
      "BLEND"  -> pure Blend
      invalid  -> failWithContext "AlphaMode" invalid

-- | The material appearance of a primitive.
data Material = Material
  { name             :: Maybe T.Text
  , extensions       :: Maybe Object
  , extras           :: Maybe Value
  , roughness        :: MetallicRoughness
  , normalTexture    :: Maybe (TextureInfo NormalScale)
  , occlusionTexture :: Maybe (TextureInfo Strength)
  , emissiveTexture  :: Maybe (TextureInfo ())
  , emissiveFactor   :: Vec3
  , alphaMode        :: AlphaMode
  , alphaCutoff      :: Float
  , doubleSided      :: Bool
  }
  deriving (Eq, Ord, Show)

instance FromJSON Material where
  parseJSON = withObject "Material" $ \v ->
    Material
      <$> v .:? "name"
      <*> v .:? "extensions"
      <*> v .:? "extras"
      <*> v .:? "pbrMetallicRoughness" .!= defaultRoughness
      <*> v .:? "normalTexture"
      <*> v .:? "occlusionTexture"
      <*> v .:? "emissiveTexture"
      <*> (toVec3 <$> v .:? "emissiveFactor" .!= (0, 0, 0))
      <*> v .:? "alphaMode"   .!= Opaque
      <*> v .:? "alphaCutoff" .!= 0.5
      <*> v .:? "doubleSided" .!= False
    where
      toVec3 (a, b, c) = Vec3 a b c

instance Index MaterialIx (V.Vector Material) Material where
  get i vec = vec V.! i.value
  {-# INLINE get #-}