{-# LANGUAGE OverloadedStrings #-}
module Hagato.GLTF.Skin where

-- aeson
import Data.Aeson (FromJSON(..), Object, Value, (.:), (.:?), withObject)

-- text
import Data.Text qualified as T

-- vector
import Data.Vector qualified as V

import Hagato.GLTF.Index (AccessorIx, Index, NodeIx, SkinIx(value), get)

-- | Joints and matrices defining a skin.
data Skin = Skin
  { matrices   :: Maybe AccessorIx
  , skeleton   :: Maybe NodeIx
  , joints     :: V.Vector NodeIx
  , name       :: Maybe T.Text
  , extensions :: Maybe Object
  , extras     :: Maybe Value
  }
  deriving (Eq, Ord, Show)

instance FromJSON Skin where
  parseJSON = withObject "Skin" $ \v ->
    Skin
      <$> v .:? "inverseBindMatrices"
      <*> v .:? "skeleton"
      <*> v .:  "joints"
      <*> v .:? "name"
      <*> v .:? "extensions"
      <*> v .:? "extras"

instance Index SkinIx (V.Vector Skin) Skin where
  get i vec = vec V.! i.value
  {-# INLINE get #-}