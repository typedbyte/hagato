{-# LANGUAGE OverloadedStrings #-}
module Hagato.GLTF.Scene where

-- aeson
import Data.Aeson (FromJSON(..), Object, Value, (.:?), (.!=), withObject)

-- text
import Data.Text qualified as T

-- vector
import Data.Vector          qualified as V
import Data.Vector.Storable qualified as S

import Hagato.GLTF.Index (Index, NodeIx, SceneIx(value), get)

-- | The root nodes of a scene.
data Scene = Scene
  { nodes      :: S.Vector NodeIx
  , name       :: Maybe T.Text
  , extensions :: Maybe Object
  , extras     :: Maybe Value
  }
  deriving (Eq, Ord, Show)

instance FromJSON Scene where
  parseJSON = withObject "Scene" $ \v ->
    Scene
      <$> v .:? "nodes" .!= S.empty
      <*> v .:? "name"
      <*> v .:? "extensions"
      <*> v .:? "extras"

instance Index SceneIx (V.Vector Scene) Scene where
  get i vec = vec V.! i.value
  {-# INLINE get #-}