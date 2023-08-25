{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hagato.GLTF.Scene
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Types and functions for handling scenes found in glTF files.
-----------------------------------------------------------------------------
module Hagato.GLTF.Scene where

-- aeson
import Data.Aeson (FromJSON(..), Object, Value, (.:?), (.!=), withObject)

-- text
import Data.Text qualified as T

-- vector
import Data.Vector          qualified as V
import Data.Vector.Storable qualified as S

import Hagato.GLTF.Index (Index, NodeIx, SceneIx(value), get)

-- | Represents the root nodes of a scene.
data Scene = Scene
  { nodes :: S.Vector NodeIx
    -- ^ The indices of each root node.
  , name :: Maybe T.Text
    -- ^ The name of the scene.
  , extensions :: Maybe Object
    -- ^ A JSON object with extension-specific objects.
  , extras :: Maybe Value
    -- ^ Application-specific data.
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