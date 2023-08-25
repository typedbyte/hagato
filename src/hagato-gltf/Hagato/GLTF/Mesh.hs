{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hagato.GLTF.Mesh
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Types and functions for handling meshes found in glTF files.
-----------------------------------------------------------------------------
module Hagato.GLTF.Mesh where

-- aeson
import Data.Aeson (FromJSON(..), Object, Value, (.:), (.:?), (.!=), withObject)

-- containers
import Data.Map.Strict qualified as M

-- text
import Data.Text qualified as T

-- vector
import Data.Vector          qualified as V
import Data.Vector.Storable qualified as S

import Hagato.GLTF.Aeson     (failWithContext)
import Hagato.GLTF.Attribute (Attribute)
import Hagato.GLTF.Index     (AccessorIx, Index, MaterialIx, MeshIx(value), get)

-- | Represents the topology type of primitives to render.
data PrimitiveMode
  = Points
  | Lines
  | LineLoop
  | LineStrip
  | Triangles
  | TriangleStrip
  | TriangleFan
  deriving (Eq, Ord, Show)

instance FromJSON PrimitiveMode where
  parseJSON value = do
    number <- parseJSON value
    case number :: Int of
      0 -> pure Points
      1 -> pure Lines
      2 -> pure LineLoop
      3 -> pure LineStrip
      4 -> pure Triangles
      5 -> pure TriangleStrip
      6 -> pure TriangleFan
      i -> failWithContext "PrimitiveMode" i

-- | Represents the geometry to be rendered with the given material.
data Primitive = Primitive
  { attributes :: M.Map Attribute AccessorIx
    -- ^ A map where each key corresponds to a mesh attribute semantic and each
    -- value is the index of the accessor containing attribute's data.
  , indices :: Maybe AccessorIx
    -- ^ The index of the accessor that contains the vertex indices.
  , material :: Maybe MaterialIx
    -- ^ The index of the material to apply to this primitive when rendering.
  , mode :: PrimitiveMode
    -- ^ The topology type of primitives to render.
  , targets :: V.Vector (M.Map Attribute AccessorIx)
    -- ^ An array of morph targets.
  , extensions :: Maybe Object
    -- ^ A JSON object with extension-specific objects.
  , extras :: Maybe Value
    -- ^ Application-specific data.
  }
  deriving (Eq, Ord, Show)

instance FromJSON Primitive where
  parseJSON = withObject "Primitive" $ \v ->
    Primitive
      <$> v .:  "attributes"
      <*> v .:? "indices"
      <*> v .:? "material"
      <*> v .:? "mode"    .!= Triangles
      <*> v .:? "targets" .!= V.empty
      <*> v .:? "extensions"
      <*> v .:? "extras"

-- | Represents a set of primitives to be rendered. It's global transform is
-- defined by a node that references it.
data Mesh = Mesh
  { primitives :: V.Vector Primitive
    -- ^ The primitives, each defining geometry to be rendered.
  , weights :: S.Vector Float
    -- ^ The weights to be applied to the morph targets.
  , name :: Maybe T.Text
    -- ^ The name of the mesh.
  , extensions :: Maybe Object
    -- ^ A JSON object with extension-specific objects.
  , extras :: Maybe Value
    -- ^ Application-specific data.
  }
  deriving (Eq, Ord, Show)

instance FromJSON Mesh where
  parseJSON = withObject "Mesh" $ \v ->
    Mesh
      <$> v .:  "primitives"
      <*> v .:? "weights" .!= S.empty
      <*> v .:? "name"
      <*> v .:? "extensions"
      <*> v .:? "extras"

instance Index MeshIx (V.Vector Mesh) Mesh where
  get i vec = vec V.! i.value
  {-# INLINE get #-}

-- | Gets a specified attribute of a primitive, if available.
getAttribute :: Attribute -> Primitive -> Maybe AccessorIx
getAttribute attribute = M.lookup attribute . (.attributes)