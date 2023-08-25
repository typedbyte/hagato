{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hagato.GLTF.Node
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Types and functions for handling nodes found in glTF files.
-----------------------------------------------------------------------------
module Hagato.GLTF.Node where

-- aeson
import Data.Aeson (FromJSON(..), Object, Value, (.:), (.:?), (.!=), withObject)

-- base
import Control.Applicative ((<|>))

-- hagato:with-core
import Hagato.Core (Mat4, Quaternion(Quaternion), Vec3(Vec3), columnMajor)

-- text
import Data.Text qualified as T

-- vector
import Data.Vector          qualified as V
import Data.Vector.Storable qualified as S

import Hagato.GLTF.Index (CameraIx, Index, MeshIx, NodeIx(value), SkinIx, get)

-- | Represents a matrix or rotation\/scale\/translation transform.
data Transform
  = MatrixTransform Mat4
  | RotateScaleTranslate Quaternion Vec3 Vec3
  deriving (Eq, Ord, Show)

-- | Represents a node in the node hierarchy.
data Node = Node
  { camera :: Maybe CameraIx
    -- ^ The index of the camera referenced by this node.
  , children :: S.Vector NodeIx
    -- ^ The indices of this node's children.
  , skin :: Maybe SkinIx
    -- ^ The index of the skin referenced by this node.
  , transform :: Transform
    -- ^ The transform applied to this node.
  , mesh :: Maybe MeshIx
    -- ^ The index of the mesh in this node.
  , weights :: S.Vector Float
    -- ^ The weights of the instantiated morph target.
  , name :: Maybe T.Text
    -- ^ The name of the node.
  , extensions :: Maybe Object
    -- ^ A JSON object with extension-specific objects.
  , extras :: Maybe Value
    -- ^ Application-specific data.
  }
  deriving (Eq, Ord, Show)

instance FromJSON Node where
  parseJSON = withObject "Node" $ \v ->
    Node
      <$> v .:? "camera"
      <*> v .:? "children" .!= S.empty
      <*> v .:? "skin"
      <*> toTransform v
      <*> v .:? "mesh"
      <*> v .:? "weights" .!= S.empty
      <*> v .:? "name"
      <*> v .:? "extensions"
      <*> v .:? "extras"
    where
      toTransform v = matrixParser v <|> rstParser v
      matrixParser v = do
        [ m00, m10, m20, m30,
          m01, m11, m21, m31,
          m02, m12, m22, m32,
          m03, m13, m23, m33 ] <- v .: "matrix"
        pure $
          MatrixTransform $
            columnMajor
              m00 m10 m20 m30
              m01 m11 m21 m31
              m02 m12 m22 m32
              m03 m13 m23 m33
      rstParser v = do
        (rx,ry,rz,rw) <- v .:? "rotation"    .!= (0, 0, 0, 1)
        (sx,sy,sz)    <- v .:? "scale"       .!= (1, 1, 1)
        (tx,ty,tz)    <- v .:? "translation" .!= (0, 0, 0)
        pure $
          RotateScaleTranslate
            (Quaternion rw rx ry rz)
            (Vec3 sx sy sz)
            (Vec3 tx ty tz)

instance Index NodeIx (V.Vector Node) Node where
  get i vec = vec V.! i.value
  {-# INLINE get #-}