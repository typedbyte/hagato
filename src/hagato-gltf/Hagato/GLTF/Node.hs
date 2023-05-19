{-# LANGUAGE OverloadedStrings #-}
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

-- | Represents a matrix or rotation/scale/translation transform.
data Transform
  = MatrixTransform Mat4
  | RotateScaleTranslate Quaternion Vec3 Vec3
  deriving (Eq, Ord, Show)

-- | A node in the node hierarchy. 
data Node = Node
  { camera     :: Maybe CameraIx
  , children   :: S.Vector NodeIx
  , skin       :: Maybe SkinIx
  , transform  :: Transform
  , mesh       :: Maybe MeshIx
  , weights    :: S.Vector Float
  , name       :: Maybe T.Text
  , extensions :: Maybe Object
  , extras     :: Maybe Value
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