{-# LANGUAGE OverloadedStrings #-}
module Hagato.GLTF.Camera where

-- aeson
import Data.Aeson (FromJSON(..), Object, Value, (.:), (.:?), withObject)

-- base
import Control.Applicative ((<|>))

-- text
import Data.Text qualified as T

-- vector
import Data.Vector qualified as V

import Hagato.GLTF.Index (CameraIx(value), Index, get)

-- | A perspective camera containing properties to create a perspective projection matrix.
data PerspectiveData = PerspectiveData
  { aspectRatio :: Maybe Float
  , yFov        :: Float
  , zFar        :: Maybe Float
  , zNear       :: Float
  , extensions  :: Maybe Object
  , extras      :: Maybe Value
  }
  deriving (Eq, Ord, Show)

instance FromJSON PerspectiveData where
  parseJSON = withObject "PerspectiveData" $ \v ->
    PerspectiveData
      <$> v .:? "aspectRatio"
      <*> v .:  "yfov"
      <*> v .:? "zfar"
      <*> v .:  "znear"
      <*> v .:? "extensions"
      <*> v .:? "extras"

-- | An orthographic camera containing properties to create an orthographic projection matrix.
data OrthographicData = OrthographicData
  { xMag       :: Float
  , yMag       :: Float
  , zFar       :: Float
  , zNear      :: Float
  , extensions :: Maybe Object
  , extras     :: Maybe Value
  }
  deriving (Eq, Ord, Show)

instance FromJSON OrthographicData where
  parseJSON = withObject "OrthographicData" $ \v ->
    OrthographicData
      <$> v .:  "xmag"
      <*> v .:  "ymag"
      <*> v .:  "zfar"
      <*> v .:  "znear"
      <*> v .:? "extensions"
      <*> v .:? "extras"

-- | Represents a perspective or orthographic projection.
data Projection
  = Perspective PerspectiveData
  | Orthographic OrthographicData
  deriving (Eq, Ord, Show)

-- | A camera's projection. A node may reference a camera to apply a transform to place the camera in the scene.
data Camera = Camera
  { projection :: Projection
  , name       :: Maybe T.Text
  , extensions :: Maybe Object
  , extras     :: Maybe Value
  }
  deriving (Eq, Ord, Show)

instance FromJSON Camera where
  parseJSON = withObject "Camera" $ \v ->
    Camera
      <$> toProjection v
      <*> v .:? "name"
      <*> v .:? "extensions"
      <*> v .:? "extras"
    where
      toProjection v =
        (Perspective  <$> v .: "perspective" ) <|>
        (Orthographic <$> v .: "orthographic")

instance Index CameraIx (V.Vector Camera) Camera where
  get i vec = vec V.! i.value
  {-# INLINE get #-}