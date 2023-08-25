{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hagato.GLTF.Camera
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Types and functions for handling cameras found in glTF files.
-----------------------------------------------------------------------------
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

-- | Represents the properties needed to create a perspective projection matrix.
data PerspectiveData = PerspectiveData
  { aspectRatio :: Maybe Float
    -- ^ The floating-point aspect ratio of the field of view.
  , yFov :: Float
    -- ^ The floating-point vertical field of view in radians.
  , zFar :: Maybe Float
    -- ^ The floating-point distance to the far clipping plane.
  , zNear :: Float
    -- ^ The floating-point distance to the near clipping plane.
  , extensions :: Maybe Object
    -- ^ A JSON object with extension-specific objects.
  , extras :: Maybe Value
    -- ^ Application-specific data.
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

-- | Represents the properties needed to create an orthographic projection matrix.
data OrthographicData = OrthographicData
  { xMag :: Float
    -- ^ The floating-point horizontal magnification of the view.
  , yMag :: Float
    -- ^ The floating-point vertical magnification of the view.
  , zFar :: Float
    -- ^ The floating-point distance to the far clipping plane.
  , zNear :: Float
    -- ^ The floating-point distance to the near clipping plane.
  , extensions :: Maybe Object
    -- ^ A JSON object with extension-specific objects.
  , extras :: Maybe Value
    -- ^ Application-specific data.
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

-- | Represents a camera. A node may reference a camera to apply a transform
-- to place the camera in the scene.
data Camera = Camera
  { projection :: Projection
    -- ^ The properties needed to create a projection matrix.
  , name :: Maybe T.Text
    -- ^ The name of the camera.
  , extensions :: Maybe Object
    -- ^ A JSON object with extension-specific objects.
  , extras :: Maybe Value
    -- ^ Application-specific data.
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