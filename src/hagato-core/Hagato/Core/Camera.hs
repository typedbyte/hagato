-----------------------------------------------------------------------------
-- |
-- Module      : Hagato.Core.Camera
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Types and functions for creating and manipulating 3D cameras, which allow
-- the observation of a world from a specific viewpoint.
-----------------------------------------------------------------------------
module Hagato.Core.Camera
  ( -- * Camera Handling
    Camera(..)
  , Frustum(..)
  , arcBall
  , orbit
  , approach
  , setAspectRatio
    -- * Camera Matrix
  , viewMatrix
  , inverseViewMatrix
  , inverseFromView
  ) where

-- base
import Prelude hiding (length)

import Hagato.Core.Extra     (clamp)
import Hagato.Core.Math.Mat4 (Mat4, inverseTransform, rowMajor)
import Hagato.Core.Math.Vec2 (Vec2(..))
import Hagato.Core.Math.Vec3 (Vec3(Vec3), cross, dot, length, normalize, scalarMultiply)

-- | Represents the view frustum of a camera.
data Frustum
  = Perspective
      !Float
      -- ^ Near plane distance.
      !(Maybe Float)
      -- ^ Far plane distance. 'Nothing' indicates an infinite distance.
      !Float
      -- ^ Vertical field of view in radians.
      !Float
      -- ^ Aspect ratio (width / height).
  | Orthographic
      !Float
      -- ^ Near plane distance.
      !Float
      -- ^ Far plane distance.
      !Float
      -- ^ Vertical field of view in radians.
      !Float
      -- ^ Aspect ratio (width / height).
  deriving (Eq, Ord, Read, Show)

-- | Represents a camera in 3D world space.
data Camera = Camera
  { position :: !Vec3
    -- ^ Position of the camera.
  , target :: !Vec3
    -- ^ Point the camera is looking at.
  , up :: !Vec3
    -- ^ Direction pointing upwards.
  , frustum :: !Frustum
    -- ^ View frustum of the camera.
  }
  deriving (Eq, Ord, Read, Show)

-- | Updates the aspect ratio of the camera's view frustum.
setAspectRatio
  :: Vec2   -- ^ Width and height that define the aspect ratio.
  -> Camera -- ^ Old camera.
  -> Camera -- ^ New camera.
setAspectRatio viewport camera =
  camera
    { frustum =
        case camera.frustum of
          Perspective near far fov _ ->
            Perspective near far fov (viewport.x / viewport.y)
          Orthographic near far fov _ ->
            Orthographic near far fov (viewport.x / viewport.y)
    }

-- | Builds the view matrix of the camera.
viewMatrix :: Camera -> Mat4
viewMatrix Camera{position, target, up} =
  let
    newXAxis@(Vec3 xx xy xz) = normalize $ cross negZAxis up
    newYAxis@(Vec3 yx yy yz) = cross newXAxis negZAxis
    negZAxis@(Vec3 zx zy zz) = normalize $ target - position
    xd = - dot newXAxis position
    yd = - dot newYAxis position
    zd =   dot negZAxis position
  in
    rowMajor
        xx    xy    xz  xd
        yx    yy    yz  yd
      (-zx) (-zy) (-zz) zd
         0     0     0   1

-- | Builds the inverse view matrix of the camera.
--
-- For performance reasons, use 'inverseFromView' instead if you have already
-- obtained the view matrix (see 'viewMatrix').
inverseViewMatrix :: Camera -> Mat4
inverseViewMatrix = inverseFromView . viewMatrix

-- | Builds the inverse view matrix from the given view matrix.
inverseFromView :: Mat4 -> Mat4
inverseFromView = inverseTransform

-- | Smart constructor for a camera based on the rotation around a point the
-- camera is looking at.
arcBall
  :: Vec3
  -- ^ Point the camera is looking at.
  -> Vec3
  -- ^ Direction pointing upwards.
  -> Float
  -- ^ Distance between the camera and the point it is looking at (radius).
  -> Float
  -- ^ Horizontal rotation angle in radians (angle between x and y axes).
  -> Float
  -- ^ Vertical rotation angle in radians (angle between z axis and x-y-plane).
  -> Frustum
  -- ^ View frustum of the camera.
  -> Camera
  -- ^ The resulting camera.
arcBall target@(Vec3 x y z) up r theta phi frustum =
  Camera
    { position = position
    , target   = target
    , up       = up
    , frustum  = frustum
    }
  where
    rSinTheta = r * sin theta
    position =
      Vec3
        ( x + rSinTheta * cos phi )
        ( y + rSinTheta * sin phi )
        ( z + r * cos theta )

-- | Rotates the camera around the point it is looking at.
orbit
  :: Float  -- ^ Horizontal rotation angle delta in radians (angle between x and y axes).
  -> Float  -- ^ Vertical rotation angle delta in radians (angle between z axis and x-y-plane).
  -> Camera -- ^ Old camera.
  -> Camera -- ^ New camera.
orbit deltaTheta deltaPhi Camera{position, target, up, frustum} =
  let
    vec@(Vec3 x y z) = position - target
    r                = length vec
    phi              = atan2 y x + deltaPhi
    thetaNow         = acos (z/r)
    thetaDesired     = thetaNow + deltaTheta
    epsilon          = 1e-6
    theta            = clamp epsilon (pi-epsilon) thetaDesired
  in
    arcBall target up r theta phi frustum

-- | Moves the camera forward (positive distance) of backwards (negative distance)
-- in the direction it is currently looking.
approach :: Float -> Camera -> Camera
approach distance camera =
  if distance < target then
    camera { position = newPosition }
  else
    camera
  where
    direction   = camera.target - camera.position
    unit        = normalize direction
    target      = length direction
    newPosition = camera.position + distance `scalarMultiply` unit
