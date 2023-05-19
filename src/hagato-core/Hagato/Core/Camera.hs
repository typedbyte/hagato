module Hagato.Core.Camera where

-- base
import Prelude hiding (length)

import Hagato.Core.Extra     (clamp)
import Hagato.Core.Math.Mat4 (Mat4, inverseTransform, rowMajor)
import Hagato.Core.Math.Vec2 (Vec2(..))
import Hagato.Core.Math.Vec3 (Vec3(Vec3), cross, dot, length, normalize, scalarMultiply)

data Frustum
  = Perspective !Float !(Maybe Float) !Float !Float
  | Orthographic !Float !Float !Float !Float
  deriving (Eq, Ord, Read, Show)

data Camera = Camera
  { position :: !Vec3
  , target   :: !Vec3
  , up       :: !Vec3
  , frustum  :: Frustum
  }
  deriving (Eq, Ord, Read, Show)

setAspectRatio :: Vec2 -> Camera -> Camera
setAspectRatio viewport camera =
  camera
    { frustum =
        case camera.frustum of
          Perspective near far fov _ ->
            Perspective near far fov (viewport.x / viewport.y)
          Orthographic near far fov _ ->
            Orthographic near far fov (viewport.x / viewport.y)
    }

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

inverseViewMatrix :: Camera -> Mat4
inverseViewMatrix = inverseFromView . viewMatrix

inverseFromView :: Mat4 -> Mat4
inverseFromView = inverseTransform

arcBall
  :: Vec3
  -> Vec3
  -> Float
  -> Float
  -> Float
  -> Frustum
  -> Camera
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

orbit :: Float -> Float -> Camera -> Camera
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
