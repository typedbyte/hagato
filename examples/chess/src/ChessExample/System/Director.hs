module ChessExample.System.Director where

-- apecs-effectful
import Apecs.Effectful (ECS, cmap, get, global)

-- base
import Control.Monad  (guard)
import Prelude hiding (length)

-- chessica
import Chess (Position, mkPosition)

-- hagato:with-core
import Hagato.Core.Animation       (deltas, once)
import Hagato.Core.Animation.Float (cubicEaseOut)
import Hagato.Core.Camera          (Camera(position, target), orbit, setAspectRatio)
import Hagato.Core.Extra           (clamp)
import Hagato.Core.Math.Plane      (Plane(Plane), intersectRay)
import Hagato.Core.Math.Vec2       (Vec2(Vec2))
import Hagato.Core.Math.Vec3       (Vec3(..), length)

-- hagato:with-vulkan
import Hagato.Vulkan.Camera (ray)

-- effectful-core
import Effectful (Eff, (:>))

import ChessExample.Component.Animation (Forward(Forward))
import ChessExample.Component.Camera    (Camera(Camera))
import ChessExample.Component.Screen    (Screen(Screen))
import ChessExample.System.World        (World)

-- The director system is concerned with the camera movement and all stuff that
-- is related to interacting with objects through the current field of view.

-- Determines which chess board position is associated with the specified cursor position.
target :: ECS World :> es => Vec2 -> Eff es (Maybe Position)
target cursor = do
  Camera cam _        <- get global
  Screen _ viewport _ <- get global
  let
    -- In the generic case, we would cast a ray and check if it hits one of the
    -- objects in 3D space in order to determine what we have targeted.
    -- We cheat a little bit here to make our lives easier: We do cast a ray, but
    -- we do not check it against some 3D object. Instead, we just calculate the
    -- intersection between the ray and the plane of the chess board, and by knowing
    -- the size of one board field (because we modelled it in Blender that way), we
    -- can map the intersection point to a board position by pure maths.
    line   = ray cursor viewport cam
    plane  = Plane 0 (Vec3 0 0 1)
    point  = intersectRay line plane
    row    = floor $ point.y + 4
    column = floor $ point.x + 4
  pure $ do
    guard (cam.position.z > 0) -- cannot select field from below the chess board
    mkPosition row column

-- Zooms the camera in (+) or out (-) for a specific distance.
zoom :: ECS World :> es => Float -> Eff es ()
zoom distance =
  cmap $ \(Camera cam nowZoom) ->
    let
      toTarget = length $ cam.target - cam.position
      minZoom  = toTarget - 20
      maxZoom  = toTarget - 3
      newZoom  = clamp minZoom maxZoom $ nowZoom + distance
      forward  = once 1 $ deltas $ (* newZoom) . cubicEaseOut
    in
      (Camera cam newZoom, Forward forward)

-- Marks the screen component dirty because of a resize.
resize :: ECS World :> es => Vec2 -> Eff es ()
resize viewport =
  cmap $ \(Camera cam nowZoom, Screen cursor _ _) ->
    ( Camera (setAspectRatio viewport cam) nowZoom
    , Screen cursor viewport True
    )

-- Activates (Just cursor) or deactivates (Nothing) the rotation of the scene.
rotate :: ECS World :> es => Maybe Vec2 -> Eff es ()
rotate cursor =
  cmap $ \(Screen _ viewport dirty) ->
    Screen cursor viewport dirty

-- If rotation is activated, moving the cursor means rotating the scene.
moveCursor :: ECS World :> es => Vec2 -> Eff es ()
moveCursor newCursor =
  cmap $ \(Camera cam nowZoom, Screen cursor viewport dirty) ->
    case cursor of
      Nothing ->
        Left ()
      Just oldCursor ->
        let
          Vec2 dx dy = oldCursor - newCursor
          Vec2 w h   = viewport
          deltaTheta = (dy / h) * pi
          deltaPhi   = (dx / w) * 2 * pi
          newCam     = orbit deltaTheta deltaPhi cam
        in
          Right
            ( Camera newCam nowZoom
            , Screen (Just newCursor) viewport dirty
            )
