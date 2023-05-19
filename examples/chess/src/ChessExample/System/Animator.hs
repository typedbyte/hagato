module ChessExample.System.Animator where

-- apecs-effectful
import Apecs.Effectful (ECS, Not(Not), cmap)

-- base
import Prelude hiding (length)

-- effectful-core
import Effectful (Eff, (:>))

-- hagato:with-core
import Hagato.Core.Animation (Animation(done, value), step)
import Hagato.Core.Camera    (approach, position)
import Hagato.Core.Math.Vec3 (length)

import ChessExample.Component.Animation (FadeOut(FadeOut), Forward(Forward), Path(Path))
import ChessExample.Component.Camera    (Camera(Camera))
import ChessExample.Component.Transform (Transform(Transform))
import ChessExample.System.World        (All, World)

-- The animator system animates (and thus, moves) entities in the world and removes
-- animations from entities when they are finished.

-- Animates all entities in the world, based on the elapsed time since the last loop iteration.
animate :: ECS World :> es => Float -> Eff es ()
animate dt = do
  cmap updatePath
  cmap updateForward
  cmap updateFadeOut
    where
      -- animate entities that follow a specified path
      updatePath (Path animation, Transform _ r s)
        | animation.done =
            Left (Not @Path)
        | otherwise =
            let
              update = step dt animation
            in
              Right (Path update, Transform update.value r s)
      -- animate entities that move forward of backward (only the camera in this example)
      updateForward (Forward animation, Camera cam zoom)
        | animation.done =
            Left (Not @Forward)
        | otherwise =
            let
              update         = step dt animation
              movedCam       = approach update.value cam
              reduceDistance = if zoom > 0 then (-) else (+)
              travelDistance = length $ movedCam.position - cam.position
              newZoom        = reduceDistance zoom travelDistance
            in
              Right (Forward update, Camera movedCam newZoom)
      -- animate (or better: remove) entities that fade out after some time.
      updateFadeOut (FadeOut animation)
        | animation.done =
            Left (Not @All)
        | otherwise =
            Right (FadeOut $ step dt animation)
