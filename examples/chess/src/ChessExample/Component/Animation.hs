{-# LANGUAGE TypeFamilies #-}
module ChessExample.Component.Animation where

-- apecs-effectful
import Apecs.Effectful (Component, Map, Storage)

-- hagato:with-core
import Hagato.Core (Animation, Vec3)

-- A path describes the trajectory of a 3D object when it is animated. In this
-- example it is used to move the chess pieces over the board and make knights
-- jump.
newtype Path = Path (Animation Vec3)

instance Component Path where
  type Storage Path = Map Path

-- The forward component indicates that a 3D object is moving a specific distance
-- in the direction it is currently looking. In this example it is used for
-- smoothly zooming (hence: animate) the camera when using the mouse scroll wheel.
newtype Forward = Forward (Animation Float)

instance Component Forward where
  type Storage Forward = Map Forward

-- The fade out component indicates that an entity is about to disappear after
-- a specific amount of time. The corresponding animation yields no data, we just
-- check the completion of the animation and then remove the entity from the world.
newtype FadeOut = FadeOut (Animation ())

instance Component FadeOut where
  type Storage FadeOut = Map FadeOut
