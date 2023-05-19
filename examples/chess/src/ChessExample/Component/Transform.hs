{-# LANGUAGE TypeFamilies #-}
module ChessExample.Component.Transform where

-- apecs-effectful
import Apecs.Effectful (Component, Map, Storage)

-- hagato:with-core
import Hagato.Core (Quaternion, Vec3)

-- A transform describes the position of a mesh in the 3D world.
data Transform = Transform
  { translation :: !Vec3
  , rotation    :: !Quaternion
  , scale       :: !Vec3
  }

instance Component Transform where
  type Storage Transform = Map Transform
