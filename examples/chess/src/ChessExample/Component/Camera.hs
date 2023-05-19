{-# LANGUAGE TypeFamilies #-}
module ChessExample.Component.Camera where

-- apecs-effectful
import Apecs.Effectful (Component, Storage, Unique)

-- hagato:with-core
import Hagato.Core qualified as Core

-- The camera component describes a virtual camera placed in 3D space. The artist
-- systems uses it to render a scene from a specific viewpoint.
data Camera = Camera
  { value :: !Core.Camera
  , zoom  :: !Float
  }

instance Component Camera where
  type Storage Camera = Unique Camera
