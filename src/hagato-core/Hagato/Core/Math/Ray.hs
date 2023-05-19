module Hagato.Core.Math.Ray where

import Hagato.Core.Math.Vec3 (Vec3)

data Ray = Ray
  { origin    :: !Vec3
  , direction :: !Vec3
  }
  deriving (Eq, Ord, Read, Show)