-----------------------------------------------------------------------------
-- |
-- Module      : Hagato.Core.Math.Plane
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Types and functions for handling 3D planes.
-----------------------------------------------------------------------------
module Hagato.Core.Math.Plane where

import Hagato.Core.Math.Ray  (Ray(..))
import Hagato.Core.Math.Vec3 (Vec3, dot, scalarMultiply)

-- | Represents a plane in 3D space.
data Plane = Plane
  { point :: !Vec3
    -- ^ A known point of the plane.
  , normal :: !Vec3
    -- ^ The normal vector of the plane.
  }

-- | Determines the intersection point between a ray and a plane.
intersectRay :: Ray -> Plane -> Vec3
intersectRay (Ray origin direction) (Plane point normal) =
  origin + t `scalarMultiply` direction
    where
      t = dot normal (point - origin)
        / dot normal direction