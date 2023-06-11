-----------------------------------------------------------------------------
-- |
-- Module      : Hagato.Core.Math.Ray
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Types and functions for handling 3D rays.
-----------------------------------------------------------------------------
module Hagato.Core.Math.Ray where

import Hagato.Core.Math.Vec3 (Vec3)

-- | Represents a ray in 3D space.
data Ray = Ray
  { origin :: !Vec3
    -- ^ The starting point of the ray.
  , direction :: !Vec3
    -- ^ The direction of the ray.
  }
  deriving (Eq, Ord, Read, Show)