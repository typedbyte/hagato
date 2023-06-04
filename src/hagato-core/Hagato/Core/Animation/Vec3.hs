-----------------------------------------------------------------------------
-- |
-- Module      : Hagato.Core.Animation.Float
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Helper functions for creating 'Vec3'-based animations.
-----------------------------------------------------------------------------
module Hagato.Core.Animation.Vec3 where

import Hagato.Core.Math.Vec3 (Vec3, scalarMultiply)

-- | Represents a linear transition function @f(t)@ from one point to another.
--
-- Note that @t@ ranges from @0@ to @1@, but this is not checked by this function.
linear
  :: Vec3
  -- ^ Start point at @t = 0@.
  -> Vec3
  -- ^ End point at @t = 1@.
  -> (Float -> Vec3)
  -- ^ Transition function @f(t)@.
linear p0 p1 =
  \t -> p0 + t `scalarMultiply` direction
    where
      direction = p1 - p0

-- | Represents a quadratic bezier transition function @f(t)@ from one point to another.
--
-- Note that @t@ ranges from @0@ to @1@, but this is not checked by this function.
quadraticBezier
  :: Vec3
  -- ^ Start point at @t = 0@.
  -> Vec3
  -- ^ Control point.
  -> Vec3
  -- ^ End point at @t = 1@.
  -> (Float -> Vec3)
  -- ^ Transition function @f(t)@.
quadraticBezier p0 p1 p2 =
  \t ->
    let
      diff = (1-t)
    in
      ((diff * diff) `scalarMultiply` p0) +
      ((2 * diff * t) `scalarMultiply` p1) +
      ((t * t) `scalarMultiply` p2)

-- | Represents a cubic bezier transition function @f(t)@ from one point to another.
--
-- Note that @t@ ranges from @0@ to @1@, but this is not checked by this function.
cubicBezier
  :: Vec3
  -- ^ Start point at @t = 0@.
  -> Vec3
  -- ^ Control point 1.
  -> Vec3
  -- ^ Control point 2.
  -> Vec3
  -- ^ End point at @t = 1@.
  -> (Float -> Vec3)
  -- ^ Transition function @f(t)@.
cubicBezier p0 p1 p2 p3 =
  \t ->
    let
      diff  = (1-t)
      diff2 = diff * diff
      t2    = t * t
    in
      ((diff2 * diff) `scalarMultiply` p0) +
      ((3 * diff2 * t) `scalarMultiply` p1) +
      ((3 * diff * t2) `scalarMultiply` p2) +
      ((t2 * t) `scalarMultiply` p3)