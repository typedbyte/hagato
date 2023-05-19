module Hagato.Core.Animation.Vec3 where

import Hagato.Core.Math.Vec3 (Vec3, scalarMultiply)

linear :: Vec3 -> Vec3 -> Float -> Vec3
linear p0 p1 =
  \t -> p0 + t `scalarMultiply` direction
    where
      direction = p1 - p0

quadraticBezier :: Vec3 -> Vec3 -> Vec3 -> Float -> Vec3
quadraticBezier p0 p1 p2 =
  \t ->
    let
      diff = (1-t)
    in
      ((diff * diff) `scalarMultiply` p0) +
      ((2 * diff * t) `scalarMultiply` p1) +
      ((t * t) `scalarMultiply` p2)

cubicBezier :: Vec3 -> Vec3 -> Vec3 -> Vec3 -> Float -> Vec3
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