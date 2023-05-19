module Hagato.Core.Animation.Float where

linear :: Float -> Float
linear = id

cubicEaseInOut :: Float -> Float
cubicEaseInOut =
  \t ->
    if t < 0.5 then
      4 * t * t * t
    else
      1 - ((-2 * t + 2) ^ (3 :: Int)) / 2

cubicEaseOut :: Float -> Float
cubicEaseOut =
  \t -> 1 - (1 - t) ^ (3 :: Int)