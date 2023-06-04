-----------------------------------------------------------------------------
-- |
-- Module      : Hagato.Core.Animation.Float
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Helper functions for creating 'Float'-based animations.
-----------------------------------------------------------------------------
module Hagato.Core.Animation.Float where

-- | Represents the function @f(t) = t@.
--
-- Note that @t@ ranges from @0@ to @1@, but this is not checked by this function.
linear :: Float -> Float
linear = id

-- | Represents a cubic ease-in-out function @f(t)@.
--
-- Note that @t@ ranges from @0@ to @1@, but this is not checked by this function.
cubicEaseInOut :: Float -> Float
cubicEaseInOut =
  \t ->
    if t < 0.5 then
      4 * t * t * t
    else
      1 - ((-2 * t + 2) ^ (3 :: Int)) / 2

-- | Represents a cubic ease-out function @f(t)@.
--
-- Note that @t@ ranges from @0@ to @1@, but this is not checked by this function.
cubicEaseOut :: Float -> Float
cubicEaseOut =
  \t -> 1 - (1 - t) ^ (3 :: Int)