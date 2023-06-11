-----------------------------------------------------------------------------
-- |
-- Module      : Hagato.Core.Math.Vec2
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Types and functions for handling 3D vectors.
-----------------------------------------------------------------------------
module Hagato.Core.Math.Vec3
  ( Vec3(..)
  , length
  , normalize
  , cross
  , dot
  , scalarMultiply
  , between
  ) where

-- base
import Foreign.Ptr      (castPtr)
import Foreign.Storable (Storable(..))
import Prelude   hiding (length)

-- | Represents a 3D vector.
data Vec3 =
  Vec3
    { x :: {-# UNPACK #-} !Float
    , y :: {-# UNPACK #-} !Float
    , z :: {-# UNPACK #-} !Float
    }
  deriving
    (Eq, Ord, Read, Show)

instance Num Vec3 where
  (Vec3 x1 y1 z1) + (Vec3 x2 y2 z2) =
    Vec3
      (x1 + x2)
      (y1 + y2)
      (z1 + z2)
  {-# INLINE (+) #-}
  
  (Vec3 x1 y1 z1) - (Vec3 x2 y2 z2) =
    Vec3
      (x1 - x2)
      (y1 - y2)
      (z1 - z2)
  {-# INLINE (-) #-}
  
  (Vec3 x1 y1 z1) * (Vec3 x2 y2 z2) =
    Vec3
      (x1 * x2)
      (y1 * y2)
      (z1 * z2)
  {-# INLINE (*) #-}
  
  abs (Vec3 x y z) =
    Vec3
      (abs x)
      (abs y)
      (abs z)
  {-# INLINE abs #-}
  
  signum (Vec3 x y z) =
    Vec3
      (signum x)
      (signum y)
      (signum z)
  {-# INLINE signum #-}
  
  fromInteger i =
    Vec3 float float float
      where
       float = fromInteger i
  {-# INLINE fromInteger #-}

instance Storable Vec3 where
  sizeOf _ = 12
  {-# INLINE sizeOf #-}
  
  alignment _ = 4
  {-# INLINE alignment #-}
  
  peek ptr =
    let
      floatPtr = castPtr ptr
    in do
      x <- peek        floatPtr
      y <- peekByteOff floatPtr 4
      z <- peekByteOff floatPtr 8
      pure $ Vec3 x y z
  {-# INLINE peek #-}
  
  poke ptr (Vec3 x y z) =
    let
      floatPtr = castPtr ptr
    in do
      poke        floatPtr   x
      pokeByteOff floatPtr 4 y
      pokeByteOff floatPtr 8 z
  {-# INLINE poke #-}

-- | Determines the length of a vector.
length :: Vec3 -> Float
length (Vec3 x y z) =
  sqrt (x*x + y*y + z*z)
{-# INLINE length #-}

-- | Transforms a vector into one with unit length and same direction.
normalize :: Vec3 -> Vec3
normalize vec@(Vec3 x y z) =
  let
    l = length vec
  in
    Vec3 (x/l) (y/l) (z/l)
{-# INLINE normalize #-}

-- | Determines the cross product of two vectors.
cross :: Vec3 -> Vec3 -> Vec3
cross (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) =
  Vec3
    (y1 * z2 - z1 * y2)
    (z1 * x2 - x1 * z2)
    (x1 * y2 - y1 * x2)
{-# INLINE cross #-}

-- | Determines the dot product of two vectors.
dot :: Vec3 -> Vec3 -> Float
dot (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) =
  x1*x2 + y1*y2 + z1*z2
{-# INLINE dot #-}

-- | Multiplies a scalar by a vector.
scalarMultiply :: Float -> Vec3 -> Vec3
scalarMultiply scalar (Vec3 x y z) =
  Vec3
    (scalar * x)
    (scalar * y)
    (scalar * z)
{-# INLINE scalarMultiply #-}

-- | Determines the point which lies in the middle of two points.
between :: Vec3 -> Vec3 -> Vec3
between p1 p2 =
  0.5 `scalarMultiply` (p1 + p2)
{-# INLINE between #-}