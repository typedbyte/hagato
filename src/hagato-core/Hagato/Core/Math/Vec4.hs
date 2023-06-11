-----------------------------------------------------------------------------
-- |
-- Module      : Hagato.Core.Math.Vec4
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Types and functions for handling 4D vectors.
-----------------------------------------------------------------------------
module Hagato.Core.Math.Vec4
  ( Vec4(..)
  , normalize
  , dot
  , perspectiveDivide
  ) where

-- base
import Foreign.Ptr      (castPtr)
import Foreign.Storable (Storable(..))

import Hagato.Core.Math.Vec3 (Vec3(Vec3))

-- | Represents a 4D vector.
data Vec4 =
  Vec4
    { x :: {-# UNPACK #-} !Float
    , y :: {-# UNPACK #-} !Float
    , z :: {-# UNPACK #-} !Float
    , w :: {-# UNPACK #-} !Float
    }
  deriving
    (Eq, Ord, Read, Show)

instance Num Vec4 where
  (Vec4 x1 y1 z1 w1) + (Vec4 x2 y2 z2 w2) =
    Vec4
      (x1 + x2)
      (y1 + y2)
      (z1 + z2)
      (w1 + w2)
  {-# INLINE (+) #-}
  
  (Vec4 x1 y1 z1 w1) - (Vec4 x2 y2 z2 w2) =
    Vec4
      (x1 - x2)
      (y1 - y2)
      (z1 - z2)
      (w1 - w2)
  {-# INLINE (-) #-}
  
  (Vec4 x1 y1 z1 w1) * (Vec4 x2 y2 z2 w2) =
    Vec4
      (x1 * x2)
      (y1 * y2)
      (z1 * z2)
      (w1 * w2)
  {-# INLINE (*) #-}
  
  abs (Vec4 x y z w) =
    Vec4
      (abs x)
      (abs y)
      (abs z)
      (abs w)
  {-# INLINE abs #-}
  
  signum (Vec4 x y z w) =
    Vec4
      (signum x)
      (signum y)
      (signum z)
      (signum w)
  {-# INLINE signum #-}
  
  fromInteger i =
    Vec4 float float float float
      where
       float = fromInteger i
  {-# INLINE fromInteger #-}

instance Storable Vec4 where
  sizeOf _ = 16
  {-# INLINE sizeOf #-}
  
  alignment _ = 4
  {-# INLINE alignment #-}
  
  peek ptr =
    let
      floatPtr = castPtr ptr
    in do
      x <- peek        floatPtr
      y <- peekByteOff floatPtr  4
      z <- peekByteOff floatPtr  8
      w <- peekByteOff floatPtr 12
      pure $ Vec4 x y z w
  {-# INLINE peek #-}
  
  poke ptr (Vec4 x y z w) =
    let
      floatPtr = castPtr ptr
    in do
      poke        floatPtr    x
      pokeByteOff floatPtr  4 y
      pokeByteOff floatPtr  8 z
      pokeByteOff floatPtr 12 w
  {-# INLINE poke #-}

-- | Transforms a vector into one with unit length and same direction.
normalize :: Vec4 -> Vec4
normalize (Vec4 x y z w) =
  let
    l = sqrt (x*x + y*y + z*z + w*w)
  in
    Vec4 (x/l) (y/l) (z/l) (w/l)
{-# INLINE normalize #-}

-- | Determines the dot product of two vectors.
dot :: Vec4 -> Vec4 -> Float
dot (Vec4 x1 y1 z1 w1) (Vec4 x2 y2 z2 w2) =
  x1*x2 + y1*y2 + z1*z2 + w1*w2
{-# INLINE dot #-}

-- | Converts a 4D vector into a 3D vector by dividing @x\/y\/z@ by @w@.
perspectiveDivide :: Vec4 -> Vec3
perspectiveDivide (Vec4 x y z w) =
  Vec3 (x/w) (y/w) (z/w)
{-# INLINE perspectiveDivide #-}