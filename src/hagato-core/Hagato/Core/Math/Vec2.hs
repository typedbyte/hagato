-----------------------------------------------------------------------------
-- |
-- Module      : Hagato.Core.Math.Vec2
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Types and functions for handling 2D vectors.
-----------------------------------------------------------------------------
module Hagato.Core.Math.Vec2 where

-- base
import Foreign.Ptr      (castPtr)
import Foreign.Storable (Storable(..))
import Prelude   hiding (length)

-- | Represents a 2D vector.
data Vec2 =
  Vec2
    { x :: {-# UNPACK #-} !Float
    , y :: {-# UNPACK #-} !Float
    }
  deriving
    (Eq, Ord, Read, Show)

instance Num Vec2 where
  (Vec2 x1 y1) + (Vec2 x2 y2) =
    Vec2
      (x1 + x2)
      (y1 + y2)
  {-# INLINE (+) #-}
  
  (Vec2 x1 y1) - (Vec2 x2 y2) =
    Vec2
      (x1 - x2)
      (y1 - y2)
  {-# INLINE (-) #-}
  
  (Vec2 x1 y1) * (Vec2 x2 y2) =
    Vec2
      (x1 * x2)
      (y1 * y2)
  {-# INLINE (*) #-}
  
  abs (Vec2 x y) =
    Vec2
      (abs x)
      (abs y)
  {-# INLINE abs #-}
  
  signum (Vec2 x y) =
    Vec2
      (signum x)
      (signum y)
  {-# INLINE signum #-}
  
  fromInteger i =
    Vec2 float float
      where
       float = fromInteger i
  {-# INLINE fromInteger #-}

instance Storable Vec2 where
  sizeOf _ = 8
  {-# INLINE sizeOf #-}
  
  alignment _ = 4
  {-# INLINE alignment #-}
  
  peek ptr =
    let
      floatPtr = castPtr ptr
    in do
      x <- peek        floatPtr
      y <- peekByteOff floatPtr 4
      pure $ Vec2 x y
  {-# INLINE peek #-}
  
  poke ptr (Vec2 x y) =
    let
      floatPtr = castPtr ptr
    in do
      poke        floatPtr   x
      pokeByteOff floatPtr 4 y
  {-# INLINE poke #-}

-- | Determines the length of a vector.
length :: Vec2 -> Float
length (Vec2 x y) =
  sqrt (x*x + y*y)
{-# INLINE length #-}

-- | Transforms a vector into one with unit length and same direction.
normalize :: Vec2 -> Vec2
normalize vec@(Vec2 x y) =
  let
    l = length vec
  in
    Vec2 (x/l) (y/l)
{-# INLINE normalize #-}

-- | Determines the scalar product of two vectors.
dot :: Vec2 -> Vec2 -> Float
dot (Vec2 x1 y1) (Vec2 x2 y2) =
  x1*x2 + y1*y2
{-# INLINE dot #-}

-- | Multiplies a scalar by a vector.
scalarMultiply :: Float -> Vec2 -> Vec2
scalarMultiply scalar (Vec2 x y) =
  Vec2
    (scalar * x)
    (scalar * y)
{-# INLINE scalarMultiply #-}
