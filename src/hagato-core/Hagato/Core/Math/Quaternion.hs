-----------------------------------------------------------------------------
-- |
-- Module      : Hagato.Core.Math.Quaternion
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Types and functions for handling rotations in 3D space.
-----------------------------------------------------------------------------
module Hagato.Core.Math.Quaternion where

-- | Represents a rotation in 3D space.
data Quaternion =
  Quaternion
    { w :: {-# UNPACK #-} !Float
    , x :: {-# UNPACK #-} !Float
    , y :: {-# UNPACK #-} !Float
    , z :: {-# UNPACK #-} !Float
    }
  deriving
    (Eq, Ord, Read, Show)
