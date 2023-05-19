module Hagato.Core.Math.Quaternion where

data Quaternion =
  Quaternion
    {-# UNPACK #-} !Float
    {-# UNPACK #-} !Float
    {-# UNPACK #-} !Float
    {-# UNPACK #-} !Float
  deriving
    (Eq, Ord, Read, Show)
