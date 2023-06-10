{-# LANGUAGE AllowAmbiguousTypes #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hagato.Core.Extra
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Utility functions.
-----------------------------------------------------------------------------
module Hagato.Core.Extra where

-- base
import Control.Monad.IO.Class (MonadIO, liftIO)
import Foreign.Ptr            (Ptr)
import Foreign.Storable       qualified as S

-- | Limits a value to a certain range.
clamp
  :: Ord a
  => a -- ^ Inclusive lower bound.
  -> a -- ^ Inclusive upper bound.
  -> a -- ^ Value to be limited.
  -> a -- ^ Value within the bounds.
clamp low high value
  | value <= low  = low
  | value >= high = high
  | otherwise     = value
{-# INLINE clamp #-}

-- | Writes the given value to the given memory location.
poke :: (S.Storable a, MonadIO m) => Ptr a -> a -> m ()
poke ptr value = liftIO $ S.poke ptr value
{-# INLINE poke #-}

-- | Computes the storage requirements of the type @a@ in bytes.
-- Intended to be used with the @TypeApplications@ language extension.
sizeOf :: forall a b. (S.Storable a, Num b) => b
sizeOf = fromIntegral $ S.sizeOf (undefined :: a)
{-# INLINE sizeOf #-}
