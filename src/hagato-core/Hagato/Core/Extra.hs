{-# LANGUAGE AllowAmbiguousTypes #-}
module Hagato.Core.Extra where

-- base
import Control.Monad.IO.Class (MonadIO, liftIO)
import Foreign.Ptr            (Ptr)
import Foreign.Storable       qualified as S

clamp :: Ord a => a -> a -> a -> a
clamp low high value
  | value <= low  = low
  | value >= high = high
  | otherwise     = value
{-# INLINE clamp #-}

poke :: (S.Storable a, MonadIO m) => Ptr a -> a -> m ()
poke ptr value = liftIO $ S.poke ptr value
{-# INLINE poke #-}

sizeOf :: forall a b. (S.Storable a, Num b) => b
sizeOf = fromIntegral $ S.sizeOf (undefined :: a)
{-# INLINE sizeOf #-}
