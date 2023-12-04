-----------------------------------------------------------------------------
-- |
-- Module      : Hagato.Vulkan.Builder
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Types and functions for building values in an incremental manner.
-----------------------------------------------------------------------------
module Hagato.Vulkan.Builder
  ( Builder
  , defaultBuilder
  , get
  , put
  , modify
  , runBuilder
  , evalBuilder
  , execBuilder
  ) where

-- base
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Coerce            (coerce)

-- | A builder can create a value of type @a@ using its current state of type @s@.
newtype Builder s a = Builder (s -> IO (s, a))

instance Functor (Builder s) where
  fmap f (Builder g) =
    Builder $ \s -> do
      (newS, a) <- g s
      pure (newS, f a)

instance Applicative (Builder s) where
  pure a = Builder $ \s -> pure (s, a)
  
  Builder f <*> Builder g =
    Builder $ \s -> do
      (tempS, func) <- f s
      (newS, a) <- g tempS
      pure (newS, func a)

instance Monad (Builder s) where
  Builder f >>= g =
    Builder $ \s -> do
      (newS, a) <- f s
      coerce (g a) newS

instance MonadFail (Builder s) where
  fail = liftIO . fail

instance MonadIO (Builder s) where
  liftIO m =
    Builder $ \s -> do
      a <- m
      pure (s, a)

-- | A builder which produces an empty value.
defaultBuilder :: Builder s ()
defaultBuilder = pure ()

-- | Gets the current state of the builder.
get :: Builder s s
get = Builder $ \s -> pure (s, s)

-- | Updates the current state of the builder.
put :: s -> Builder s ()
put s = Builder $ \_ -> pure (s, ())

-- | Modifies the current state of the builder.
modify :: (s -> s) -> Builder s ()
modify f = do
  s <- get
  put (f s)

-- | Runs the builder, thus creating a final state of type @s@ and a value of type @a@.
runBuilder :: s -> Builder s a -> IO (s, a)
runBuilder = flip coerce

-- | A variation of 'runBuilder' which discards the final state.
evalBuilder :: s -> Builder s a -> IO a
evalBuilder s (Builder f) =
  fmap snd (f s)

-- | A variation of 'runBuilder' which discards the produced value.
execBuilder :: s -> Builder s a -> IO s
execBuilder s (Builder f) =
  fmap fst (f s)
