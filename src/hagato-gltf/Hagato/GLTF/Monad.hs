{-# LANGUAGE LambdaCase #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hagato.GLTF.Monad
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- A monad tranformer for loading data from a glTF file.
-----------------------------------------------------------------------------
module Hagato.GLTF.Monad
  ( -- * glTF Monad Transfomer
    GLTFT
  , runGLTFT
    -- * glTF Operations
  , askT
  , liftT
  , fetchT
  , getNodeT
  , loadAccessorIxT
  , loadAccessorT
  , loadBufferIxT
  , loadBufferT
  , loadBufferViewIxT
  , loadBufferViewT
  , loadImageT
  , loadURIT
  ) where

-- base
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Coerce            (coerce)
import Prelude hiding         (lookup)

-- bytestring
import Data.ByteString qualified as BS

-- text
import Data.Text (Text)

import Hagato.GLTF.Accessor     qualified as A
import Hagato.GLTF.Transmission qualified as T
import Hagato.GLTF.BufferView   qualified as V
import Hagato.GLTF.Image        (Image)
import Hagato.GLTF.Index        qualified as Ix
import Hagato.GLTF.Asset        (Asset)
import Hagato.GLTF.Buffer       (Buffer)
import Hagato.GLTF.Exception    (GLTFException(..), throw)
import Hagato.GLTF.Node         (Node)
import Hagato.GLTF.URI          qualified as URI

-- | The glTF monad transformer, which allows to load data from a glTF
-- transmission and which takes care of caching glTF buffers if they are
-- processed multiple times.
newtype GLTFT m a = GLTFT (T.Transmission -> T.Cache -> m (a, T.Cache))

instance Monad m => Applicative (GLTFT m) where
  pure a =
    GLTFT $ \_ cache ->
      pure (a, cache)
  {-# INLINE pure #-}
  GLTFT f <*> GLTFT g =
    GLTFT $ \tm cache -> do
      (h, cache')  <- f tm cache
      (a, cache'') <- g tm cache'
      pure (h a, cache'')
  {-# INLINE (<*>) #-}

instance Functor m => Functor (GLTFT m) where
  fmap f (GLTFT g) =
    GLTFT $ \tm cache ->
      fmap
        ( \(a, c) -> (f a, c) )
        ( g tm cache )
  {-# INLINE fmap #-}

instance Monad m => Monad (GLTFT m) where
  GLTFT f >>= g =
    GLTFT $ \tm cache -> do
      (a, cache') <- f tm cache
      coerce (g a) tm cache'
  {-# INLINE (>>=) #-}

instance MonadIO m => MonadIO (GLTFT m) where
  liftIO m =
    GLTFT $ \_ cache ->
      fmap
        ( \a -> (a, cache) )
        ( liftIO m )
  {-# INLINE liftIO #-}

instance MonadFail m => MonadFail (GLTFT m) where
  fail txt = GLTFT $ \_ _ -> fail txt
  {-# INLINE fail #-}

-- | Runs the glTF monad transformer on a specific glTF transmission and extracts
-- the final value from it.
runGLTFT :: MonadIO m => T.Source -> GLTFT m a -> m a
runGLTFT source (GLTFT f) =
  T.parseSource source >>= \case
    Left err ->
      throw $ InvalidFormat err
    Right tm ->
      fst <$> f tm T.empty

-- | Fetches the glTF transmission which is currently processed.
askT :: Applicative m => GLTFT m T.Transmission
askT = GLTFT $ \tm cache -> pure (tm, cache)
{-# INLINE askT #-}

-- | Lifts a computation into the 'GLTFT' monad.
liftT :: Monad m => m a -> GLTFT m a
liftT m =
  GLTFT $ \_ cache -> do
    a <- m
    pure (a, cache)
{-# INLINE liftT #-}

-- | Gets an indexed value from the asset in the glTF transmission.
fetchT :: (Applicative m, Ix.Index i Asset a) => i -> GLTFT m a
fetchT = flip fmap askT . T.fetchM
{-# INLINE fetchT #-}

-- | Gets the node with the specified name from the glTF transmission.
getNodeT :: MonadFail m => Text -> GLTFT m Node
getNodeT = (askT >>=) . T.getNodeM
{-# INLINE getNodeT #-}

-- | Loads the data that is associated with the specified accessor.
loadAccessorT :: (MonadFail m, MonadIO m) => A.Accessor -> GLTFT m BS.ByteString
loadAccessorT = GLTFT . T.loadAccessorM
{-# INLINE loadAccessorT #-}

-- | Loads the data that is associated with the specified accessor index.
loadAccessorIxT :: (MonadFail m, MonadIO m) => Ix.AccessorIx -> GLTFT m BS.ByteString
loadAccessorIxT = GLTFT . T.loadAccessorIxM
{-# INLINE loadAccessorIxT #-}

-- | Loads the data that is associated with the specified buffer.
loadBufferT :: (MonadFail m, MonadIO m) => Buffer -> GLTFT m BS.ByteString
loadBufferT = GLTFT . T.loadBufferM
{-# INLINE loadBufferT #-}

-- | Loads the data that is associated with the specified buffer index.
loadBufferIxT :: (MonadFail m, MonadIO m) => Ix.BufferIx -> GLTFT m BS.ByteString
loadBufferIxT = GLTFT . T.loadBufferIxM
{-# INLINE loadBufferIxT #-}

-- | Loads the data that is associated with the specified buffer view.
loadBufferViewT :: (MonadFail m, MonadIO m) => V.BufferView -> GLTFT m BS.ByteString
loadBufferViewT = GLTFT . T.loadBufferViewM
{-# INLINE loadBufferViewT #-}

-- | Loads the data that is associated with the specified buffer view index.
loadBufferViewIxT :: (MonadFail m, MonadIO m) => Ix.BufferViewIx -> GLTFT m BS.ByteString
loadBufferViewIxT = GLTFT . T.loadBufferViewIxM
{-# INLINE loadBufferViewIxT #-}

-- | Loads the data that is associated with the specified image.
loadImageT :: (MonadFail m, MonadIO m) => Image -> GLTFT m BS.ByteString
loadImageT = GLTFT . T.loadImageM
{-# INLINE loadImageT #-}

-- | Loads the data that is associated with the specified URI.
loadURIT :: (MonadFail m, MonadIO m) => URI.URI -> GLTFT m BS.ByteString
loadURIT = GLTFT . T.loadURIM
{-# INLINE loadURIT #-}