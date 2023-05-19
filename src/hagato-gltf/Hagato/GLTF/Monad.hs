{-# LANGUAGE LambdaCase #-}
module Hagato.GLTF.Monad where

-- base
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Coerce            (coerce)
import Prelude hiding         (lookup)

-- bytestring
import Data.ByteString      qualified as BS
import Data.ByteString.Lazy qualified as BL

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

-- text
import Data.Text (Text)

newtype GLTFT m a = GLTFT { runGLTFT :: T.Transmission -> T.Cache -> m (a, T.Cache) }

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

withGLTFT :: MonadIO m => T.Source -> GLTFT m a -> m a
withGLTFT source (GLTFT f) =
  case source of
    T.FromBytes bytes ->
      go $ T.parse Nothing (BS.fromStrict bytes)
    T.FromFile file -> do
      bytes <- liftIO (BL.readFile file)
      go $ T.parse (Just file) bytes
  where
    go = \case
      Left err ->
        throw $ InvalidFormat err
      Right tm ->
        fst <$> f tm T.empty

askT :: Applicative m => GLTFT m T.Transmission
askT = GLTFT $ \tm cache -> pure (tm, cache)
{-# INLINE askT #-}

getT :: Applicative m => GLTFT m T.Cache
getT = GLTFT $ \_ cache -> pure (cache, cache)
{-# INLINE getT #-}

putT :: Applicative m => T.Cache -> GLTFT m ()
putT cache = GLTFT $ \_ _ -> pure ((), cache)
{-# INLINE putT #-}

liftT :: Monad m => m a -> GLTFT m a
liftT m =
  GLTFT $ \_ cache -> do
    a <- m
    pure (a, cache)
{-# INLINE liftT #-}

fetchT :: (Applicative m, Ix.Index i Asset a) => i -> GLTFT m a
fetchT = flip fmap askT . T.fetchM
{-# INLINE fetchT #-}

getNodeT :: MonadFail m => Text -> GLTFT m Node
getNodeT = (askT >>=) . T.getNodeM
{-# INLINE getNodeT #-}

loadAccessorT :: (MonadFail m, MonadIO m) => A.Accessor -> GLTFT m BS.ByteString
loadAccessorT = GLTFT . T.loadAccessorM
{-# INLINE loadAccessorT #-}

loadAccessorIxT :: (MonadFail m, MonadIO m) => Ix.AccessorIx -> GLTFT m BS.ByteString
loadAccessorIxT = GLTFT . T.loadAccessorIxM
{-# INLINE loadAccessorIxT #-}


loadBufferT :: (MonadFail m, MonadIO m) => Buffer -> GLTFT m BS.ByteString
loadBufferT = GLTFT . T.loadBufferM
{-# INLINE loadBufferT #-}

loadBufferIxT :: (MonadFail m, MonadIO m) => Ix.BufferIx -> GLTFT m BS.ByteString
loadBufferIxT = GLTFT . T.loadBufferIxM
{-# INLINE loadBufferIxT #-}

loadBufferViewT :: (MonadFail m, MonadIO m) => V.BufferView -> GLTFT m BS.ByteString
loadBufferViewT = GLTFT . T.loadBufferViewM
{-# INLINE loadBufferViewT #-}

loadBufferViewIxT :: (MonadFail m, MonadIO m) => Ix.BufferViewIx -> GLTFT m BS.ByteString
loadBufferViewIxT = GLTFT . T.loadBufferViewIxM
{-# INLINE loadBufferViewIxT #-}

loadImageT :: (MonadFail m, MonadIO m) => Image -> GLTFT m BS.ByteString
loadImageT = GLTFT . T.loadImageM
{-# INLINE loadImageT #-}

loadURIT :: (MonadFail m, MonadIO m) => URI.URI -> GLTFT m BS.ByteString
loadURIT = GLTFT . T.loadURIM
{-# INLINE loadURIT #-}