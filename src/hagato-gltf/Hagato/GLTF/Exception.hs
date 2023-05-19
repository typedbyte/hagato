module Hagato.GLTF.Exception where

-- base
import Control.Exception      (Exception, throwIO)
import Control.Monad.IO.Class (MonadIO, liftIO)

data GLTFException
  = InvalidFormat String
  | UnexpectedFormat String
  deriving (Eq, Ord, Show)

instance Exception GLTFException

throw :: MonadIO m => GLTFException -> m a
throw = liftIO . throwIO
{-# INLINE throw #-}