-----------------------------------------------------------------------------
-- |
-- Module      : Hagato.GLTF.Exception
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Types and functions for handling exceptions that may occur when parsing or
-- processing glTF files.
-----------------------------------------------------------------------------
module Hagato.GLTF.Exception where

-- base
import Control.Exception      (Exception, throwIO)
import Control.Monad.IO.Class (MonadIO, liftIO)

-- | Represents an exception that may occur when parsing or processing glTF files.
newtype GLTFException = InvalidFormat String
  deriving (Eq, Ord, Show)

instance Exception GLTFException

-- | Throws a glTF-related exception.
throw :: MonadIO m => GLTFException -> m a
throw = liftIO . throwIO
{-# INLINE throw #-}