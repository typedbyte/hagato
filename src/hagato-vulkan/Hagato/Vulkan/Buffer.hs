-----------------------------------------------------------------------------
-- |
-- Module      : Hagato.Vulkan.Buffer
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Convenience functions and re-exports for handling Vulkan buffers.
-----------------------------------------------------------------------------
module Hagato.Vulkan.Buffer
  ( createBuffer
  , destroyBuffer
  , withBuffer
  , module Vulkan.Core10.Buffer
  ) where

-- base
import Control.Exception      (bracket)
import Control.Monad.IO.Class (MonadIO)

-- unliftio-core
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)

-- vulkan
import Vulkan                 qualified as Vk
import Vulkan.Core10.Buffer   hiding (createBuffer, destroyBuffer, withBuffer)
import Vulkan.CStruct.Extends qualified as Vk

-- | A simpler version of 'Vk.createBuffer'.
createBuffer
  :: (Vk.Extendss Vk.BufferCreateInfo a, Vk.PokeChain a, MonadIO m)
  => Vk.Device -> Vk.BufferCreateInfo a -> m Vk.Buffer
createBuffer device info =
  Vk.createBuffer device info Nothing

-- | A simpler version of 'Vk.destroyBuffer'.
destroyBuffer :: MonadIO m => Vk.Device -> Vk.Buffer -> m ()
destroyBuffer device buffer =
  Vk.destroyBuffer device buffer Nothing

-- | A convenient wrapper around 'createBuffer' and 'destroyBuffer'.
withBuffer
  :: (Vk.Extendss Vk.BufferCreateInfo a, Vk.PokeChain a, MonadUnliftIO m)
  => Vk.Device -> Vk.BufferCreateInfo a -> (Vk.Buffer -> m b) -> m b
withBuffer device info f =
  withRunInIO $ \run ->
    bracket
      ( createBuffer device info )
      ( destroyBuffer device )
      ( run . f )
