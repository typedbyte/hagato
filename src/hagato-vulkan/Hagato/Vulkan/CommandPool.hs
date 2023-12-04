-----------------------------------------------------------------------------
-- |
-- Module      : Hagato.Vulkan.CommandPool
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Convenience functions and re-exports for handling Vulkan command pools.
-----------------------------------------------------------------------------
module Hagato.Vulkan.CommandPool
  ( createCommandPool
  , destroyCommandPool
  , withCommandPool
  , module Vulkan.Core10.CommandPool
  ) where

-- base
import Control.Exception      (bracket)
import Control.Monad.IO.Class (MonadIO)

-- unliftio-core
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)

-- vulkan
import Vulkan                    qualified as Vk
import Vulkan.Core10.CommandPool hiding (createCommandPool, destroyCommandPool, withCommandPool)

-- | A simpler version of 'Vk.createCommandPool'.
createCommandPool :: MonadIO m => Vk.Device -> Vk.CommandPoolCreateInfo -> m Vk.CommandPool
createCommandPool device info =
  Vk.createCommandPool device info Nothing
{-# INLINE createCommandPool #-}

-- | A simpler version of 'Vk.destroyCommandPool'.
destroyCommandPool :: MonadIO m => Vk.Device -> Vk.CommandPool -> m ()
destroyCommandPool device pool =
  Vk.destroyCommandPool device pool Nothing
{-# INLINE destroyCommandPool #-}

-- | A convenient wrapper around 'createCommandPool' and 'destroyCommandPool'.
withCommandPool :: MonadUnliftIO m => Vk.Device -> Vk.CommandPoolCreateInfo -> (Vk.CommandPool -> m a) -> m a
withCommandPool device info f =
  withRunInIO $ \run ->
    bracket
      ( createCommandPool device info )
      ( destroyCommandPool device )
      ( run . f )
