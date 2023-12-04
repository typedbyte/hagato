-----------------------------------------------------------------------------
-- |
-- Module      : Hagato.Vulkan.DescriptorSet
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Convenience functions and re-exports for handling Vulkan descriptor sets.
-----------------------------------------------------------------------------
module Hagato.Vulkan.DescriptorSet
  ( createDescriptorSetLayout
  , destroyDescriptorSetLayout
  , withDescriptorSetLayout
  , createDescriptorPool
  , destroyDescriptorPool
  , withDescriptorPool
  , resetDescriptorPool
  , withDescriptorSets
  , module Vulkan.Core10.DescriptorSet
  ) where

-- base
import Control.Exception      (bracket)
import Control.Monad.IO.Class (MonadIO)

-- unliftio-core
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)

-- vector
import Data.Vector qualified as V

-- vulkan
import Vulkan                      qualified as Vk
import Vulkan.Core10.DescriptorSet hiding (createDescriptorSetLayout, destroyDescriptorSetLayout,
                                           withDescriptorSetLayout, createDescriptorPool,
                                           destroyDescriptorPool, withDescriptorPool,
                                           resetDescriptorPool, withDescriptorSets)
import Vulkan.CStruct.Extends      qualified as Vk
import Vulkan.Zero                 qualified as Vk

-- | A simpler version of 'Vk.createDescriptorSetLayout'.
createDescriptorSetLayout
  :: (Vk.Extendss Vk.DescriptorSetLayoutCreateInfo a, Vk.PokeChain a, MonadIO m)
  => Vk.Device -> Vk.DescriptorSetLayoutCreateInfo a -> m Vk.DescriptorSetLayout
createDescriptorSetLayout device info =
  Vk.createDescriptorSetLayout device info Nothing

-- | A simpler version of 'Vk.destroyDescriptorSetLayout'.
destroyDescriptorSetLayout :: MonadIO m => Vk.Device -> Vk.DescriptorSetLayout -> m ()
destroyDescriptorSetLayout device layout =
  Vk.destroyDescriptorSetLayout device layout Nothing

-- | A convenient wrapper around 'createDescriptorSetLayout' and 'destroyDescriptorSetLayout'.
withDescriptorSetLayout
  :: (Vk.Extendss Vk.DescriptorSetLayoutCreateInfo a, Vk.PokeChain a, MonadUnliftIO m)
  => Vk.Device -> Vk.DescriptorSetLayoutCreateInfo a -> (Vk.DescriptorSetLayout -> m b) -> m b
withDescriptorSetLayout device info f =
  withRunInIO $ \run ->
    bracket
      ( createDescriptorSetLayout device info )
      ( destroyDescriptorSetLayout device )
      ( run . f )

-- | A simpler version of 'Vk.createDescriptorPool'.
createDescriptorPool
  :: (Vk.Extendss Vk.DescriptorPoolCreateInfo a, Vk.PokeChain a, MonadIO m)
  => Vk.Device -> Vk.DescriptorPoolCreateInfo a -> m Vk.DescriptorPool
createDescriptorPool device info =
  Vk.createDescriptorPool device info Nothing
{-# INLINE createDescriptorPool #-}

-- | A simpler version of 'Vk.destroyDescriptorPool'.
destroyDescriptorPool :: MonadIO m => Vk.Device -> Vk.DescriptorPool -> m ()
destroyDescriptorPool device pool =
  Vk.destroyDescriptorPool device pool Nothing
{-# INLINE destroyDescriptorPool #-}

-- | A convenient wrapper around 'createDescriptorPool' and 'destroyDescriptorPool'.
withDescriptorPool
  :: (Vk.Extendss Vk.DescriptorPoolCreateInfo a, Vk.PokeChain a, MonadUnliftIO m)
  => Vk.Device -> Vk.DescriptorPoolCreateInfo a -> (Vk.DescriptorPool -> m b) -> m b
withDescriptorPool device info f =
  withRunInIO $ \run ->
    bracket
      ( createDescriptorPool device info )
      ( destroyDescriptorPool device )
      ( run . f )

-- | A simpler version of 'Vk.resetDescriptorPool'.
resetDescriptorPool :: MonadIO m => Vk.Device -> Vk.DescriptorPool -> m ()
resetDescriptorPool device pool =
  Vk.resetDescriptorPool device pool Vk.zero

-- | A convenient wrapper around 'allocateDescriptorSets' and 'freeDescriptorSets'.
withDescriptorSets
  :: (Vk.Extendss Vk.DescriptorSetAllocateInfo a, Vk.PokeChain a, MonadUnliftIO m)
  => Vk.Device -> Vk.DescriptorSetAllocateInfo a -> (V.Vector Vk.DescriptorSet -> m b) -> m b
withDescriptorSets device info f =
  withRunInIO $ \run ->
    bracket
      ( allocateDescriptorSets device info )
      ( freeDescriptorSets device (Vk.descriptorPool info) )
      ( run . f )
