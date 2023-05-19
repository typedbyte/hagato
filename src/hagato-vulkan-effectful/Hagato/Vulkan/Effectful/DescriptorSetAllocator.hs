module Hagato.Vulkan.Effectful.DescriptorSetAllocator 
  ( module Hagato.Vulkan.DescriptorSetAllocator
  , module Hagato.Vulkan.Effectful.DescriptorSetAllocator
  ) where

-- base
import Data.Word (Word32)

-- effectful-core
import Effectful (Eff, (:>))

-- hagato:with-vulkan
import Hagato.Vulkan qualified as Vk
import Hagato.Vulkan.DescriptorSetAllocator

-- resource-effectful
import Effectful.Resource (Key, Resource, allocate)

allocateDescriptorSetAllocator
  :: Resource :> es
  => Vk.Device
  -> Word32
  -> Vk.PoolSizeFactors
  -> Eff es (Vk.DescriptorSetAllocator, Key)
allocateDescriptorSetAllocator device maxSetsPerPool sizeFactors =
  allocate
    ( Vk.createDescriptorSetAllocator device maxSetsPerPool sizeFactors )
    ( Vk.destroyDescriptorSetAllocator )

manageDescriptorSetAllocator
  :: Resource :> es
  => Vk.Device
  -> Word32
  -> Vk.PoolSizeFactors
  -> Eff es Vk.DescriptorSetAllocator
manageDescriptorSetAllocator device maxSetsPerPool sizeFactors =
  fst <$> allocateDescriptorSetAllocator device maxSetsPerPool sizeFactors
