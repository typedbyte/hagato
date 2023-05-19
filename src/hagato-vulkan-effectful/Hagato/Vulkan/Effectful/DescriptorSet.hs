module Hagato.Vulkan.Effectful.DescriptorSet
  ( allocateDescriptorSetLayout
  , manageDescriptorSetLayout
  , allocateDescriptorPool
  , manageDescriptorPool
  , allocateDescriptorSets
  , manageDescriptorSets
  , module Hagato.Vulkan.DescriptorSet
  ) where

-- effectful-core
import Effectful (Eff, (:>))

-- hagato:with-vulkan
import Hagato.Vulkan               qualified as Vk
import Hagato.Vulkan.DescriptorSet hiding (allocateDescriptorSets)

-- resource-effectful
import Effectful.Resource (Key, Resource, allocate)

-- vector
import Data.Vector qualified as V

allocateDescriptorSetLayout
  :: (Vk.Extendss Vk.DescriptorSetLayoutCreateInfo a, Vk.PokeChain a, Resource :> es)
  => Vk.Device -> Vk.DescriptorSetLayoutCreateInfo a -> Eff es (Vk.DescriptorSetLayout, Key)
allocateDescriptorSetLayout device info =
  allocate
    ( Vk.createDescriptorSetLayout device info )
    ( Vk.destroyDescriptorSetLayout device )

manageDescriptorSetLayout
  :: (Vk.Extendss Vk.DescriptorSetLayoutCreateInfo a, Vk.PokeChain a, Resource :> es)
  => Vk.Device -> Vk.DescriptorSetLayoutCreateInfo a -> Eff es Vk.DescriptorSetLayout
manageDescriptorSetLayout device info =
  fst <$> allocateDescriptorSetLayout device info

allocateDescriptorPool
  :: (Vk.Extendss Vk.DescriptorPoolCreateInfo a, Vk.PokeChain a, Resource :> es)
  => Vk.Device -> Vk.DescriptorPoolCreateInfo a -> Eff es (Vk.DescriptorPool, Key)
allocateDescriptorPool device info =
  allocate
    ( Vk.createDescriptorPool device info )
    ( Vk.destroyDescriptorPool device )

manageDescriptorPool
  :: (Vk.Extendss Vk.DescriptorPoolCreateInfo a, Vk.PokeChain a, Resource :> es)
  => Vk.Device -> Vk.DescriptorPoolCreateInfo a -> Eff es Vk.DescriptorPool
manageDescriptorPool device info =
  fst <$> allocateDescriptorPool device info

allocateDescriptorSets
  :: (Vk.Extendss Vk.DescriptorSetAllocateInfo a, Vk.PokeChain a, Resource :> es)
  => Vk.Device -> Vk.DescriptorSetAllocateInfo a -> Eff es (V.Vector Vk.DescriptorSet, Key)
allocateDescriptorSets device info =
  allocate
    ( Vk.allocateDescriptorSets device info )
    ( Vk.freeDescriptorSets device (Vk.descriptorPool info) )

manageDescriptorSets
  :: (Vk.Extendss Vk.DescriptorSetAllocateInfo a, Vk.PokeChain a, Resource :> es)
  => Vk.Device -> Vk.DescriptorSetAllocateInfo a -> Eff es (V.Vector Vk.DescriptorSet)
manageDescriptorSets device info =
  fst <$> allocateDescriptorSets device info
