module Hagato.Vulkan.Effectful.Sampler
  ( allocateSampler
  , manageSampler
  , module Hagato.Vulkan.Sampler
  ) where

-- effectful-core
import Effectful (Eff, (:>))

-- hagato:with-vulkan
import Hagato.Vulkan qualified as Vk
import Hagato.Vulkan.Sampler

-- resource-effectful
import Effectful.Resource (Key, Resource, allocate)

allocateSampler
  :: (Vk.Extendss Vk.SamplerCreateInfo a, Vk.PokeChain a, Resource :> es)
  => Vk.Device -> Vk.SamplerCreateInfo a -> Eff es (Vk.Sampler, Key)
allocateSampler device info =
  allocate
    ( Vk.createSampler device info )
    ( Vk.destroySampler device  )

manageSampler
  :: (Vk.Extendss Vk.SamplerCreateInfo a, Vk.PokeChain a, Resource :> es)
  => Vk.Device -> Vk.SamplerCreateInfo a -> Eff es Vk.Sampler
manageSampler device info =
  fst <$> allocateSampler device info
