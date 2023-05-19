module Hagato.Vulkan.Effectful.PipelineLayout
  ( allocatePipelineLayout
  , managePipelineLayout
  , module Hagato.Vulkan.PipelineLayout
  ) where

-- effectful-core
import Effectful (Eff, (:>))

-- hagato:with-vulkan
import Hagato.Vulkan qualified as Vk
import Hagato.Vulkan.PipelineLayout

-- resource-effectful
import Effectful.Resource (Key, Resource, allocate)

allocatePipelineLayout
  :: Resource :> es
  => Vk.Device
  -> Vk.PipelineLayoutCreateInfo
  -> Eff es (Vk.PipelineLayout, Key)
allocatePipelineLayout device info =
  allocate
    ( Vk.createPipelineLayout device info )
    ( Vk.destroyPipelineLayout device )

managePipelineLayout
  :: Resource :> es
  => Vk.Device
  -> Vk.PipelineLayoutCreateInfo
  -> Eff es Vk.PipelineLayout
managePipelineLayout device info =
  fst <$> allocatePipelineLayout device info
