module Hagato.Vulkan.Effectful.Pipeline
  ( allocateGraphicsPipelines
  , manageGraphicsPipelines
  , module Hagato.Vulkan.Pipeline
  ) where

-- effectful-core
import Effectful (Eff, (:>))

-- hagato:with-vulkan
import Hagato.Vulkan qualified as Vk
import Hagato.Vulkan.Pipeline

-- resource-effectful
import Effectful.Resource (Key, Resource, allocate)

-- vector
import Data.Vector qualified as V

-- vulkan
import Vulkan.Core10.PipelineCache qualified as Vk

allocateGraphicsPipelines
  :: Resource :> es
  => Vk.Device
  -> Vk.PipelineCache
  -> V.Vector (Vk.SomeStruct Vk.GraphicsPipelineCreateInfo)
  -> Eff es (V.Vector Vk.Pipeline, Key)
allocateGraphicsPipelines device cache infos =
  allocate
    ( Vk.createGraphicsPipelines device cache infos )
    ( flip V.forM_ $ Vk.destroyPipeline device )

manageGraphicsPipelines
  :: Resource :> es
  => Vk.Device
  -> Vk.PipelineCache
  -> V.Vector (Vk.SomeStruct Vk.GraphicsPipelineCreateInfo)
  -> Eff es (V.Vector Vk.Pipeline)
manageGraphicsPipelines device cache infos =
  fst <$> allocateGraphicsPipelines device cache infos
