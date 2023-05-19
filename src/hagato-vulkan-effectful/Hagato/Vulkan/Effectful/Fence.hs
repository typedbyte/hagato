module Hagato.Vulkan.Effectful.Fence
  ( allocateFence
  , manageFence
  , module Hagato.Vulkan.Fence
  ) where

-- effectful-core
import Effectful (Eff, (:>))

-- hagato:with-vulkan
import Hagato.Vulkan qualified as Vk
import Hagato.Vulkan.Fence

-- resource-effectful
import Effectful.Resource (Key, Resource, allocate)

allocateFence :: Resource :> es => Vk.Device -> Eff es (Vk.Fence, Key)
allocateFence device =
  allocate
    ( Vk.createFence device )
    ( Vk.destroyFence device )

manageFence :: Resource :> es => Vk.Device -> Eff es Vk.Fence
manageFence device =
  fst <$> allocateFence device
