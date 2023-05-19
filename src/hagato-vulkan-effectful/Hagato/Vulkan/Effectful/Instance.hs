module Hagato.Vulkan.Effectful.Instance
  ( allocateInstance
  , manageInstance
  , module Hagato.Vulkan.Instance
  ) where

-- effectful-core
import Effectful (Eff, (:>))

-- hagato:with-vulkan
import Hagato.Vulkan qualified as Vk
import Hagato.Vulkan.Instance

-- resource-effectful
import Effectful.Resource (Key, Resource, allocate)

allocateInstance :: Resource :> es => Vk.InstanceBuilder () -> Eff es (Vk.Instance, Key)
allocateInstance builder = do
  allocate
    ( Vk.createInstance builder )
    ( Vk.destroyInstance )

manageInstance :: Resource :> es => Vk.InstanceBuilder () -> Eff es Vk.Instance
manageInstance builder =
  fst <$> allocateInstance builder
