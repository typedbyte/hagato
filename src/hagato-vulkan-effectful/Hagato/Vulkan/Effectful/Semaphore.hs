module Hagato.Vulkan.Effectful.Semaphore
  ( allocateSemaphore
  , manageSemaphore
  , module Hagato.Vulkan.Semaphore
  ) where

-- effectful-core
import Effectful (Eff, (:>))

-- hagato:with-vulkan
import Hagato.Vulkan qualified as Vk
import Hagato.Vulkan.Semaphore

-- resource-effectful
import Effectful.Resource (Key, Resource, allocate)

allocateSemaphore :: Resource :> es => Vk.Device -> Eff es (Vk.Semaphore, Key)
allocateSemaphore device =
  allocate
    ( Vk.createSemaphore device )
    ( Vk.destroySemaphore device )

manageSemaphore :: Resource :> es => Vk.Device -> Eff es Vk.Semaphore
manageSemaphore device =
  fst <$> allocateSemaphore device
