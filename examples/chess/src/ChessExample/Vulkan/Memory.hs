module ChessExample.Vulkan.Memory where

-- effectful-core
import Effectful (Eff, (:>))

-- hagato:with-vulkan
import Hagato.Vulkan (MemoryAllocator)

-- hagato:with-vulkan-effectful
import Hagato.Vulkan.Effectful qualified as Hk

-- resource-effectful
import Effectful.Resource (Resource)

import ChessExample.Vulkan.Setup (RenderSetup(..))

-- Simple wrapper for instantiating the memory allocator whose concrete memory
-- allocating strategy is determined later by the provided effect handler.
manageMemoryAllocator :: (Hk.Memory a k :> es, Resource :> es) => RenderSetup -> Eff es (MemoryAllocator a k)
manageMemoryAllocator setup =
  Hk.manageMemoryAllocator
    setup.vkInstance
    setup.physicalDevice
    setup.device