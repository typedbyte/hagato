module Hagato.Vulkan.Effectful.Swapchain
  ( allocateSwapchain
  , manageSwapchain
  , recreateSwapchain
  , module Hagato.Vulkan.Swapchain
  ) where

-- effectful-core
import Effectful (Eff, (:>))

-- hagato:with-vulkan
import Hagato.Vulkan           qualified as Vk
import Hagato.Vulkan.Swapchain hiding (recreateSwapchain)

-- resource-effectful
import Effectful.Resource (Key, Resource, allocate, free)

allocateSwapchain
  :: Resource :> es
  => Vk.SurfaceKHR
  -> Vk.PhysicalDevice
  -> Vk.Device
  -> [Vk.Queue]
  -> Vk.SwapchainBuilder a
  -> Eff es (Vk.Swapchain, Key)
allocateSwapchain surface physicalDevice device queues builder =
  allocate
    ( Vk.createSwapchain surface physicalDevice device queues builder )
    ( Vk.destroySwapchain )

manageSwapchain
  :: Resource :> es
  => Vk.SurfaceKHR
  -> Vk.PhysicalDevice
  -> Vk.Device
  -> [Vk.Queue]
  -> Vk.SwapchainBuilder a
  -> Eff es Vk.Swapchain
manageSwapchain surface physicalDevice device queues builder =
  fst <$> allocateSwapchain surface physicalDevice device queues builder

recreateSwapchain :: Resource :> es => Vk.Swapchain -> Key -> Eff es (Vk.Swapchain, Key)
recreateSwapchain swapchain swapchainKey = do
  new <- allocate (Vk.recreateSwapchain swapchain) Vk.destroySwapchain
  free swapchainKey
  pure new
