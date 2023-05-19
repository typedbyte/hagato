module Hagato.Vulkan.Effectful.RenderPass
  ( allocateRenderPass
  , manageRenderPass
  , recreateRenderPass
  , module Hagato.Vulkan.RenderPass
  ) where

-- effectful-core
import Effectful (Eff, (:>))

-- hagato:with-vulkan
import Hagato.Vulkan            qualified as Vk
import Hagato.Vulkan.RenderPass hiding (recreateRenderPass)

-- resource-effectful
import Effectful.Resource (Key, Resource, allocate, free)

allocateRenderPass
  :: Resource :> es
  => Vk.Swapchain
  -> (Vk.Swapchain -> Vk.RenderPassBuilder ())
  -> Eff es (Vk.RenderPass, Key)
allocateRenderPass swapchain f =
  allocate
    ( Vk.createRenderPass swapchain f )
    ( Vk.destroyRenderPass )

manageRenderPass
  :: Resource :> es
  => Vk.Swapchain
  -> (Vk.Swapchain -> Vk.RenderPassBuilder ())
  -> Eff es Vk.RenderPass
manageRenderPass swapchain f =
  fst <$> allocateRenderPass swapchain f

recreateRenderPass :: Resource :> es => Vk.Swapchain -> Vk.RenderPass -> Key -> Eff es (Vk.RenderPass, Key)
recreateRenderPass swapchain renderPass renderPassKey = do
  new <- allocate (Vk.recreateRenderPass swapchain renderPass) Vk.destroyRenderPass
  free renderPassKey
  pure new
