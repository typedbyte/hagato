module Hagato.Vulkan.Effectful.Framebuffer
  ( allocateFramebuffer
  , manageFramebuffer
  , module Hagato.Vulkan.Framebuffer
  ) where

-- effectful-core
import Effectful (Eff, (:>))

-- hagato:with-vulkan
import Hagato.Vulkan qualified as Vk
import Hagato.Vulkan.Framebuffer

-- resource-effectful
import Effectful.Resource (Key, Resource, allocate)

allocateFramebuffer
  :: (Vk.Extendss Vk.FramebufferCreateInfo a, Vk.PokeChain a, Resource :> es)
  => Vk.Device -> Vk.FramebufferCreateInfo a -> Eff es (Vk.Framebuffer, Key)
allocateFramebuffer device info =
  allocate
    ( Vk.createFramebuffer device info )
    ( Vk.destroyFramebuffer device )

manageFramebuffer
  :: (Vk.Extendss Vk.FramebufferCreateInfo a, Vk.PokeChain a, Resource :> es)
  => Vk.Device -> Vk.FramebufferCreateInfo a -> Eff es Vk.Framebuffer
manageFramebuffer device info =
  fst <$> allocateFramebuffer device info
