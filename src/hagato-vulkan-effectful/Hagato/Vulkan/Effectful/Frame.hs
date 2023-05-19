module Hagato.Vulkan.Effectful.Frame
  ( getNextFrame
  , module Hagato.Vulkan.Frame
  ) where

-- effectful-core
import Effectful                    (Eff, IOE, (:>))
import Effectful.State.Static.Local (State, stateM)

-- hagato:with-vulkan
import Hagato.Vulkan       qualified as Vk
import Hagato.Vulkan.Frame hiding (getNextFrame)

-- vector
import Data.Vector qualified as V

getNextFrame
  :: (State Vk.FrameIndex :> es, IOE :> es)
  => Vk.Swapchain
  -> V.Vector Vk.VirtualFrame
  -> Eff es (Maybe Vk.Frame)
getNextFrame swapchain frames =
  stateM $ Vk.getNextFrame swapchain frames
{-# INLINE getNextFrame #-}
