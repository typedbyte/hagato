{-# LANGUAGE DataKinds #-}
module Hagato.GLFW.Effectful.Window
  ( runWindow
  , module Hagato.GLFW.Window
  ) where

-- effectful-core
import Effectful (Eff, IOE, (:>))

-- hagato:with-glfw
import Hagato.GLFW.Instance (initialize, terminate)
import Hagato.GLFW.Window

-- hagato:with-vulkan-effectful
import Hagato.Vulkan.Effectful qualified as Vk

-- resource-effectful
import Effectful.Resource (Resource, manage)

runWindow :: (IOE :> es, Resource :> es) => Eff (Vk.Window GlfwWindow : es) a -> Eff es a
runWindow m = do
  glfw <- manage initialize (const terminate)
  Vk.runWindow (glfwStrategy glfw) m
