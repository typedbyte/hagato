module Hagato.GLFW.Instance (GLFW, initialize, terminate, withGLFW) where

-- base
import Control.Exception (bracket)
import Control.Monad     (unless)

-- GLFW-b
import Graphics.UI.GLFW qualified as GLFW

data GLFW = GLFW

initialize :: IO GLFW
initialize =
  GLFW.init
    >>= flip unless (fail "Could not initialize GLFW.")
    >> pure GLFW

terminate :: IO ()
terminate = GLFW.terminate

withGLFW :: (GLFW -> IO a) -> IO a
withGLFW = bracket initialize (const terminate)
