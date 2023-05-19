module Main where

-- base
import Text.Printf (printf)

-- effectful-core
import Effectful (runEff)

-- hagato:with-core-effectful
import Hagato.Core.Effectful.Log (LogLevel(Debug), mapLogger, mapLoggerIO, runLog,
                                  stdoutLogger)

-- hagato:with-glfw-effectful
import Hagato.GLFW.Effectful.Window (runWindow)

-- hagato:with-vma-effectful
import Hagato.Vulkan.Effectful.MemoryAllocator (AllocationCreateFlagBits(..), runMemory)

-- hagato:with-vulkan
import Hagato.Vulkan qualified as Vk

-- chess
import ChessExample.Game (game)

-- resource-effectful
import Effectful.Resource (runResource)

-- vector
import qualified Data.Vector as V

-- vulkan
import Vulkan.Core10.FundamentalTypes (Extent2D(height, width))

swapchainToString :: Vk.Swapchain -> String
swapchainToString swapchain =
  printf "Swapchain (%d images, %dx%d resolution, %s)"
    ( V.length swapchain.images )
    ( swapchain.extent.width )
    ( swapchain.extent.height )
    ( show swapchain.format )

queueToString :: Vk.Queue -> String
queueToString queue
   = "Queue family: "
  ++ show queue.familyIndex
  ++ " / Queue index: "
  ++ show queue.queueIndex

-- Here we start the game by choosing strategies (=effect handlers) for logging,
-- GPU memory allocation, windowing and debugging.
main :: IO ()
main
  = runEff
  . runResource
  . runLog (mapLogger queueToString logger)
  . runLog (mapLogger swapchainToString logger)
  . runLog (mapLoggerIO Vk.getDeviceName logger)
  . runMemory ALLOCATION_CREATE_STRATEGY_MIN_TIME_BIT
  . runWindow
  $ game ["VK_LAYER_KHRONOS_validation"]
  where
    logger = stdoutLogger Debug
