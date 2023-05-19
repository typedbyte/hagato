{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE RecordWildCards    #-}
module ChessExample.Vulkan.Setup where

-- base
import Control.Monad  (forM)
import Data.Bits      ((.|.))
import Prelude hiding (log, null)

-- effectful-core
import Effectful      (Eff, IOE, (:>))
import Effectful.Fail (runFailIO)

-- hagato:with-core-effectful
import Hagato.Core.Effectful (Log, LogLevel(Debug), log)

-- hagato:with-vulkan
import Hagato.Vulkan

-- hagato:with-vulkan-effectful
import Hagato.Vulkan.Effectful qualified as Hk

-- resource-effectful
import Effectful.Resource (Resource, defer)

-- vulkan
import Vulkan                    qualified as Vk
import Vulkan.Core10.CommandPool qualified as Vp
import Vulkan.Core12             (PhysicalDeviceVulkan12Features(..))
import Vulkan.Zero               qualified as Vk

import Effectful.Extra (type (<:))

-- The render setup is the static GPU configuration that is determined once (like
-- picking a GPU, picking command queues, etc.) and remains unchanged during execution.
data RenderSetup = RenderSetup
  { vkInstance     :: Vk.Instance
  , surface        :: Vk.SurfaceKHR
  , physicalDevice :: Vk.PhysicalDevice
  , device         :: Vk.Device
  , tCommandPool   :: Vk.CommandPool
  , gCommandPool   :: Vk.CommandPool
  , tQueue         :: Queue
  , gQueue         :: Queue
  , pQueue         :: Queue
  }

-- Create the render setup.
manageRenderSetup
  :: ( es <: IOE
     , es <: Log Vk.PhysicalDevice
     , es <: Log Queue
     , es <: Resource
     , es <: Hk.Window w
     )
  => w -> [String] -> Eff es RenderSetup
manageRenderSetup window layers =
  runFailIO $ do
    -- initialize Vulkan
    extensions <- Hk.getExtensions window
    vkInstance <-
      Hk.manageInstance $ do
        setAppName "Chess"
        setAppVersion 1 0 0
        setApiVersion 1 2 0
        setEngineName "Hagato"
        setEngineVersion 1 0 0
        enableVulkanExtensions extensions
        enableLayers layers
    -- pick a physical GPU device which:
    -- a) can render on the window surface
    -- b) supports Vulkan 1.2
    -- c) supports multisampling
    -- d) supports dynamic texture arrays in shaders
    -- e) is a discrete GPU, if possible
    surface <- Hk.manageSurface window vkInstance
    let
      indexing =
        Vk.zero
          { shaderSampledImageArrayNonUniformIndexing = True
          , runtimeDescriptorArray                    = True
          , descriptorBindingVariableDescriptorCount  = True
          , descriptorBindingPartiallyBound           = True
          }
    physicalDevice <-
      getPhysicalDevice vkInstance $ do
        prefer discreteDevice
        must `support` minimumApiVersion 1 2 0
        must `support` graphics
        must `support` presentation surface
        must `support` transfer
        must `support` features12 indexing
        must `support` multisampling
    -- pick a logical GPU device based on the physical device which:
    -- a) has swapchain support
    -- b) has aforementioned support for dynamic shader arrays
    -- c) has overall 1x graphics, 1x presentation and 1x transfer queue
    -- d) has graphics and presentation queue of the same queue family, if possible
    -- e) has a special transfer queue, if possible
    log Debug physicalDevice
    (device, [gQueue, pQueue, tQueue]) <-
      Hk.manageDevice physicalDevice $ do
        enableSwapchainSupport
        enableFeatures12 indexing
        [gID] <- give graphicsQueue [1.0]
        [pID] <- give (presentQueue surface) [1.0]
        [tID] <- give transferQueue [1.0]
        preferFamiliesOf [gID, pID] (==)
        preferDistinct [gID, pID, tID]
        preferPropertyOf tID (not . hasGraphics)
        preferPropertyOf tID (not . hasCompute)
        pure [gID, pID, tID]
    log Debug gQueue
    log Debug pQueue
    log Debug tQueue
    -- create command pools needed for creating command buffers in which we can
    -- place and submit graphics, presentation and transfer operations.
    [gCommandPool, tCommandPool] <-
      forM @[] [gQueue, tQueue] $ \queue ->
        Hk.manageCommandPool device $
          Vk.zero
            { Vp.queueFamilyIndex =
                queue.familyIndex
            , Vp.flags =
                Vk.COMMAND_POOL_CREATE_TRANSIENT_BIT .|.
                Vk.COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT
            }
    pure $ RenderSetup{..}

-- Helper function which waits until the picked GPU device is idle after executing
-- the provided action. This prevents the deallocation of GPU resources which are
-- still in use by the GPU.
withRenderSetup :: Resource :> es => RenderSetup -> Eff es a -> Eff es a
withRenderSetup setup action = do
  result <- action
  defer $ Vk.deviceWaitIdle setup.device
  pure result