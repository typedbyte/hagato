{-# LANGUAGE LambdaCase #-}
module Hagato.Vulkan.Swapchain
  ( SwapchainBuilder
  , Swapchain(surface, physicalDevice, device, handle, images, format, extent, queueFamilies)
  , preferFormat
  , preferMode
  , preferImageCount
  , setImageUsage
  , setInfo
  , createSwapchain
  , destroySwapchain
  , withSwapchain
  , recreateSwapchain
  , getNextImage
  , indexOf
  , null
  , toRect2D
  , toSize
  , toVec2
  , toViewport
  , module Vulkan.Extensions.VK_KHR_swapchain
  ) where

-- base
import Control.Exception      (bracket)
import Control.Monad          (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List              (nub, sortOn)
import Data.Word              (Word32, Word64)
import Prelude         hiding (null)

-- hagato:with-core
import Hagato.Core (Vec2(Vec2), clamp)

-- unliftio-core
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)

-- vector
import Data.Vector qualified as V

-- vulkan
import Vulkan      qualified as Vk
import Vulkan.Extensions.VK_KHR_swapchain
import Vulkan.Zero qualified as Vk

import Data.Bits.Extra         (testAllBits)
import Hagato.Vulkan.Builder   (Builder, execBuilder, modify)
import Hagato.Vulkan.Exception (HagatoVulkanException(..), throwIO, throwVk)
import Hagato.Vulkan.Queue     (Queue(familyIndex), QueueFamily)
import Hagato.Vulkan.Result    (expect)

data SwapchainConfig = SwapchainConfig
  { formatPreference    :: Vk.SurfaceFormatKHR -> Ordering
  , modePreference      :: Vk.PresentModeKHR -> Ordering
  , imagePreference     :: Vk.SurfaceCapabilitiesKHR -> Vk.PresentModeKHR -> Word32
  , extentPreference    :: Vk.Extent2D -> Vk.Extent2D -> Vk.Extent2D -> Vk.Extent2D
  , transformPreference :: Vk.SurfaceTransformFlagsKHR
                        -> Vk.SurfaceTransformFlagBitsKHR
                        -> Vk.SurfaceTransformFlagBitsKHR
  , sharingPreference   :: [QueueFamily] -> Vk.SharingMode
  , infoPreference      :: forall es. Vk.SwapchainCreateInfoKHR es -> Vk.SwapchainCreateInfoKHR es
  , imageUsageDecider   :: Vk.ImageUsageFlags -> Vk.ImageUsageFlags
  }

instance Vk.Zero SwapchainConfig where
  zero =
    SwapchainConfig
      { formatPreference    = const EQ
      , modePreference      = const EQ
      , imagePreference     = defaultImagePreference
      , extentPreference    = defaultExtentPreference
      , transformPreference = defaultTransformPreference
      , sharingPreference   = defaultSharingPreference
      , infoPreference      = id
      , imageUsageDecider   = const Vk.IMAGE_USAGE_COLOR_ATTACHMENT_BIT
      }

defaultFormats :: (Vk.SurfaceFormatKHR, Vk.SurfaceFormatKHR)
defaultFormats =
  ( Vk.SurfaceFormatKHR
      Vk.FORMAT_B8G8R8A8_SRGB
      Vk.COLOR_SPACE_SRGB_NONLINEAR_KHR
  , Vk.SurfaceFormatKHR
      Vk.FORMAT_R8G8B8A8_SRGB
      Vk.COLOR_SPACE_SRGB_NONLINEAR_KHR
  )

defaultImagePreference :: Vk.SurfaceCapabilitiesKHR -> Vk.PresentModeKHR -> Word32
defaultImagePreference capabilities = \case
  Vk.PRESENT_MODE_MAILBOX_KHR -> capabilities.minImageCount + 1
  _                           -> capabilities.minImageCount

defaultExtentPreference :: Vk.Extent2D -> Vk.Extent2D -> Vk.Extent2D -> Vk.Extent2D
defaultExtentPreference _minExtent maxExtent current
  | current.width /= maxBound = current
  | otherwise                 = maxExtent

defaultTransformPreference
  :: Vk.SurfaceTransformFlagsKHR
  -> Vk.SurfaceTransformFlagBitsKHR
  -> Vk.SurfaceTransformFlagBitsKHR
defaultTransformPreference _supportedTransforms current = current

defaultSharingPreference :: [QueueFamily] -> Vk.SharingMode
defaultSharingPreference = \case
  _:_:_ -> Vk.SHARING_MODE_CONCURRENT
  _     -> Vk.SHARING_MODE_EXCLUSIVE

type SwapchainBuilder a = Builder SwapchainConfig a

preferFormat :: Vk.SurfaceFormatKHR -> SwapchainBuilder ()
preferFormat surfaceFormat =
  modify $ \cfg ->
    cfg
      { formatPreference = cfg.formatPreference <> prefer surfaceFormat }

preferMode :: Vk.PresentModeKHR -> SwapchainBuilder ()
preferMode mode =
  modify $ \cfg ->
    cfg
      { modePreference = cfg.modePreference <> prefer mode }

preferImageCount :: Word32 -> SwapchainBuilder ()
preferImageCount count =
  modify $ \cfg ->
    cfg
      { imagePreference = \_ _ -> count }

setInfo :: (forall es. Vk.SwapchainCreateInfoKHR es -> Vk.SwapchainCreateInfoKHR es) -> SwapchainBuilder ()
setInfo f =
  modify $ \cfg@(SwapchainConfig {infoPreference=currentPreference}) ->
    cfg
      { infoPreference = f . currentPreference }

prefer :: Eq a => a -> a -> Ordering
prefer want have | want == have = LT
                 | otherwise    = EQ

setImageUsage :: (Vk.ImageUsageFlags -> Vk.ImageUsageFlags) -> SwapchainBuilder ()
setImageUsage f =
  modify $ \cfg ->
    cfg
      { imageUsageDecider = f }

data Swapchain = Swapchain
  { surface        :: Vk.SurfaceKHR
  , physicalDevice :: Vk.PhysicalDevice
  , device         :: Vk.Device
  , handle         :: Vk.SwapchainKHR
  , images         :: V.Vector Vk.Image
  , format         :: Vk.Format
  , extent         :: Vk.Extent2D
  , config         :: SwapchainConfig
  , queueFamilies  :: [QueueFamily]
  }

null :: Swapchain -> Bool
null = (== Vk.NULL_HANDLE) . (.handle)

toRect2D :: Swapchain -> Vk.Rect2D
toRect2D swapchain =
  Vk.Rect2D Vk.zero swapchain.extent

toSize :: Num a => Swapchain -> (a, a)
toSize swapchain =
  (fromIntegral swapchain.extent.width, fromIntegral swapchain.extent.height)

toVec2 :: Swapchain -> Vec2
toVec2 swapchain =
  Vec2
    ( fromIntegral swapchain.extent.width  )
    ( fromIntegral swapchain.extent.height )

toViewport :: Float -> Float -> Swapchain -> Vk.Viewport
toViewport minDepth maxDepth swapchain =
  Vk.zero
    { Vk.x        = 0
    , Vk.y        = 0
    , Vk.width    = fromIntegral swapchain.extent.width
    , Vk.height   = fromIntegral swapchain.extent.height
    , Vk.minDepth = minDepth
    , Vk.maxDepth = maxDepth
    }

renewSwapchain :: Swapchain -> IO Swapchain
renewSwapchain swapchain = do
  let
    surface   = swapchain.surface
    phyDevice = swapchain.physicalDevice
    device    = swapchain.device
    families  = swapchain.queueFamilies
    cfg@SwapchainConfig{infoPreference=applyPreference} = swapchain.config
  
  surfaceFormats <-
    fmap V.toList $
      expect [Vk.SUCCESS, Vk.INCOMPLETE] $
        Vk.getPhysicalDeviceSurfaceFormatsKHR
          phyDevice
          surface
  
  surfaceFormat <-
    case sortOn cfg.formatPreference surfaceFormats of
      [] ->
        throwIO NoSurfaceFormat
      [f] | f.format == Vk.FORMAT_UNDEFINED ->
        pure (fst defaultFormats)
      (f:_) ->
        pure f
  
  capabilities <-
    Vk.getPhysicalDeviceSurfaceCapabilitiesKHR
      phyDevice
      surface
  
  presentModes <-
    fmap V.toList $
      expect [Vk.SUCCESS, Vk.INCOMPLETE] $
        Vk.getPhysicalDeviceSurfacePresentModesKHR
          phyDevice
          surface
  
  let
    presentMode =
      case sortOn cfg.modePreference presentModes of
        []  -> Vk.PRESENT_MODE_FIFO_KHR
        m:_ -> m

  let
    imageFormat         = surfaceFormat.format
    minImageCount       = capabilities.minImageCount
    maxImageCount       = capabilities.maxImageCount
    realMaxImageCount   = if maxImageCount == 0
                          then maxBound
                          else maxImageCount
    minimumExtent       = capabilities.minImageExtent
    maximumExtent       = capabilities.maxImageExtent
    currentExtent       = capabilities.currentExtent
    currentTransform    = capabilities.currentTransform
    supportedTransforms = capabilities.supportedTransforms
    supportedUsage      = cfg.imageUsageDecider $ capabilities.supportedUsageFlags
    wantedImageCount    = cfg.imagePreference capabilities presentMode
    imageCount          = clamp minImageCount realMaxImageCount wantedImageCount
    realExtent          = cfg.extentPreference minimumExtent maximumExtent currentExtent
    sharingMode         = cfg.sharingPreference families
    wantedTransform     = cfg.transformPreference supportedTransforms currentTransform
    transform           = if testAllBits supportedTransforms wantedTransform
                          then wantedTransform
                          else currentTransform
  
  if realExtent.width <= 0 || realExtent.height <= 0 then
    pure $
      swapchain
        { handle = Vk.NULL_HANDLE
        , images = V.empty
        , format = Vk.FORMAT_UNDEFINED
        , extent = Vk.Extent2D 0 0
        }
  else do
    let
      swapCreateInfo =
        Vk.zero
          { Vk.surface            = surface
          , Vk.minImageCount      = imageCount
          , Vk.imageFormat        = imageFormat
          , Vk.imageColorSpace    = surfaceFormat.colorSpace
          , Vk.imageExtent        = realExtent
          , Vk.imageArrayLayers   = 1
          , Vk.imageUsage         = supportedUsage
          , Vk.imageSharingMode   = sharingMode
          , Vk.queueFamilyIndices = if sharingMode == Vk.SHARING_MODE_EXCLUSIVE
                                    then V.empty
                                    else V.fromList families
          , Vk.preTransform       = transform
          , Vk.compositeAlpha     = Vk.COMPOSITE_ALPHA_OPAQUE_BIT_KHR
          , Vk.presentMode        = presentMode
          , Vk.clipped            = True
          , Vk.oldSwapchain       = swapchain.handle
          }
    newHandle <-
      Vk.createSwapchainKHR device (applyPreference swapCreateInfo) Nothing
    images <-
      expect [Vk.SUCCESS, Vk.INCOMPLETE] $
        Vk.getSwapchainImagesKHR device newHandle
    pure $
      swapchain
        { handle = newHandle
        , images = images
        , format = imageFormat
        , extent = realExtent
        }

createSwapchain
  :: MonadIO m
  => Vk.SurfaceKHR
  -> Vk.PhysicalDevice
  -> Vk.Device
  -> [Queue]
  -> SwapchainBuilder a
  -> m Swapchain
createSwapchain surface physicalDevice device queues builder =
  liftIO $ do
    config <- execBuilder Vk.zero $
      builder
        >> preferFormat (fst defaultFormats)
        >> preferFormat (snd defaultFormats)
        >> preferMode Vk.PRESENT_MODE_MAILBOX_KHR
    recreateSwapchain $
      Swapchain
        { surface        = surface
        , physicalDevice = physicalDevice
        , device         = device
        , handle         = Vk.NULL_HANDLE
        , images         = V.empty
        , format         = Vk.FORMAT_UNDEFINED
        , extent         = Vk.Extent2D 0 0
        , config         = config
        , queueFamilies  = nub $ fmap (.familyIndex) queues
        }

destroySwapchain :: MonadIO m => Swapchain -> m ()
destroySwapchain swapchain =
  liftIO $
    when (not $ null swapchain) $
      Vk.destroySwapchainKHR swapchain.device swapchain.handle Nothing
{-# INLINE destroySwapchain #-}

withSwapchain
  :: MonadUnliftIO m
  => Vk.SurfaceKHR
  -> Vk.PhysicalDevice
  -> Vk.Device
  -> [Queue]
  -> SwapchainBuilder ()
  -> (Swapchain -> m a) -> m a
withSwapchain surf phyDevice dev queues builder f =
  withRunInIO $ \run ->
    bracket
      ( createSwapchain surf phyDevice dev queues builder )
      ( destroySwapchain )
      ( run . f )

recreateSwapchain :: MonadIO m => Swapchain -> m Swapchain
recreateSwapchain = liftIO . renewSwapchain
{-# INLINE recreateSwapchain #-}

data ImageResult
  = Index Word32
  | Timeout
  | NotReady
  | Suboptimal

getNextImage :: MonadIO m => Word64 -> Vk.Semaphore -> Vk.Fence -> Swapchain -> m ImageResult
getNextImage timeout semaphore fence swapchain = do
  (result, ix) <-
    Vk.acquireNextImageKHR
      swapchain.device
      swapchain.handle
      timeout
      semaphore
      fence
  case result of
    Vk.SUCCESS        -> pure (Index ix)
    Vk.TIMEOUT        -> pure Timeout
    Vk.NOT_READY      -> pure NotReady
    Vk.SUBOPTIMAL_KHR -> pure Suboptimal
    errorResult       -> throwVk errorResult

indexOf :: ImageResult -> Maybe Word32
indexOf = \case
  Index ix -> Just ix
  _        -> Nothing
{-# INLINE indexOf #-}
