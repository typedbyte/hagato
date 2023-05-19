{-# LANGUAGE DataKinds #-}
module Hagato.Vulkan.PhysicalDevice
  ( PhysicalDeviceBuilder
  , must
  , support
  , compute
  , graphics
  , transfer
  , presentation
  , minimumApiVersion
  , multisampling
  , features12
  , prefer
  , preferType
  , discreteDevice
  , integratedDevice
  , getPhysicalDevice
  , getDeviceName
  , getMaxSampleCount
  , module Vulkan.Core10.DeviceInitialization
  ) where

-- base
import Control.Monad          (filterM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits              ((.&.))
import Data.Traversable       (for)
import Data.Word              (Word32)

-- bytestring
import Data.ByteString.Char8 qualified as BS

-- vector
import Data.Vector qualified as V

-- vulkan
import Vulkan                             qualified as Vk
import Vulkan.Core10.DeviceInitialization hiding (createInstance, destroyInstance, withInstance)
import Vulkan.Zero                        qualified as Vk

import Data.Bits.Extra         (testAllBits)
import Data.Ord.Extra          (sortByM)
import Hagato.Vulkan.Builder   (Builder, execBuilder, modify)
import Hagato.Vulkan.Exception (HagatoVulkanException(..), throwIO)
import Hagato.Vulkan.Queue     (QueueFamily, hasGraphics, hasCompute, hasTransfer)
import Hagato.Vulkan.Result    (expect)
import Hagato.Vulkan.Version   (version)

data PhysicalDeviceConfig = PhysicalDeviceConfig
  { deviceFilter     :: Vk.PhysicalDevice -> IO Bool
  , devicePreference :: Vk.PhysicalDevice -> Vk.PhysicalDevice -> IO Ordering
  }

instance Vk.Zero PhysicalDeviceConfig where
  zero =
    PhysicalDeviceConfig
      { deviceFilter     = const (pure True)
      , devicePreference = \_ _ -> pure EQ
      }

type PhysicalDeviceBuilder a = Builder PhysicalDeviceConfig a

must :: (Vk.PhysicalDevice -> IO Bool) -> PhysicalDeviceBuilder ()
must f =
  modify $ \config ->
    config
      { deviceFilter =
          \info -> do
            success <- config.deviceFilter info
            case success of
              True -> f info
              False -> pure False
      }

support :: (a -> b) -> a -> b
support = ($)

compute :: Vk.PhysicalDevice -> IO Bool
compute physicalDevice =
  not . null <$> getComputeQueues physicalDevice

graphics :: Vk.PhysicalDevice -> IO Bool
graphics physicalDevice =
  not . null <$> getGraphicsQueues physicalDevice

transfer :: Vk.PhysicalDevice -> IO Bool
transfer physicalDevice =
  not . null <$> getTransferQueues physicalDevice

presentation :: Vk.SurfaceKHR -> Vk.PhysicalDevice -> IO Bool
presentation surface physicalDevice =
  not . null <$> getPresentationQueues surface physicalDevice

minimumApiVersion :: Word32 -> Word32 -> Word32 -> Vk.PhysicalDevice -> IO Bool
minimumApiVersion major minor patch physicalDevice = do
  props <- Vk.getPhysicalDeviceProperties physicalDevice
  pure $ propVersion props >= version major minor patch
  where
    -- needed for disambiguation of "apiVersion"
    propVersion :: Vk.PhysicalDeviceProperties -> Word32
    propVersion Vk.PhysicalDeviceProperties{apiVersion = v} = v

multisampling :: Vk.PhysicalDevice -> IO Bool
multisampling physicalDevice = do
  maxSampleCount <- getMaxSampleCount physicalDevice
  pure $ maxSampleCount /= Vk.SAMPLE_COUNT_1_BIT

implies :: a -> a -> [a -> Bool] -> Bool
implies want have fields =
  and compatibilities
    where
      wantFields      = fmap ($ want) fields
      haveFields      = fmap ($ have) fields
      imply a b       = not a || b
      compatibilities = zipWith imply wantFields haveFields

features12 :: Vk.PhysicalDeviceVulkan12Features -> Vk.PhysicalDevice -> IO Bool
features12 wantedFeatures physicalDevice = do
  haves <-
    Vk.getPhysicalDeviceFeatures2
      @'[Vk.PhysicalDeviceVulkan12Features]
      physicalDevice
  let haveFeatures = fst haves.next
  pure $
    wantedFeatures `implies` haveFeatures $
      [ (.samplerMirrorClampToEdge)
      , (.drawIndirectCount)
      , (.storageBuffer8BitAccess)
      , (.uniformAndStorageBuffer8BitAccess)
      , (.storagePushConstant8)
      , (.shaderBufferInt64Atomics)
      , (.shaderSharedInt64Atomics)
      , (.shaderFloat16)
      , (.shaderInt8)
      , (.descriptorIndexing)
      , (.shaderInputAttachmentArrayDynamicIndexing)
      , (.shaderUniformTexelBufferArrayDynamicIndexing)
      , (.shaderStorageTexelBufferArrayDynamicIndexing)
      , (.shaderUniformBufferArrayNonUniformIndexing)
      , (.shaderSampledImageArrayNonUniformIndexing)
      , (.shaderStorageBufferArrayNonUniformIndexing)
      , (.shaderStorageImageArrayNonUniformIndexing)
      , (.shaderInputAttachmentArrayNonUniformIndexing)
      , (.shaderUniformTexelBufferArrayNonUniformIndexing)
      , (.shaderStorageTexelBufferArrayNonUniformIndexing)
      , (.descriptorBindingUniformBufferUpdateAfterBind)
      , (.descriptorBindingSampledImageUpdateAfterBind)
      , (.descriptorBindingStorageImageUpdateAfterBind)
      , (.descriptorBindingStorageBufferUpdateAfterBind)
      , (.descriptorBindingUniformTexelBufferUpdateAfterBind)
      , (.descriptorBindingStorageTexelBufferUpdateAfterBind)
      , (.descriptorBindingUpdateUnusedWhilePending)
      , (.descriptorBindingPartiallyBound)
      , (.descriptorBindingVariableDescriptorCount)
      , (.runtimeDescriptorArray)
      , (.samplerFilterMinmax)
      , (.scalarBlockLayout)
      , (.imagelessFramebuffer)
      , (.uniformBufferStandardLayout)
      , (.shaderSubgroupExtendedTypes)
      , (.separateDepthStencilLayouts)
      , (.hostQueryReset)
      , (.timelineSemaphore)
      , (.bufferDeviceAddress)
      , (.bufferDeviceAddressCaptureReplay)
      , (.bufferDeviceAddressMultiDevice)
      , (.vulkanMemoryModel)
      , (.vulkanMemoryModelDeviceScope)
      , (.vulkanMemoryModelAvailabilityVisibilityChains)
      , (.shaderOutputViewportIndex)
      , (.shaderOutputLayer)
      , (.subgroupBroadcastDynamicId)
      ]

prefer :: (Vk.PhysicalDevice -> Vk.PhysicalDevice -> IO Ordering) -> PhysicalDeviceBuilder ()
prefer comp =
  modify $ \config ->
    config
      { devicePreference = config.devicePreference <> comp }

preferType :: Vk.PhysicalDeviceType -> Vk.PhysicalDevice -> Vk.PhysicalDevice -> IO Ordering
preferType preferredType devL devR = do
  typeL <- Vk.deviceType <$> Vk.getPhysicalDeviceProperties devL
  typeR <- Vk.deviceType <$> Vk.getPhysicalDeviceProperties devR
  pure $
    if typeL == typeR then
      EQ
    else if typeL == preferredType then
      LT
    else
      GT

discreteDevice :: Vk.PhysicalDevice -> Vk.PhysicalDevice -> IO Ordering
discreteDevice = preferType Vk.PHYSICAL_DEVICE_TYPE_DISCRETE_GPU

integratedDevice :: Vk.PhysicalDevice -> Vk.PhysicalDevice -> IO Ordering
integratedDevice = preferType Vk.PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU

getPhysicalDevice :: MonadIO m => Vk.Instance -> PhysicalDeviceBuilder () -> m Vk.PhysicalDevice
getPhysicalDevice vkInstance builder =
  liftIO $ do
    config <- execBuilder Vk.zero builder
    allDevices <-
      fmap V.toList $
        expect [Vk.SUCCESS, Vk.INCOMPLETE] $
          Vk.enumeratePhysicalDevices vkInstance
    filteredDevices <- filterM config.deviceFilter allDevices
    sortedDevices <- sortByM config.devicePreference filteredDevices
    case sortedDevices of
      [] -> throwIO NoSuitablePhysicalDevice
      (dev:_) -> pure dev

getDeviceName :: MonadIO m => Vk.PhysicalDevice -> m String
getDeviceName physicalDevice = do
  props <- Vk.getPhysicalDeviceProperties physicalDevice
  pure $ BS.unpack (Vk.deviceName props)

getMaxSampleCount :: MonadIO m => Vk.PhysicalDevice -> m Vk.SampleCountFlagBits
getMaxSampleCount physicalDevice = do
  props <- Vk.getPhysicalDeviceProperties physicalDevice
  let
    sampleCountFlags
      =   props.limits.framebufferColorSampleCounts
      .&. props.limits.framebufferDepthSampleCounts
  pure $
    if testAllBits sampleCountFlags Vk.SAMPLE_COUNT_64_BIT then
      Vk.SAMPLE_COUNT_64_BIT
    else if testAllBits sampleCountFlags Vk.SAMPLE_COUNT_32_BIT then
      Vk.SAMPLE_COUNT_32_BIT
    else if testAllBits sampleCountFlags Vk.SAMPLE_COUNT_16_BIT then
      Vk.SAMPLE_COUNT_16_BIT
    else if testAllBits sampleCountFlags Vk.SAMPLE_COUNT_8_BIT then
      Vk.SAMPLE_COUNT_8_BIT
    else if testAllBits sampleCountFlags Vk.SAMPLE_COUNT_4_BIT then
      Vk.SAMPLE_COUNT_4_BIT
    else if testAllBits sampleCountFlags Vk.SAMPLE_COUNT_2_BIT then
      Vk.SAMPLE_COUNT_2_BIT
    else
      Vk.SAMPLE_COUNT_1_BIT

filterQueues
  :: (Vk.QueueFamilyProperties -> Bool)
  -> [Vk.QueueFamilyProperties]
  -> [QueueFamily]
filterQueues f props =
  [ i | (i, family) <- families, f family ]
    where
      families = zip [0..] props

getComputeQueues :: Vk.PhysicalDevice -> IO [QueueFamily]
getComputeQueues physicalDevice = do
  props <- V.toList <$> Vk.getPhysicalDeviceQueueFamilyProperties physicalDevice
  pure $ filterQueues hasCompute props

getGraphicsQueues :: Vk.PhysicalDevice -> IO [QueueFamily]
getGraphicsQueues physicalDevice = do
  props <- V.toList <$> Vk.getPhysicalDeviceQueueFamilyProperties physicalDevice
  pure $ filterQueues hasGraphics props

getTransferQueues :: Vk.PhysicalDevice -> IO [QueueFamily]
getTransferQueues physicalDevice = do
  props <- V.toList <$> Vk.getPhysicalDeviceQueueFamilyProperties physicalDevice
  pure $ filterQueues hasTransfer props

getPresentationQueues :: Vk.SurfaceKHR -> Vk.PhysicalDevice -> IO [QueueFamily]
getPresentationQueues surface physicalDevice = do
  props <- V.toList <$> Vk.getPhysicalDeviceQueueFamilyProperties physicalDevice
  presentFlags <-
    for (zip [0..] props) $
      \(i,_) ->
        Vk.getPhysicalDeviceSurfaceSupportKHR
          physicalDevice
          i
          surface
  pure $ [ i | (i, True) <- zip [0..] presentFlags ]
