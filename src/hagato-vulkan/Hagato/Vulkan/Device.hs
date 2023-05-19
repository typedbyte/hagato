module Hagato.Vulkan.Device
  ( DeviceBuilder
  , QueueID
  , QueueInfo(..)
  , computeQueue
  , graphicsQueue
  , transferQueue
  , presentQueue
  , give
  , preferThat
  , preferPropertyOf
  , preferFamiliesOf
  , preferPairOf
  , preferDistinct
  , preferSame
  , enableDeviceExtensions
  , enableFeatures12
  , enableSwapchainSupport
  , createDevice
  , destroyDevice
  , withDevice
  , module Vulkan.Core10.Device
  ) where

-- base
import Control.Exception      (bracket)
import Control.Monad          (forM, forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Functor           ((<&>))
import Data.List              (sortBy)
import Data.Maybe             (fromMaybe)
import Data.Word              (Word32)

-- bytestring
import Data.ByteString.Char8 qualified as BS

-- containers
import Data.Map.Strict qualified as M

-- hagato:with-core
import Hagato.Core (clamp)

-- unliftio-core
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)

-- vector
import Data.Vector qualified as V

-- vulkan
import Vulkan                 qualified as Vk
import Vulkan.Core10.Device   hiding (createDevice, destroyDevice, withDevice)
import Vulkan.CStruct.Extends qualified as Vk
import Vulkan.Zero            qualified as Vk

import Control.Monad.Logic.Extra qualified as L
import Data.List.Extra           (pairs)
import Hagato.Vulkan.Builder     (Builder, get, modify, put, runBuilder)
import Hagato.Vulkan.Exception   (HagatoVulkanException(..), throwIO)
import Hagato.Vulkan.Queue       (QueueFamily, QueueIndex, Queue(Queue), hasCompute, hasGraphics, hasTransfer)

data DeviceConfig = DeviceConfig
  { queueRequests :: [(QueueFamilyPredicate, [(QueueID, Float)])]
  , preference    :: QueueMapping -> QueueMapping -> Ordering
  , extensions    :: [String]
  , features12    :: Vk.PhysicalDeviceVulkan12Features
  }

instance Vk.Zero DeviceConfig where
  zero =
    DeviceConfig
      { queueRequests = []
      , preference    = \_ _ -> EQ
      , extensions    = []
      , features12    = Vk.zero
      }

newtype DeviceBuilder a =
  DeviceBuilder { builder :: Builder (Int, DeviceConfig) a }
    deriving (Applicative, Functor, Monad, MonadFail, MonadIO)

newtype QueueID = QueueID Int
  deriving (Eq, Ord, Show)

give :: QueueFamilyPredicate -> [Float] -> DeviceBuilder [QueueID]
give f priorities =
  DeviceBuilder $ do
    idPrioPairs <-
      forM priorities $ \priority -> do
        (ix, config) <- get
        put (succ ix, config)
        pure (QueueID ix, priority)
    modify $ \(ix, config) ->
      (ix, config { queueRequests = (f, idPrioPairs) : config.queueRequests })
    pure $ map fst idPrioPairs

data QueueInfo = QueueInfo
  { queueFamily :: QueueFamily
  , queueIndex  :: QueueIndex
  , properties  :: Vk.QueueFamilyProperties
  , queues      :: [QueueID]
  , priority    :: Float
  }

data QueueMapping = QueueMapping
  { infos   :: M.Map QueueFamily (M.Map QueueIndex QueueInfo)
  , assigns :: M.Map QueueID (QueueFamily, QueueIndex)
  }

emptyAssignment :: QueueMapping
emptyAssignment = QueueMapping M.empty M.empty

flatten :: [(a, [(b, c)])] -> [(a, b, c)]
flatten []                = []
flatten ((_, []):r)       = flatten r
flatten ((a, (b,c):ps):r) = (a, b, c) : flatten ((a, ps):r)

assign :: QueueID -> QueueFamily -> Vk.QueueFamilyProperties -> QueueIndex -> Float -> QueueMapping -> QueueMapping
assign qid fam props ix priority state =
  let
    newInfos = M.alter famUpdate fam state.infos
    famUpdate Nothing  = Just (M.singleton ix (QueueInfo fam ix props [qid] priority))
    famUpdate (Just f) = Just (M.alter inUpdate ix f)
    inUpdate Nothing     = Just (QueueInfo fam ix props [qid] priority)
    inUpdate (Just info) = Just $
      info {
        queues   = qid : info.queues
      , priority = clamp 0.0 1.0 ((priority + info.priority) / 2)
      }
  in
    QueueMapping newInfos (M.insert qid (fam, ix) state.assigns)

getQueueFamilies :: Vk.PhysicalDevice -> IO [(QueueFamily, Vk.QueueFamilyProperties)]
getQueueFamilies physicalDevice =
  fmap (zip [0..] . V.toList) $
    Vk.getPhysicalDeviceQueueFamilyProperties physicalDevice

solve :: Vk.PhysicalDevice -> DeviceConfig -> IO QueueMapping
solve physicalDevice config = do
  families <- getQueueFamilies physicalDevice
  mappings <- L.findall $
    go families emptyAssignment (flatten $ config.queueRequests)
  case sortBy config.preference mappings of
    []  -> throwIO NoSuitableDevice
    m:_ -> pure m
  where
    go _ acc [] = pure acc
    go families acc ((predicate, qid, priority):rs) = do
      (fam, props)   <- L.fromList families
      isFamilyWanted <- liftIO $ predicate physicalDevice fam props
      L.assume isFamilyWanted
      queueIx <- L.fromList [0..min (sizeOf fam acc) (Vk.queueCount props - 1)]
      let newAcc = assign qid fam props queueIx priority acc
      go families newAcc rs
    sizeOf fam mapping =
      case M.lookup fam mapping.infos of
        Nothing -> 0
        Just m  -> fromIntegral (M.size m)

addPreference :: (QueueMapping -> QueueMapping -> Ordering) -> DeviceBuilder ()
addPreference f =
  DeviceBuilder $
    modify $ \(ix, config) ->
      (ix, config { preference = config.preference <> f })

preferThat :: QueueID -> (QueueInfo -> Bool) -> DeviceBuilder ()
preferThat qid f =
  addPreference $ \mapL mapR ->
    let
      satisfiesL = satisfies mapL
      satisfiesR = satisfies mapR
      satisfies mapping =
        fromMaybe False $ do
          (fam, ix) <- M.lookup qid mapping.assigns
          infoMap   <- M.lookup fam mapping.infos
          info      <- M.lookup ix infoMap
          pure $ f info
    in
      if satisfiesL && satisfiesR then
        EQ
      else if satisfiesL then
        LT
      else
        GT

preferPropertyOf :: QueueID -> (Vk.QueueFamilyProperties -> Bool) -> DeviceBuilder ()
preferPropertyOf qid f =
  preferThat qid (f . (.properties))

preferPairOf :: QueueID -> QueueID -> (QueueInfo -> QueueInfo -> Bool) -> DeviceBuilder ()
preferPairOf id1 id2 f =
  addPreference $ \mapL mapR ->
    let
      satisfiesL = satisfies mapL
      satisfiesR = satisfies mapR
      satisfies mapping =
        fromMaybe False $ do
          (fam1, ix1) <- M.lookup id1 mapping.assigns
          infoMap1    <- M.lookup fam1 mapping.infos
          info1       <- M.lookup ix1 infoMap1
          (fam2, ix2) <- M.lookup id2 mapping.assigns
          infoMap2    <- M.lookup fam2 mapping.infos
          info2       <- M.lookup ix2 infoMap2
          pure $ f info1 info2
    in
      if satisfiesL && satisfiesR then
        EQ
      else if satisfiesL then
        LT
      else
        GT

preferDistinct :: [QueueID] -> DeviceBuilder ()
preferDistinct ids =
  forM_ (pairs ids) $ \(id1, id2) ->
    preferPairOf id1 id2 $
      \info1 info2 ->
        info1.queueFamily /= info2.queueFamily ||
        info1.queueIndex /= info2.queueIndex

preferSame :: [QueueID] -> DeviceBuilder ()
preferSame ids =
  forM_ (pairs ids) $ \(id1, id2) ->
    preferPairOf id1 id2 $
      \info1 info2 ->
        info1.queueFamily == info2.queueFamily &&
        info1.queueIndex == info2.queueIndex

preferFamiliesOf :: [QueueID] -> (QueueFamily -> QueueFamily -> Bool) -> DeviceBuilder ()
preferFamiliesOf ids f =
  forM_ (pairs ids) $ \(id1, id2) ->
    preferPairOf id1 id2 $
      \info1 info2 ->
        f info1.queueFamily info2.queueFamily

enableDeviceExtensions :: [String] -> DeviceBuilder ()
enableDeviceExtensions exts =
  DeviceBuilder $
    modify $ \(ix, config) ->
      (ix, config { extensions = exts ++ config.extensions })

enableSwapchainSupport :: DeviceBuilder ()
enableSwapchainSupport = enableDeviceExtensions ["VK_KHR_swapchain"]

enableFeatures12 :: Vk.PhysicalDeviceVulkan12Features -> DeviceBuilder ()
enableFeatures12 features =
  DeviceBuilder $
    modify $ \(ix, config) ->
      (ix, config { features12 = features })

type QueueFamilyPredicate = Vk.PhysicalDevice -> Word32 -> Vk.QueueFamilyProperties -> IO Bool

computeQueue :: QueueFamilyPredicate
computeQueue _ _ = pure . hasCompute

graphicsQueue :: QueueFamilyPredicate
graphicsQueue _ _ = pure . hasGraphics

transferQueue :: QueueFamilyPredicate
transferQueue _ _ = pure . hasTransfer

presentQueue :: Vk.SurfaceKHR -> QueueFamilyPredicate
presentQueue surface physicalDevice ix _ =
  Vk.getPhysicalDeviceSurfaceSupportKHR physicalDevice ix surface

createDevice :: MonadIO m => Vk.PhysicalDevice -> DeviceBuilder [QueueID] -> m (Vk.Device, [Queue])
createDevice physicalDevice deviceBuilder =
  liftIO $ do
    ((_, config), ids) <-
      runBuilder (0, Vk.zero) deviceBuilder.builder
    mapping <-
      solve physicalDevice config
    let
      infoList = M.toList mapping.infos
      familyQueues = fmap (\(fam, m) -> (fam, fmap (.priority) (M.elems m))) infoList
      queueCreateInfos =
        familyQueues <&> \(family, qs) ->
          Vk.SomeStruct $ Vk.zero
            { Vk.queueFamilyIndex = family
            , Vk.queuePriorities  = V.fromList qs
            }
      createInfo =
        Vk.zero
          { Vk.next                  = (config.features12, ())
          , Vk.queueCreateInfos      = V.fromList queueCreateInfos
          , Vk.enabledExtensionNames = V.fromList $ fmap BS.pack config.extensions
          }
    device <- Vk.createDevice physicalDevice createInfo Nothing
    queues <-
      forM ids $ \qid ->
        case M.lookup qid mapping.assigns of
          Nothing -> throwIO NoSuitableQueue
          Just (family, i) -> do
            queueHandle <- Vk.getDeviceQueue device family i
            pure $ Queue queueHandle family i
    pure (device, queues)

destroyDevice :: MonadIO m => Vk.Device -> m ()
destroyDevice device =
  liftIO $ do
    Vk.deviceWaitIdle device
    Vk.destroyDevice device Nothing

withDevice
  :: MonadUnliftIO m
  => Vk.PhysicalDevice
  -> DeviceBuilder [QueueID]
  -> ((Vk.Device, [Queue]) -> m a)
  -> m a
withDevice physicalDevice builder f =
  withRunInIO $ \run ->
    bracket
      ( createDevice physicalDevice builder )
      ( destroyDevice . fst )
      ( run . f )
