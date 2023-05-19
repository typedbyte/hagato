{-# LANGUAGE DataKinds #-}
module Hagato.Vulkan.Instance
  ( InstanceBuilder
  , setAppName
  , setAppVersion
  , setEngineName
  , setEngineVersion
  , setApiVersion
  , enableLayer
  , enableLayers
  , enableVulkanExtensions
  , createInstance
  , destroyInstance
  , withInstance
  ) where

-- base
import Control.Exception      (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe             (fromMaybe)
import Data.Word              (Word32)

-- bytestring
import Data.ByteString.Char8 qualified as BS

-- unliftio-core
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)

-- vector
import Data.Vector qualified as V

-- vulkan
import Vulkan.Core10.DeviceInitialization qualified as Vk
import Vulkan.Zero                        qualified as Vk

import Hagato.Vulkan.Builder (Builder, execBuilder, modify)
import Hagato.Vulkan.Version (version)

newtype InstanceBuilder a =
  InstanceBuilder { builder :: Builder (Vk.InstanceCreateInfo '[]) a }
    deriving (Applicative, Functor, Monad, MonadFail, MonadIO)

updateAppInfo :: (Vk.ApplicationInfo -> Vk.ApplicationInfo) -> InstanceBuilder ()
updateAppInfo f =
  InstanceBuilder $
    modify $ \createInfo ->
      let
        appInfo = fromMaybe Vk.zero (Vk.applicationInfo createInfo)
      in
        createInfo { Vk.applicationInfo = Just $ f appInfo }

setAppName :: String -> InstanceBuilder ()
setAppName name =
  updateAppInfo $
    \info -> info { Vk.applicationName = Just $ BS.pack name }

setAppVersion :: Word32 -> Word32 -> Word32 -> InstanceBuilder ()
setAppVersion major minor patch =
  updateAppInfo $
    \info -> info { Vk.applicationVersion = version major minor patch }

setEngineName :: String -> InstanceBuilder ()
setEngineName name =
  updateAppInfo $
    \info -> info { Vk.engineName = Just $ BS.pack name }

setEngineVersion :: Word32 -> Word32 -> Word32 -> InstanceBuilder ()
setEngineVersion major minor patch =
  updateAppInfo $
    \info -> info { Vk.engineVersion = version major minor patch }

setApiVersion :: Word32 -> Word32 -> Word32 -> InstanceBuilder ()
setApiVersion major minor patch =
  updateAppInfo $
    -- little bit different from the others to disambiguate "apiVersion"
    \(Vk.ApplicationInfo n av en ev _) ->
      Vk.ApplicationInfo n av en ev (version major minor patch)

enableLayer :: String -> InstanceBuilder ()
enableLayer name = enableLayers [name]

enableLayers :: [String] -> InstanceBuilder ()
enableLayers names =
  InstanceBuilder $
    modify $ \info ->
      info
        { Vk.enabledLayerNames =
            (V.++)
              ( Vk.enabledLayerNames info )
              ( V.fromList $ fmap BS.pack names )
        }

enableVulkanExtensions :: [String] -> InstanceBuilder ()
enableVulkanExtensions names =
  InstanceBuilder $
    modify $ \info ->
      info
        { Vk.enabledExtensionNames =
            (V.++)
              ( Vk.enabledExtensionNames info )
              ( V.fromList $ fmap BS.pack names )
        }

createInstance :: MonadIO m => InstanceBuilder () -> m Vk.Instance
createInstance instanceBuilder =
  liftIO $ do
    info <- execBuilder Vk.zero instanceBuilder.builder
    Vk.createInstance info Nothing

destroyInstance :: MonadIO m => Vk.Instance -> m ()
destroyInstance = flip Vk.destroyInstance Nothing

withInstance :: MonadUnliftIO m => InstanceBuilder () -> (Vk.Instance -> m a) -> m a
withInstance builder f =
  withRunInIO $ \run ->
    bracket
      ( createInstance builder )
      ( destroyInstance )
      ( run . f )
