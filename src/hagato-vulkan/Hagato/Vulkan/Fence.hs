{-# LANGUAGE OverloadedLists #-}
module Hagato.Vulkan.Fence
  ( createFence
  , destroyFence
  , withFence
  , resetFence
  , waitForFence
  , waitForFences
  , module Vulkan.Core10.Fence
  ) where

-- base
import Control.Exception      (bracket)
import Control.Monad.IO.Class (MonadIO)
import Data.Word              (Word64)

-- unliftio-core
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)

-- vector
import Data.Vector qualified as V

-- vulkan
import Vulkan              qualified as Vk
import Vulkan.Core10.Fence hiding (createFence, destroyFence, withFence, waitForFences)
import Vulkan.Zero         qualified as Vk

createFence :: MonadIO m => Vk.Device -> m Vk.Fence
createFence device =
  Vk.createFence
    ( device )
    ( Vk.zero { flags = Vk.FENCE_CREATE_SIGNALED_BIT } )
    ( Nothing )

destroyFence :: MonadIO m => Vk.Device -> Vk.Fence -> m ()
destroyFence device fence =
  Vk.destroyFence device fence Nothing

withFence :: MonadUnliftIO m => Vk.Device -> (Vk.Fence -> m a) -> m a
withFence device f =
  withRunInIO $ \run ->
    bracket
      ( createFence device )
      ( destroyFence device )
      ( run . f )

resetFence :: MonadIO m => Vk.Device -> Vk.Fence -> m ()
resetFence device fence =
  Vk.resetFences device [fence]
{-# INLINE resetFence #-}

waitForFence :: MonadIO m => Vk.Device -> Vk.Fence -> Word64 -> m Vk.Result
waitForFence device fence timeout =
  Vk.waitForFences device [fence] True timeout
{-# INLINE waitForFence #-}

waitForFences :: MonadIO m => Vk.Device -> V.Vector Vk.Fence -> Word64 -> m Vk.Result
waitForFences device fences timeout =
  Vk.waitForFences device fences True timeout
{-# INLINE waitForFences #-}
