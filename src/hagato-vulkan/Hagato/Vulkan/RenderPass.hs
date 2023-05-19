module Hagato.Vulkan.RenderPass
  ( RenderPassBuilder
  , RenderPass(device, handle)
  , Attachment(..)
  , Subpass(..)
  , defineAttachment
  , defineSubpass
  , defineDependency
  , withLayout
  , createRenderPass
  , destroyRenderPass
  , withRenderPass
  , recreateRenderPass
  , module Vulkan.Core10.Pass
  ) where

-- base
import Control.Exception      (bracket)
import Control.Monad          (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Word              (Word32)

-- unliftio-core
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)

-- vector
import Data.Vector qualified as V

-- vulkan
import Vulkan             qualified as Vk
import Vulkan.Core10.Pass qualified as Rp
import Vulkan.Core10.Pass hiding (RenderPass, createRenderPass, destroyRenderPass,
                                  withRenderPass, createFramebuffer,
                                  destroyFramebuffer, withFramebuffer)
import Vulkan.Zero        qualified as Vk

import Hagato.Vulkan.Swapchain qualified as Sw
import Hagato.Vulkan.Builder   (Builder, execBuilder, get, modify, put)

data RenderPassConfig = RenderPassConfig
  { attachments'  :: [Vk.AttachmentDescription]
  , subpasses'    :: [Vk.SubpassDescription]
  , dependencies' :: [Vk.SubpassDependency]
  }

instance Vk.Zero RenderPassConfig where
  zero =
    RenderPassConfig
      { attachments'  = []
      , subpasses'    = []
      , dependencies' = []
      }

newtype RenderPassBuilder a =
  RenderPassBuilder { builder :: Builder (Word32, Word32, Word32, RenderPassConfig) a }
    deriving (Applicative, Functor, Monad, MonadFail, MonadIO)

newtype Attachment = Attachment { index :: Word32 }

newtype Subpass = Subpass { index :: Word32 }

defineAttachment :: Vk.AttachmentDescription -> RenderPassBuilder Attachment
defineAttachment description =
  RenderPassBuilder $ do
    (aix, six, dix, cfg) <- get
    put (aix + 1, six, dix, cfg { attachments' = cfg.attachments' ++ [description] })
    pure (Attachment aix)

defineSubpass :: Vk.SubpassDescription -> RenderPassBuilder Subpass
defineSubpass description =
  RenderPassBuilder $ do
    (aix, six, dix, cfg) <- get
    put (aix, six + 1, dix, cfg { subpasses' = cfg.subpasses' ++ [description] })
    pure (Subpass six)

defineDependency :: Vk.SubpassDependency -> RenderPassBuilder ()
defineDependency dependency =
  RenderPassBuilder $
    modify $ \(aix, six, dix, cfg) ->
      (aix, six, dix + 1, cfg { dependencies' = cfg.dependencies' ++ [dependency] })

withLayout :: Attachment -> Vk.ImageLayout -> Vk.AttachmentReference
withLayout attachment layout =
  Vk.zero
    { Rp.attachment = attachment.index
    , Rp.layout     = layout
    }

data RenderPass = RenderPass
  { device    :: Vk.Device
  , handle    :: Vk.RenderPass
  , updater   :: Sw.Swapchain -> RenderPassBuilder ()
  }

renewRenderPass :: Sw.Swapchain -> RenderPass -> IO RenderPass
renewRenderPass swapchain renderPass
  | swapchain.handle == Vk.NULL_HANDLE      = pure $ renderPass { handle = Vk.NULL_HANDLE }
  | swapchain.format == Vk.FORMAT_UNDEFINED = pure $ renderPass { handle = Vk.NULL_HANDLE }
  | otherwise = do
      (aix, six, dix, cfg) <-
        execBuilder
          (0, 0, 0, Vk.zero)
          (renderPass.updater swapchain).builder
      let
        createInfo =
          Vk.zero
            { Rp.attachments  = V.fromListN (fromIntegral aix) cfg.attachments'
            , Rp.subpasses    = V.fromListN (fromIntegral six) cfg.subpasses'
            , Rp.dependencies = V.fromListN (fromIntegral dix) cfg.dependencies'
            }
      newHandle <- Vk.createRenderPass swapchain.device createInfo Nothing
      pure $ renderPass { handle = newHandle }

createRenderPass :: MonadIO m => Sw.Swapchain -> (Sw.Swapchain -> RenderPassBuilder ()) -> m RenderPass
createRenderPass swapchain f =
  liftIO . recreateRenderPass swapchain $
    RenderPass
      { device    = swapchain.device
      , handle    = Vk.NULL_HANDLE
      , updater   = f
      }

destroyRenderPass :: MonadIO m => RenderPass -> m ()
destroyRenderPass renderPass =
  when (renderPass.handle /= Vk.NULL_HANDLE) $
    Vk.destroyRenderPass renderPass.device renderPass.handle Nothing

withRenderPass
  :: MonadUnliftIO m
  => Sw.Swapchain
  -> (Sw.Swapchain -> RenderPassBuilder ())
  -> (RenderPass -> m a)
  -> m a
withRenderPass swapchain f g =
  withRunInIO $ \run ->
    bracket
      ( createRenderPass swapchain f )
      ( destroyRenderPass )
      ( run . g )

recreateRenderPass :: MonadIO m => Sw.Swapchain -> RenderPass -> m RenderPass
recreateRenderPass swapchain = liftIO . renewRenderPass swapchain
