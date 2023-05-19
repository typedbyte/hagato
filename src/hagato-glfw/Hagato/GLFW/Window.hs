module Hagato.GLFW.Window
  ( GlfwWindow(..)
  , glfwStrategy
  ) where

-- base
import Control.Concurrent.MVar (MVar, isEmptyMVar, newEmptyMVar, tryPutMVar, tryTakeMVar)
import Control.Exception       (throwIO)
import Data.IORef              (IORef, atomicModifyIORef', newIORef)
import Foreign                 (Ptr, Int32, Word64, alloca, nullPtr, peek)
import Foreign.C.String        (peekCString)

-- GLFW-b
import Graphics.UI.GLFW qualified as GLFW

-- hagato:with-core
import Hagato.Core (Event(..), Input(Input), Vec2(Vec2))

-- hagato:with-vulkan
import Hagato.Vulkan qualified as Vk

-- vulkan
import Vulkan qualified as Vk

import Hagato.GLFW.Input    (mapKeyState, mapKey, mapModifiers, mapMouseButton, mapMouseButtonState)
import Hagato.GLFW.Instance (GLFW)

data GlfwWindow = GlfwWindow
  { handle  :: GLFW.Window
  , resized :: MVar ()
  , events  :: IORef [Event]
  }

glfwStrategy :: GLFW -> Vk.WindowStrategy GlfwWindow
glfwStrategy glfw =
  seq glfw $
    Vk.WindowStrategy
      { _createWindow = \width height title -> do
          GLFW.defaultWindowHints
          GLFW.windowHint $ GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI
          maybeWindow <- GLFW.createWindow width height title Nothing Nothing
          case maybeWindow of
            Nothing     -> fail "Could not create GLFW window."
            Just window -> do
              ref <- newIORef []
              var <- newEmptyMVar
              GLFW.setKeyCallback             window (Just $ keyCallback ref)
              GLFW.setMouseButtonCallback     window (Just $ mouseCallback ref)
              GLFW.setScrollCallback          window (Just $ scrollCallback ref)
              GLFW.setFramebufferSizeCallback window (Just $ sizeCallback var)
              GLFW.setWindowCloseCallback     window (Just $ closeCallback ref)
              pure $ GlfwWindow window var ref
      , _destroyWindow = GLFW.destroyWindow . (.handle)
      , _getExtensions = \_window -> do
          cExts <- GLFW.getRequiredInstanceExtensions
          sequence $ fmap peekCString cExts
      , _createSurface = \window vkInstance -> do
          result <-
            peekPtr $
              GLFW.createWindowSurface
                ( Vk.instanceHandle vkInstance )
                ( window.handle )
          case result of
            Left r  -> throwIO $ Vk.VulkanException (Vk.Result r)
            Right r -> pure $ Vk.SurfaceKHR r
      , _destroySurface = \_window vkInstance surface ->
          Vk.destroySurfaceKHR vkInstance surface Nothing
      , _resized = \window -> do
          unchanged <- isEmptyMVar window.resized
          pure $ not unchanged
          {-\window -> do
          --events <- readIORef window.events
          --pure $ not $ null [e | e@(ResizeEvent _) <- events]-}
      , _poll = \window -> do
          GLFW.pollEvents
          events  <- atomicModifyIORef' window.events (\is -> ([], reverse is))
          resized <- tryTakeMVar window.resized
          -- this hack is needed since resize events come in reversed order on Windows,
          -- so we only pick the last one to remain platform-independent.
          allEvents <-
            case resized of
              Nothing -> pure events
              Just () -> do
                (fx,fy) <- GLFW.getFramebufferSize window.handle
                let event = ResizeEvent (Vec2 (fromIntegral fx) (fromIntegral fy))
                pure $ event : events
          (cx,cy) <- GLFW.getCursorPos window.handle 
          pure $ Input allEvents (Vec2 (realToFrac cx) (realToFrac cy))
      }
  where
    peekPtr :: (Ptr a -> Ptr Word64 -> IO Int32) -> IO (Either Int32 Word64)
    peekPtr f =
      alloca $ \ptr -> do
        result <- f nullPtr ptr
        if result == 0 then
          Right <$> peek ptr
        else
          pure (Left result)
    
    addEvent :: IORef [Event] -> Event -> IO ()
    addEvent ref event =
      atomicModifyIORef' ref $ \events -> (event : events , ())
          
    keyCallback :: IORef [Event] -> GLFW.KeyCallback
    keyCallback ref _window key scanCode keyState modifiers =
      addEvent ref $
        KeyEvent hagatoKey scanCode hagatoState hagatoMods
          where
            hagatoKey   = mapKey key
            hagatoState = mapKeyState keyState
            hagatoMods  = mapModifiers modifiers
    
    mouseCallback :: IORef [Event] -> GLFW.MouseButtonCallback
    mouseCallback ref window button buttonState modifiers = do
      (cx,cy) <- GLFW.getCursorPos window
      addEvent ref $
        MouseEvent
          ( Vec2 (realToFrac cx) (realToFrac cy) )
          ( hagatoButton )
          ( hagatoState )
          ( hagatoMods )
      where
        hagatoButton = mapMouseButton button
        hagatoState  = mapMouseButtonState buttonState
        hagatoMods   = mapModifiers modifiers

    scrollCallback :: IORef [Event] -> GLFW.ScrollCallback
    scrollCallback ref window x y = do
      (cx,cy) <- GLFW.getCursorPos window
      addEvent ref $
        ScrollEvent
          ( Vec2 (realToFrac cx) (realToFrac cy) )
          ( Vec2 (realToFrac x) (realToFrac y) )
    
    {-sizeCallback :: IORef [Event] -> GLFW.FramebufferSizeCallback
    sizeCallback ref _window width height = do
      addEvent ref $
        ResizeEvent (Vec2 (fromIntegral width) (fromIntegral height))-}
    
    sizeCallback :: MVar () -> GLFW.FramebufferSizeCallback
    sizeCallback var _window _width _height =
      tryPutMVar var () >>= \_ -> pure ()
    
    closeCallback :: IORef [Event] -> GLFW.WindowCloseCallback
    closeCallback ref _window =
      addEvent ref CloseEvent