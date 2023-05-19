module Hagato.Vulkan.Memory
  ( MemoryAllocator(..)
  , Allocation(..)
  , MemoryStrategy(..)
  , createMemoryAllocator
  , destroyMemoryAllocator
  , withMemoryAllocator
  , createBufferMemory
  , createBoundBufferMemory
  , destroyBufferMemory
  , withBufferMemory
  , withBoundBufferMemory
  , withBoundBufferMemory_
  , createImageMemory
  , createBoundImageMemory
  , destroyImageMemory
  , withImageMemory
  , withBoundImageMemory
  , withBoundImageMemory_
  , bindBufferMemory
  , bindImageMemory
  , mapBufferMemory
  , unmapBufferMemory
  , withMappedBufferMemory
  , mapImageMemory
  , unmapImageMemory
  , withMappedImageMemory
  , writeBytes
  , writeVector
  , vectorByteSize
  , module Vulkan.Core10.Memory
  ) where

-- base
import Control.Exception      (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Kind              (Type)
import Foreign                (Storable, castPtr, copyBytes, sizeOf)
import Foreign.Ptr            (Ptr)
import Prelude         hiding (map)

-- bytestring
import Data.ByteString        qualified as BS
import Data.ByteString.Unsafe qualified as Unsafe

-- unliftio-core
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)

-- vector
import Data.Vector.Storable qualified as S

-- vulkan
import Vulkan qualified as Vk
import Vulkan.Core10.Memory

data Allocation (t :: Type) (k :: Type) =
  Allocation
    { key    :: k
    , handle :: Vk.DeviceMemory
    , offset :: Vk.DeviceSize
    , size   :: Vk.DeviceSize
    }
    deriving (Eq, Ord, Show)

newtype MemoryAllocator (a :: Type) (k :: Type) = MemoryAllocator { allocator :: a }

data MemoryStrategy (a :: Type) (k :: Type) =
  MemoryStrategy
    { _createMemoryAllocator  :: Vk.Instance -> Vk.PhysicalDevice -> Vk.Device -> IO (MemoryAllocator a k)
    , _destroyMemoryAllocator :: MemoryAllocator a k -> IO ()
    , _createBufferMemory     :: MemoryAllocator a k -> Vk.Buffer -> Vk.MemoryPropertyFlags -> IO (Allocation Vk.Buffer k)
    , _createImageMemory      :: MemoryAllocator a k -> Vk.Image -> Vk.MemoryPropertyFlags -> IO (Allocation Vk.Image k)
    , _destroyBufferMemory    :: MemoryAllocator a k -> Allocation Vk.Buffer k -> IO ()
    , _destroyImageMemory     :: MemoryAllocator a k -> Allocation Vk.Image k -> IO ()
    , _bindBufferMemory       :: MemoryAllocator a k -> Vk.Buffer -> Allocation Vk.Buffer k -> IO ()
    , _bindImageMemory        :: MemoryAllocator a k -> Vk.Image -> Allocation Vk.Image k -> IO ()
    , _mapBufferMemory        :: forall b. MemoryAllocator a k -> Allocation Vk.Buffer k -> IO (Ptr b)
    , _mapImageMemory         :: forall b. MemoryAllocator a k -> Allocation Vk.Image k -> IO (Ptr b)
    , _unmapBufferMemory      :: MemoryAllocator a k -> Allocation Vk.Buffer k -> IO ()
    , _unmapImageMemory       :: MemoryAllocator a k -> Allocation Vk.Image k -> IO ()
    }

createMemoryAllocator
  :: MonadIO m
  => MemoryStrategy a k
  -> Vk.Instance
  -> Vk.PhysicalDevice
  -> Vk.Device
  -> m (MemoryAllocator a k)
createMemoryAllocator strategy vk physicalDevice device =
  liftIO $
    strategy._createMemoryAllocator vk physicalDevice device

destroyMemoryAllocator :: MonadIO m => MemoryStrategy a k -> MemoryAllocator a k -> m ()
destroyMemoryAllocator strategy allocator =
  liftIO $
    strategy._destroyMemoryAllocator allocator

withMemoryAllocator
  :: MonadUnliftIO m
  => MemoryStrategy a k
  -> Vk.Instance
  -> Vk.PhysicalDevice
  -> Vk.Device
  -> (MemoryAllocator a k -> m r)
  -> m r
withMemoryAllocator strategy vk physicalDevice device f =
  withRunInIO $ \run ->
    bracket
      ( strategy._createMemoryAllocator vk physicalDevice device )
      ( strategy._destroyMemoryAllocator )
      ( run . f )

createBufferMemory
  :: MonadIO m
  => MemoryStrategy a k
  -> MemoryAllocator a k
  -> Vk.Buffer
  -> Vk.MemoryPropertyFlags
  -> m (Allocation Vk.Buffer k)
createBufferMemory strategy allocator buffer flags =
  liftIO $
    strategy._createBufferMemory allocator buffer flags

createBoundBufferMemory
  :: MonadIO m
  => MemoryStrategy a k
  -> MemoryAllocator a k
  -> Vk.Buffer
  -> Vk.MemoryPropertyFlags
  -> m (Allocation Vk.Buffer k)
createBoundBufferMemory strategy allocator buffer flags =
  liftIO $ do
    allocation <- strategy._createBufferMemory allocator buffer flags
    strategy._bindBufferMemory allocator buffer allocation
    pure allocation

withBufferMemory
  :: MonadUnliftIO m
  => MemoryStrategy a k
  -> MemoryAllocator a k
  -> Vk.Buffer
  -> Vk.MemoryPropertyFlags
  -> (Allocation Vk.Buffer k -> m r)
  -> m r
withBufferMemory strategy allocator buffer flags f =
  withRunInIO $ \run ->
    bracket
      ( strategy._createBufferMemory allocator buffer flags )
      ( strategy._destroyBufferMemory allocator )
      ( run . f )

withBoundBufferMemory
  :: MonadUnliftIO m
  => MemoryStrategy a k
  -> MemoryAllocator a k
  -> Vk.Buffer
  -> Vk.MemoryPropertyFlags
  -> (Allocation Vk.Buffer k -> m r)
  -> m r
withBoundBufferMemory strategy allocator buffer flags f =
  withRunInIO $ \run ->
    bracket
      ( createBoundBufferMemory strategy allocator buffer flags )
      ( strategy._destroyBufferMemory allocator )
      ( run . f )

withBoundBufferMemory_
  :: MonadUnliftIO m
  => MemoryStrategy a k
  -> MemoryAllocator a k
  -> Vk.Buffer
  -> Vk.MemoryPropertyFlags
  -> m r
  -> m r
withBoundBufferMemory_ strategy allocator buffer flags action =
  withBoundBufferMemory strategy allocator buffer flags (const action)

createImageMemory
  :: MonadIO m
  => MemoryStrategy a k
  -> MemoryAllocator a k
  -> Vk.Image
  -> Vk.MemoryPropertyFlags
  -> m (Allocation Vk.Image k)
createImageMemory strategy allocator image flags =
  liftIO $
    strategy._createImageMemory allocator image flags

createBoundImageMemory
  :: MonadIO m
  => MemoryStrategy a k
  -> MemoryAllocator a k
  -> Vk.Image
  -> Vk.MemoryPropertyFlags
  -> m (Allocation Vk.Image k)
createBoundImageMemory strategy allocator image flags =
  liftIO $ do
    allocation <- strategy._createImageMemory allocator image flags
    strategy._bindImageMemory allocator image allocation
    pure allocation

withImageMemory
  :: MonadUnliftIO m
  => MemoryStrategy a k
  -> MemoryAllocator a k
  -> Vk.Image
  -> Vk.MemoryPropertyFlags
  -> (Allocation Vk.Image k -> m r)
  -> m r
withImageMemory strategy allocator image flags f =
  withRunInIO $ \run ->
    bracket
      ( strategy._createImageMemory allocator image flags )
      ( strategy._destroyImageMemory allocator )
      ( run . f )

withBoundImageMemory
  :: MonadUnliftIO m
  => MemoryStrategy a k
  -> MemoryAllocator a k
  -> Vk.Image
  -> Vk.MemoryPropertyFlags
  -> (Allocation Vk.Image k -> m r)
  -> m r
withBoundImageMemory strategy allocator image flags f =
  withRunInIO $ \run ->
    bracket
      ( createBoundImageMemory strategy allocator image flags )
      ( strategy._destroyImageMemory allocator )
      ( run . f )

withBoundImageMemory_
  :: MonadUnliftIO m
  => MemoryStrategy a k
  -> MemoryAllocator a k
  -> Vk.Image
  -> Vk.MemoryPropertyFlags
  -> m r
  -> m r
withBoundImageMemory_ strategy allocator image flags action =
  withBoundImageMemory strategy allocator image flags (const action)

destroyBufferMemory :: MonadIO m => MemoryStrategy a k -> MemoryAllocator a k -> Allocation Vk.Buffer k -> m ()
destroyBufferMemory strategy allocator allocation =
  liftIO $
    strategy._destroyBufferMemory allocator allocation

destroyImageMemory :: MonadIO m => MemoryStrategy a k -> MemoryAllocator a k -> Allocation Vk.Image k -> m ()
destroyImageMemory strategy allocator allocation =
  liftIO $
    strategy._destroyImageMemory allocator allocation

bindBufferMemory
  :: MonadIO m
  => MemoryStrategy a k
  -> MemoryAllocator a k
  -> Vk.Buffer
  -> Allocation Vk.Buffer k
  -> m ()
bindBufferMemory strategy allocator buffer allocation =
  liftIO $
    strategy._bindBufferMemory allocator buffer allocation

bindImageMemory :: MonadIO m => MemoryStrategy a k -> MemoryAllocator a k -> Vk.Image -> Allocation Vk.Image k -> m ()
bindImageMemory strategy allocator image allocation =
  liftIO $
    strategy._bindImageMemory allocator image allocation

mapBufferMemory :: MonadIO m => MemoryStrategy a k -> MemoryAllocator a k -> Allocation Vk.Buffer k -> m (Ptr b)
mapBufferMemory MemoryStrategy{_mapBufferMemory=map} allocator allocation =
  liftIO $
    map allocator allocation

mapImageMemory :: MonadIO m => MemoryStrategy a k -> MemoryAllocator a k -> Allocation Vk.Image k -> m (Ptr b)
mapImageMemory MemoryStrategy{_mapImageMemory=map} allocator allocation =
  liftIO $
    map allocator allocation

unmapBufferMemory :: MonadIO m => MemoryStrategy a k -> MemoryAllocator a k -> Allocation Vk.Buffer k -> m ()
unmapBufferMemory strategy allocator allocation =
  liftIO $
    strategy._unmapBufferMemory allocator allocation

unmapImageMemory :: MonadIO m => MemoryStrategy a k -> MemoryAllocator a k -> Allocation Vk.Image k -> m ()
unmapImageMemory strategy allocator allocation =
  liftIO $
    strategy._unmapImageMemory allocator allocation

withMappedBufferMemory
  :: MonadUnliftIO m
  => MemoryStrategy a k
  -> MemoryAllocator a k
  -> Allocation Vk.Buffer k
  -> (Ptr b -> m a)
  -> m a
withMappedBufferMemory MemoryStrategy{_mapBufferMemory=map, _unmapBufferMemory=unmap} allocator allocation f =
  withRunInIO $ \run ->
    bracket
      ( map allocator allocation )
      ( const $ unmap allocator allocation )
      ( run . f )

withMappedImageMemory
  :: MonadUnliftIO m
  => MemoryStrategy a k
  -> MemoryAllocator a k
  -> Allocation Vk.Image k
  -> (Ptr () -> m a)
  -> m a
withMappedImageMemory MemoryStrategy{_mapImageMemory=map, _unmapImageMemory=unmap} allocator allocation f =
  withRunInIO $ \run ->
    bracket
      ( map allocator allocation )
      ( const $ unmap allocator allocation )
      ( run . f )

writeBytes :: MonadIO m => Ptr a -> BS.ByteString -> m ()
writeBytes dstPtr bytes =
  liftIO $ Unsafe.unsafeUseAsCString bytes $
    \srcPtr ->
      copyBytes (castPtr dstPtr) srcPtr (BS.length bytes)
{-# INLINE writeBytes #-}

writeVector :: (MonadIO m, Storable a) => Ptr a -> S.Vector a -> m ()
writeVector dstPtr vector =
  liftIO $ S.unsafeWith vector $ \srcPtr ->
    copyBytes
      ( dstPtr )
      ( castPtr srcPtr )
      ( vectorByteSize vector )
{-# INLINE writeVector #-}

vectorByteSize :: forall a. Storable a => S.Vector a -> Int
vectorByteSize vector =
  S.length vector * sizeOf @a undefined
