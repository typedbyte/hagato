{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}
module Hagato.Vulkan.Effectful.Memory
  ( Memory
  , runMemory
  , createMemoryAllocator
  , destroyMemoryAllocator
  , allocateMemoryAllocator
  , manageMemoryAllocator
  , createBufferMemory
  , createBoundBufferMemory
  , destroyBufferMemory
  , allocateBufferMemory
  , allocateBoundBufferMemory
  , manageBufferMemory
  , manageBoundBufferMemory
  , manageBoundBufferMemory_
  , createImageMemory
  , createBoundImageMemory
  , destroyImageMemory
  , allocateImageMemory
  , allocateBoundImageMemory
  , manageImageMemory
  , manageBoundImageMemory
  , manageBoundImageMemory_
  , bindBufferMemory
  , manageMappedBuffer
  , bindImageMemory
  , manageMappedImage
  , Vk.MemoryAllocator(..)
  , Vk.Allocation(..)
  , Vk.MemoryStrategy(..)
  , Vk.writeBytes
  , Vk.writeVector
  , Vk.vectorByteSize
  ) where

-- base
import Control.Monad (void)
import Data.Kind     (Type)
import Foreign.Ptr   (Ptr)

-- effectful-core
import Effectful                 (Dispatch(..), DispatchOf, Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Static (SideEffects(..), StaticRep, evalStaticRep, getStaticRep, unsafeEff_)

-- hagato:with-vulkan
import Hagato.Vulkan qualified as Vk

-- resource-effectful
import Effectful.Resource (Key, Resource, allocate, manage)

data Memory (a :: Type) (k :: Type) :: Effect

type instance DispatchOf (Memory a k) = Static WithSideEffects
newtype instance StaticRep (Memory a k) = Memory (Vk.MemoryStrategy a k)

runMemory :: IOE :> es => Vk.MemoryStrategy a k -> Eff (Memory a k : es) b -> Eff es b
runMemory = evalStaticRep . Memory

createMemoryAllocator :: Memory a k :> es => Vk.Instance -> Vk.PhysicalDevice -> Vk.Device -> Eff es (Vk.MemoryAllocator a k)
createMemoryAllocator vk physicalDevice device = do
  Memory strategy <- getStaticRep
  unsafeEff_ $ strategy._createMemoryAllocator vk physicalDevice device

destroyMemoryAllocator :: Memory a k :> es => Vk.MemoryAllocator a k -> Eff es ()
destroyMemoryAllocator allocator = do
  Memory strategy <- getStaticRep
  unsafeEff_ $ strategy._destroyMemoryAllocator allocator

allocateMemoryAllocator
  :: (Memory a k :> es, Resource :> es)
  => Vk.Instance -> Vk.PhysicalDevice -> Vk.Device -> Eff es (Vk.MemoryAllocator a k, Key)
allocateMemoryAllocator vk physicalDevice device = do
  Memory strategy <- getStaticRep
  allocate
    ( strategy._createMemoryAllocator vk physicalDevice device )
    ( strategy._destroyMemoryAllocator )

manageMemoryAllocator
  :: (Memory a k :> es, Resource :> es)
  => Vk.Instance -> Vk.PhysicalDevice -> Vk.Device -> Eff es (Vk.MemoryAllocator a k)
manageMemoryAllocator vk physicalDevice device =
  fst <$> allocateMemoryAllocator vk physicalDevice device

createBufferMemory
  :: Memory a k :> es
  => Vk.MemoryAllocator a k
  -> Vk.Buffer
  -> Vk.MemoryPropertyFlags
  -> Eff es (Vk.Allocation Vk.Buffer k)
createBufferMemory allocator buffer flags = do
  Memory strategy <- getStaticRep
  unsafeEff_ $ strategy._createBufferMemory allocator buffer flags

createBoundBufferMemory
  :: Memory a k :> es
  => Vk.MemoryAllocator a k
  -> Vk.Buffer
  -> Vk.MemoryPropertyFlags
  -> Eff es (Vk.Allocation Vk.Buffer k)
createBoundBufferMemory allocator buffer flags = do
  Memory strategy <- getStaticRep
  unsafeEff_ $ Vk.createBoundBufferMemory strategy allocator buffer flags

createImageMemory
  :: Memory a k :> es
  => Vk.MemoryAllocator a k -> Vk.Image -> Vk.MemoryPropertyFlags -> Eff es (Vk.Allocation Vk.Image k)
createImageMemory allocator image flags = do
  Memory strategy <- getStaticRep
  unsafeEff_ $ strategy._createImageMemory allocator image flags

createBoundImageMemory
  :: Memory a k :> es
  => Vk.MemoryAllocator a k
  -> Vk.Image
  -> Vk.MemoryPropertyFlags
  -> Eff es (Vk.Allocation Vk.Image k)
createBoundImageMemory allocator image flags = do
  Memory strategy <- getStaticRep
  unsafeEff_ $ Vk.createBoundImageMemory strategy allocator image flags

destroyBufferMemory :: Memory a k :> es => Vk.MemoryAllocator a k -> Vk.Allocation Vk.Buffer k -> Eff es ()
destroyBufferMemory allocator allocation = do
  Memory strategy <- getStaticRep
  unsafeEff_ $ strategy._destroyBufferMemory allocator allocation

destroyImageMemory :: Memory a k :> es => Vk.MemoryAllocator a k -> Vk.Allocation Vk.Image k -> Eff es ()
destroyImageMemory allocator allocation = do
  Memory strategy <- getStaticRep
  unsafeEff_ $ strategy._destroyImageMemory allocator allocation

allocateBufferMemory
  :: (Memory a k :> es, Resource :> es)
  => Vk.MemoryAllocator a k -> Vk.Buffer -> Vk.MemoryPropertyFlags -> Eff es (Vk.Allocation Vk.Buffer k, Key)
allocateBufferMemory allocator buffer flags = do
  Memory strategy <- getStaticRep
  allocate
    ( strategy._createBufferMemory allocator buffer flags )
    ( strategy._destroyBufferMemory allocator )

manageBufferMemory
  :: (Memory a k :> es, Resource :> es)
  => Vk.MemoryAllocator a k -> Vk.Buffer -> Vk.MemoryPropertyFlags -> Eff es (Vk.Allocation Vk.Buffer k)
manageBufferMemory allocator buffer flags =
  fst <$> allocateBufferMemory allocator buffer flags

allocateBoundBufferMemory
  :: (Memory a k :> es, Resource :> es)
  => Vk.MemoryAllocator a k -> Vk.Buffer -> Vk.MemoryPropertyFlags -> Eff es (Vk.Allocation Vk.Buffer k, Key)
allocateBoundBufferMemory allocator buffer flags = do
  Memory strategy <- getStaticRep
  allocate
    ( Vk.createBoundBufferMemory strategy allocator buffer flags )
    ( strategy._destroyBufferMemory allocator )

manageBoundBufferMemory
  :: (Memory a k :> es, Resource :> es)
  => Vk.MemoryAllocator a k -> Vk.Buffer -> Vk.MemoryPropertyFlags -> Eff es (Vk.Allocation Vk.Buffer k)
manageBoundBufferMemory allocator buffer flags =
  fst <$> allocateBoundBufferMemory allocator buffer flags

manageBoundBufferMemory_
  :: (Memory a k :> es, Resource :> es)
  => Vk.MemoryAllocator a k -> Vk.Buffer -> Vk.MemoryPropertyFlags -> Eff es ()
manageBoundBufferMemory_ allocator buffer flags =
  void $ manageBoundBufferMemory allocator buffer flags

allocateImageMemory
  :: (Memory a k :> es, Resource :> es)
  => Vk.MemoryAllocator a k -> Vk.Image -> Vk.MemoryPropertyFlags -> Eff es (Vk.Allocation Vk.Image k, Key)
allocateImageMemory allocator image flags = do
  Memory strategy <- getStaticRep
  allocate
    ( strategy._createImageMemory allocator image flags )
    ( strategy._destroyImageMemory allocator )

manageImageMemory
  :: (Memory a k :> es, Resource :> es)
  => Vk.MemoryAllocator a k -> Vk.Image -> Vk.MemoryPropertyFlags -> Eff es (Vk.Allocation Vk.Image k)
manageImageMemory allocator image flags =
  fst <$> allocateImageMemory allocator image flags

allocateBoundImageMemory
  :: (Memory a k :> es, Resource :> es)
  => Vk.MemoryAllocator a k -> Vk.Image -> Vk.MemoryPropertyFlags -> Eff es (Vk.Allocation Vk.Image k, Key)
allocateBoundImageMemory allocator image flags = do
  Memory strategy <- getStaticRep
  allocate
    ( Vk.createBoundImageMemory strategy allocator image flags )
    ( strategy._destroyImageMemory allocator )

manageBoundImageMemory
  :: (Memory a k :> es, Resource :> es)
  => Vk.MemoryAllocator a k -> Vk.Image -> Vk.MemoryPropertyFlags -> Eff es (Vk.Allocation Vk.Image k)
manageBoundImageMemory allocator image flags =
  fst <$> allocateBoundImageMemory allocator image flags

manageBoundImageMemory_
  :: (Memory a k :> es, Resource :> es)
  => Vk.MemoryAllocator a k -> Vk.Image -> Vk.MemoryPropertyFlags -> Eff es ()
manageBoundImageMemory_ allocator image flags =
  void $ manageBoundImageMemory allocator image flags

bindBufferMemory :: Memory a k :> es => Vk.MemoryAllocator a k -> Vk.Buffer -> Vk.Allocation Vk.Buffer k -> Eff es ()
bindBufferMemory allocator buffer allocation = do
  Memory strategy <- getStaticRep
  unsafeEff_ $ strategy._bindBufferMemory allocator buffer allocation

bindImageMemory :: Memory a k :> es => Vk.MemoryAllocator a k -> Vk.Image -> Vk.Allocation Vk.Image k -> Eff es ()
bindImageMemory allocator image allocation = do
  Memory strategy <- getStaticRep
  unsafeEff_ $ strategy._bindImageMemory allocator image allocation

manageMappedBuffer
  :: (Memory a k :> es, Resource :> es)
  => Vk.MemoryAllocator a k -> Vk.Allocation Vk.Buffer k -> Eff es (Ptr b)
manageMappedBuffer allocator allocation = do
  Memory strategy <- getStaticRep
  manage
    ( Vk.mapBufferMemory strategy allocator allocation )
    ( const $ Vk.unmapBufferMemory strategy allocator allocation )

manageMappedImage
  :: (Memory a k :> es, Resource :> es)
  => Vk.MemoryAllocator a k -> Vk.Allocation Vk.Image k -> Eff es (Ptr b)
manageMappedImage allocator allocation = do
  Memory strategy <- getStaticRep
  manage
    ( Vk.mapImageMemory strategy allocator allocation )
    ( const $ Vk.unmapImageMemory strategy allocator allocation )
