{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
module Hagato.Vulkan.DescriptorSetAllocator where

-- base
import Control.Exception      (bracket, catch, throwIO)
import Control.Monad          (forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef             (IORef, newIORef, readIORef, writeIORef)
import Data.Word              (Word32)

-- containers
import Data.Sequence (Seq((:<|)), (<|), (><))

-- hagato:with-vulkan
import Hagato.Vulkan.DescriptorSet as Set

-- unliftio-core
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)

-- vector
import Data.Vector qualified as V

-- vulkan
import Vulkan           qualified as Vk
import Vulkan.Exception qualified as Vk
import Vulkan.Zero      qualified as Vk

type PoolSizeFactors = V.Vector (Vk.DescriptorType, Float)

defaultSizeFactors :: PoolSizeFactors
defaultSizeFactors =
  [ (Vk.DESCRIPTOR_TYPE_SAMPLER               , 0.5)
  , (Vk.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, 4.0)
  , (Vk.DESCRIPTOR_TYPE_SAMPLED_IMAGE         , 4.0)
  , (Vk.DESCRIPTOR_TYPE_STORAGE_IMAGE         , 1.0)
  , (Vk.DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER  , 1.0)
  , (Vk.DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER  , 1.0)
  , (Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER        , 2.0)
  , (Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER        , 2.0)
  , (Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC, 1.0)
  , (Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC, 1.0)
  , (Vk.DESCRIPTOR_TYPE_INPUT_ATTACHMENT      , 0.5)
  ]

data DescriptorSetAllocator = DescriptorSetAllocator
  { device         :: Vk.Device
  , sizeFactors    :: PoolSizeFactors
  , maxSetsPerPool :: Word32
  , currentPool    :: IORef Vk.DescriptorPool
  , pools          :: IORef (Seq Vk.DescriptorPool, Seq Vk.DescriptorPool)
  }

createDescriptorSetAllocator
  :: MonadIO m
  => Vk.Device
  -> Word32
  -> PoolSizeFactors
  -> m DescriptorSetAllocator
createDescriptorSetAllocator device maxSetsPerPool sizeFactors =
  liftIO $ do
    startPool   <- createPool device sizeFactors maxSetsPerPool
    currentPool <- newIORef startPool
    pools       <- newIORef ([startPool], [])
    pure DescriptorSetAllocator{..}

destroyDescriptorSetAllocator :: MonadIO m => DescriptorSetAllocator -> m ()
destroyDescriptorSetAllocator allocator =
  liftIO $ do
    (usedPools, freePools) <- readIORef allocator.pools
    let destroyPool = Set.destroyDescriptorPool allocator.device
    forM_ usedPools destroyPool
    forM_ freePools destroyPool

withDescriptorSetAllocator
  :: MonadUnliftIO m
  => Vk.Device
  -> Word32
  -> PoolSizeFactors
  -> (DescriptorSetAllocator -> m a)
  -> m a
withDescriptorSetAllocator device maxSetsPerPool sizeFactors f =
  withRunInIO $ \run ->
    bracket
      ( createDescriptorSetAllocator device maxSetsPerPool sizeFactors )
      ( destroyDescriptorSetAllocator )
      ( run . f )

allocateDescriptorSet
  :: MonadIO m
  => DescriptorSetAllocator
  -> Vk.DescriptorSetLayout
  -> m Vk.DescriptorSet
allocateDescriptorSet allocator layout =
  liftIO $ do
    currentPool <- readIORef allocator.currentPool
    catch (allocateSet currentPool) $
      \err@(Vk.VulkanException result) ->
        case result of
          Vk.ERROR_FRAGMENTED_POOL    -> reallocate
          Vk.ERROR_OUT_OF_POOL_MEMORY -> reallocate
          _                           -> throwIO err
  where
    allocateSet pool =
      fmap (V.! 0) $
        Set.allocateDescriptorSets allocator.device $
          Vk.zero
            { Vk.descriptorPool = pool
            , Vk.setLayouts     = [layout]
            }
    reallocate = do
      newPool <- renewCurrentPool allocator
      allocateSet newPool

resetPools :: MonadIO m => DescriptorSetAllocator -> m ()
resetPools allocator =
  liftIO $ do
    currentPool <- readIORef allocator.currentPool
    (usedPools, freePools) <- readIORef allocator.pools
    forM_ usedPools (Set.resetDescriptorPool allocator.device)
    let newFreePools = dropOne usedPools >< freePools -- first one is current
    writeIORef allocator.pools ([currentPool], newFreePools)
  where
    dropOne = \case
      _ :<| ps -> ps
      empty    -> empty

createPool :: Vk.Device -> PoolSizeFactors -> Word32 -> IO Vk.DescriptorPool
createPool device sizeFactors maxSetsPerPool =
  let
    poolSizes =
      flip V.map sizeFactors $ \(type', factor) ->
        Vk.zero
          { Vk.type' = type'
          , Vk.descriptorCount =
              round $
                factor * fromIntegral maxSetsPerPool
          }
  in
    Set.createDescriptorPool device $
      Vk.zero
        { Vk.maxSets   = maxSetsPerPool
        , Vk.poolSizes = poolSizes
        }

renewCurrentPool :: DescriptorSetAllocator -> IO Vk.DescriptorPool
renewCurrentPool allocator = do
  (usedPools, freePools)  <- readIORef allocator.pools
  (newPool, newFreePools) <-
    case freePools of
      p :<| ps ->
        pure (p, ps)
      empty ->
        (,empty) <$>
          createPool
            allocator.device
            allocator.sizeFactors
            allocator.maxSetsPerPool
  writeIORef allocator.currentPool newPool
  writeIORef allocator.pools (newPool <| usedPools, newFreePools)
  pure newPool