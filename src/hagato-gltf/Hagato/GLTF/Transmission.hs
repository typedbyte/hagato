{-# LANGUAGE LambdaCase #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hagato.GLTF.Transmission
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Types and functions for handling glTF transmissions.
-----------------------------------------------------------------------------
module Hagato.GLTF.Transmission
  ( -- * glTF File Handling
    Transmission(..)
  , Source(..)
  , parseSource
  , parse
    -- * glTF Operations
  , fetchM
  , getNodeM
  , loadAccessorIxM
  , loadAccessorM
  , loadBufferIxM
  , loadBufferM
  , loadBufferViewIxM
  , loadBufferViewM
  , loadImageM
  , loadURIM
  , fromMaybeM
    -- * glTF Buffer Caching
  , Cache
  , empty
  , insert
  , lookup
  ) where

-- aeson
import Data.Aeson (eitherDecode)

-- base
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Coerce            (coerce)
import Prelude hiding         (lookup)

-- binary
import Data.Binary.Get (getWord32le, runGetOrFail)

-- bytestring
import Data.ByteString      qualified as BS
import Data.ByteString.Lazy qualified as BL

-- containers
import Data.Map.Strict qualified as M

-- text
import Data.Text qualified as T

-- vector
import Data.Vector qualified as V

import Hagato.GLTF.Accessor   qualified as A
import Hagato.GLTF.Asset      (Asset, getAsset, nodes)
import Hagato.GLTF.Buffer     (Buffer, uri)
import Hagato.GLTF.BufferView qualified as V
import Hagato.GLTF.Chunk      (getChunks)
import Hagato.GLTF.Header     (Header, getHeader)
import Hagato.GLTF.Image      (Image(imageData), ImageData(..))
import Hagato.GLTF.Index      (AccessorIx, BufferIx, BufferViewIx, Index, get)
import Hagato.GLTF.Node       (Node, name)
import Hagato.GLTF.URI        qualified as URI

-- | Represents a glTF transmission, which is usually provided as a glTF file.
data Transmission = Transmission
  { asset :: Asset
    -- ^ The root object which contains data about the actual scenes and models.
  , chunks :: V.Vector BS.ByteString
    -- ^ The bytestrings which contain the buffer payloads (vertex buffers, etc.)
    -- if the transmission is a binary glTF file.
  , context :: Maybe FilePath
    -- ^ The source of the glTF transmission, which is important for resolving
    -- relative URIs.
  , header :: Maybe Header
    -- ^ Meta information if the transmission is a binary glTF file.
  }

-- | Represents a cache which maps glTF buffers to their actual payloads. Using
-- the cache ensures that big payloads (large vertex buffers, images, etc.) are
-- loaded into memory only once.
newtype Cache = Cache (M.Map Buffer BS.ByteString)

-- | The empty cache.
empty :: Cache
empty = Cache M.empty

-- | Lookup the payload that belongs to the specified glTF buffer.
lookup :: Buffer -> Cache -> Maybe BS.ByteString
lookup buffer = M.lookup buffer . coerce

-- | Insert a pair of glTF buffer and corresponding payload into the cache.
insert :: Buffer -> BS.ByteString -> Cache -> Cache
insert buffer bytes = Cache . M.insert buffer bytes . coerce

-- | Represents the source of a glTF transmission.
data Source
  = FromBytes BL.ByteString
  | FromFile FilePath

-- | Parses a glTF transmission (both binary and non-binary formats) from the
-- specified source.
parseSource :: MonadIO m => Source -> m (Either String Transmission)
parseSource source =
  liftIO $
    case source of
      FromBytes bytes ->
        pure $ parse Nothing bytes
      FromFile file -> do
        bytes <- BL.readFile file
        pure $ parse (Just file) bytes

-- | A pure version of 'parseSource'.
parse
  :: Maybe FilePath
  -- ^ The source of the glTF transmission.
  -> BL.ByteString
  -- ^ The raw bytes containing the glTF transmission.
  -> Either String Transmission
  -- ^ The parsed glTF transmission, or an error text.
parse context bytes =
  case runGetOrFail getWord32le bytes of
    Right (_, _, 0x46546C67) -> parseGLB
    _                        -> parseGLTF
  where
    getGLB = do
      header <- getHeader
      asset  <- getAsset
      chunks <- getChunks
      pure $
        Transmission asset chunks context (Just header)
    parseGLB =
      case runGetOrFail getGLB bytes of
        Right (_, _, tm)  -> Right tm
        Left  (_, _, err) -> Left err
    parseGLTF =
      case eitherDecode bytes of
        Right asset -> Right (Transmission asset V.empty context Nothing)
        Left err    -> Left err

-- | A utility function that extracts the value from a 'Maybe', or fails with a string.
fromMaybeM :: MonadFail m => String -> Maybe a -> m a
fromMaybeM ctx = \case
  Just a  -> pure a
  Nothing -> fail $ "Invalid Just assumption: " ++ ctx

-- | Gets an indexed value from the asset in the glTF transmission.
fetchM :: Index i Asset a => i -> Transmission -> a
fetchM i = get i . (.asset)

-- | Gets the node with the specified name from the glTF transmission.
getNodeM :: MonadFail m => T.Text -> Transmission -> m Node
getNodeM nodeName tm =
  fromMaybeM (T.unpack nodeName) $
    V.find ((== Just nodeName) . (.name)) tm.asset.nodes

-- | Loads the data that is associated with the specified accessor.
loadAccessorM :: (MonadFail m, MonadIO m) => A.Accessor -> Transmission -> Cache -> m (BS.ByteString, Cache)
loadAccessorM accessor tm cache =
  case accessor.bufferView of
    Just (i, offset) -> do
      (bytes, newCache) <- loadBufferViewIxM i tm cache
      pure (crop offset bytes, newCache)
    Nothing ->
      fail "Sparse accessor values are not implemented yet"
  where
    crop offset
      = BS.take byteSize
      . BS.drop offset
    byteSize
      = accessor.count
      * A.elementCount accessor.attributeType
      * A.componentByteSize accessor.componentType

-- | Loads the data that is associated with the specified accessor index.
loadAccessorIxM :: (MonadFail m, MonadIO m) => AccessorIx -> Transmission -> Cache -> m (BS.ByteString, Cache)
loadAccessorIxM i =
  fetchM i >>= loadAccessorM

-- | Loads the data that is associated with the specified buffer.
loadBufferM :: (MonadFail m, MonadIO m) => Buffer -> Transmission -> Cache -> m (BS.ByteString, Cache)
loadBufferM buffer tm cache =
  case lookup buffer cache of
    Just bytes ->
      pure (bytes, cache)
    Nothing -> do
      bytes <-
        case buffer.uri of
          Just uri -> URI.load tm.context uri
          Nothing  -> fromMaybeM "GLB buffer" $ tm.chunks V.!? 0
      pure (bytes, insert buffer bytes cache)

-- | Loads the data that is associated with the specified buffer index.
loadBufferIxM :: (MonadFail m, MonadIO m) => BufferIx -> Transmission -> Cache -> m (BS.ByteString, Cache)
loadBufferIxM i =
  fetchM i >>= loadBufferM

-- | Loads the data that is associated with the specified buffer view.
loadBufferViewM :: (MonadFail m, MonadIO m) => V.BufferView -> Transmission -> Cache -> m (BS.ByteString, Cache)
loadBufferViewM view tm cache = do
  (bytes, newCache) <- loadBufferIxM view.buffer tm cache
  pure (crop bytes, newCache)
    where
      crop
        = BS.take view.byteLength
        . BS.drop view.byteOffset

-- | Loads the data that is associated with the specified buffer view index.
loadBufferViewIxM :: (MonadFail m, MonadIO m) => BufferViewIx -> Transmission -> Cache -> m (BS.ByteString, Cache)
loadBufferViewIxM i =
  fetchM i >>= loadBufferViewM

-- | Loads the data that is associated with the specified image.
loadImageM :: (MonadFail m, MonadIO m) => Image -> Transmission -> Cache -> m (BS.ByteString, Cache)
loadImageM image =
  case image.imageData of
    ImageBuffer bufferViewIx _mime -> loadBufferViewIxM bufferViewIx
    ImageURI uri _maybeMime        -> loadURIM uri

-- | Loads the data that is associated with the specified URI.
loadURIM :: (MonadFail m, MonadIO m) => URI.URI -> Transmission -> Cache -> m (BS.ByteString, Cache)
loadURIM uri tm cache = do
  bytes <- URI.load tm.context uri
  pure (bytes, cache)