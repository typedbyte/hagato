{-# LANGUAGE LambdaCase #-}
module Hagato.GLTF.Transmission where

-- aeson
import Data.Aeson (eitherDecode)

-- base
import Control.Monad.IO.Class (MonadIO)
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
import Hagato.GLTF.BufferView qualified as V
import Hagato.GLTF.Asset      (Asset, getAsset, nodes)
import Hagato.GLTF.Buffer     (Buffer, uri)
import Hagato.GLTF.Chunk      (getChunks)
import Hagato.GLTF.Header     (Header, getHeader)
import Hagato.GLTF.Image      (Image(imageData), ImageData(..))
import Hagato.GLTF.Index      (AccessorIx, BufferIx, BufferViewIx, Index, get)
import Hagato.GLTF.Node       (Node, name)
import Hagato.GLTF.URI        qualified as URI

data Transmission = Transmission
  { asset   :: Asset
  , chunks  :: V.Vector BS.ByteString
  , context :: Maybe FilePath
  , header  :: Maybe Header
  }

newtype Cache = Cache { unCache :: M.Map Buffer BS.ByteString }

empty :: Cache
empty = Cache M.empty

lookup :: Buffer -> Cache -> Maybe BS.ByteString
lookup buffer = M.lookup buffer . coerce

insert :: Buffer -> BS.ByteString -> Cache -> Cache
insert buffer bytes = Cache . M.insert buffer bytes . coerce

data Source
  = FromBytes BS.ByteString
  | FromFile FilePath

parse :: Maybe FilePath -> BL.ByteString -> Either String Transmission
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

fromMaybeM :: MonadFail m => String -> Maybe a -> m a
fromMaybeM ctx = \case
  Just a  -> pure a
  Nothing -> fail $ "Invalid Just assumption: " ++ ctx

fetchM :: Index i Asset a => i -> Transmission -> a
fetchM i = get i . (.asset)

getNodeM :: MonadFail m => T.Text -> Transmission -> m Node
getNodeM nodeName tm =
  fromMaybeM (T.unpack nodeName) $
    V.find ((== Just nodeName) . (.name)) tm.asset.nodes

loadAccessorM :: (MonadFail m, MonadIO m) => A.Accessor -> Transmission -> Cache -> m (BS.ByteString, Cache)
loadAccessorM accessor tm cache =
  case accessor.bufferView of
    Just (i, offset) -> do
      (bytes, newCache) <- loadBufferViewIxM i tm cache
      pure (crop offset bytes, newCache)
    Nothing ->
      fail "Sparse accessor values are not implemented ye"
  where
    crop offset
      = BS.take byteSize
      . BS.drop offset
    byteSize
      = accessor.count
      * A.elementCount accessor.attributeType
      * A.componentByteSize accessor.componentType

loadAccessorIxM :: (MonadFail m, MonadIO m) => AccessorIx -> Transmission -> Cache -> m (BS.ByteString, Cache)
loadAccessorIxM i =
  fetchM i >>= loadAccessorM

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

loadBufferIxM :: (MonadFail m, MonadIO m) => BufferIx -> Transmission -> Cache -> m (BS.ByteString, Cache)
loadBufferIxM i =
  fetchM i >>= loadBufferM

loadBufferViewM :: (MonadFail m, MonadIO m) => V.BufferView -> Transmission -> Cache -> m (BS.ByteString, Cache)
loadBufferViewM view tm cache = do
  (bytes, newCache) <- loadBufferIxM view.buffer tm cache
  pure (crop bytes, newCache)
    where
      crop
        = BS.take view.byteLength
        . BS.drop view.byteOffset

loadBufferViewIxM :: (MonadFail m, MonadIO m) => BufferViewIx -> Transmission -> Cache -> m (BS.ByteString, Cache)
loadBufferViewIxM i =
  fetchM i >>= loadBufferViewM

loadImageM :: (MonadFail m, MonadIO m) => Image -> Transmission -> Cache -> m (BS.ByteString, Cache)
loadImageM image =
  case image.imageData of
    ImageBuffer bufferViewIx _mime -> loadBufferViewIxM bufferViewIx
    ImageURI uri _maybeMime        -> loadURIM uri

loadURIM :: (MonadFail m, MonadIO m) => URI.URI -> Transmission -> Cache -> m (BS.ByteString, Cache)
loadURIM uri tm cache = do
  bytes <- URI.load tm.context uri
  pure (bytes, cache)