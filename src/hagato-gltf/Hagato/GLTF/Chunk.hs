module Hagato.GLTF.Chunk where

-- binary
import Data.Binary.Get (Get, getByteString, getWord32le, isEmpty)

-- bytestring
import Data.ByteString qualified as BS

-- vector
import Data.Vector qualified as V

getChunk :: Get BS.ByteString
getChunk = do
  dataLength <- getWord32le
  0x004E4942 <- getWord32le
  getByteString (fromIntegral dataLength)

getChunks :: Get (V.Vector BS.ByteString)
getChunks = V.fromList <$> go
  where
    go = do
      end <- isEmpty
      if end then
        pure []
      else do
        chunk <- getChunk
        rest  <- go
        pure (chunk:rest)
