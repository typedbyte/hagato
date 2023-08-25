-----------------------------------------------------------------------------
-- |
-- Module      : Hagato.GLTF.Chunk
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Types and functions for handling chunks, which are buffer payloads (vertex
-- buffers, etc.) found in binary glTF files.
-----------------------------------------------------------------------------
module Hagato.GLTF.Chunk where

-- binary
import Data.Binary.Get (Get, getByteString, getWord32le, isEmpty)

-- bytestring
import Data.ByteString qualified as BS

-- vector
import Data.Vector qualified as V

-- | Reads one chunk of a binary glTF file.
getChunk :: Get BS.ByteString
getChunk = do
  dataLength <- getWord32le
  0x004E4942 <- getWord32le
  getByteString (fromIntegral dataLength)

-- | Reads all chunks of a binary glTF file.
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
