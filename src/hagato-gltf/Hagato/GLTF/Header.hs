-----------------------------------------------------------------------------
-- |
-- Module      : Hagato.GLTF.Header
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Types and functions for handling the header of a binary glTF file.
-----------------------------------------------------------------------------
module Hagato.GLTF.Header where

-- base
import Data.Word (Word32)

-- binary
import Data.Binary.Get (Get, getWord32le)

-- | Represents the header of a binary glTF file.
data Header = Header
  { version :: Word32
    -- ^ The version of the binary glTF container format.
  , length :: Word32
    -- ^ The total length of the binary glTF file.
  }

-- | Reads the header of a binary glTF file.
getHeader :: Get Header
getHeader = do
  0x46546C67 <- getWord32le
  Header
    <$> getWord32le
    <*> getWord32le