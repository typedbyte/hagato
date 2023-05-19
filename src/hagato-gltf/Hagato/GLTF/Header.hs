module Hagato.GLTF.Header where

-- base
import Data.Word (Word32)

-- binary
import Data.Binary.Get (Get, getWord32le)

data Header = Header
  { version :: Word32
  , length  :: Word32
  }

getHeader :: Get Header
getHeader = do
  0x46546C67 <- getWord32le
  Header
    <$> getWord32le
    <*> getWord32le