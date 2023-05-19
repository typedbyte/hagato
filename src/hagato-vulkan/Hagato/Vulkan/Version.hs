module Hagato.Vulkan.Version where

-- base
import Data.Bits (shiftL, (.|.))
import Data.Word (Word32)

version :: Word32 -> Word32 -> Word32 -> Word32
version major minor patch =
  shiftL major 22 .|. shiftL minor 12 .|. patch