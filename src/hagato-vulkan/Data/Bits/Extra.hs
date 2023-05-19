module Data.Bits.Extra (testAnyBit, testAllBits) where

-- base
import Data.Bits ((.&.), Bits, zeroBits)

testAnyBit :: Bits a => a -> a -> Bool
testAnyBit bits testBits =
  bits .&. testBits /= zeroBits
{-# INLINE testAnyBit #-}

testAllBits :: Bits a => a -> a -> Bool
testAllBits bits testBits =
  bits .&. testBits == testBits
{-# INLINE testAllBits #-}