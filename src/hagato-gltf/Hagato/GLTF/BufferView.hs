{-# LANGUAGE OverloadedStrings #-}
module Hagato.GLTF.BufferView where

-- aeson
import Data.Aeson (FromJSON(..), Object, Value, (.:), (.:?), (.!=), withObject)

-- text
import Data.Text qualified as T

-- vector
import Data.Vector qualified as V

import Hagato.GLTF.Aeson (failWithContext)
import Hagato.GLTF.Index (BufferIx, BufferViewIx(value), Index, get)

-- | A hint representing the intended GPU buffer type to use with a buffer view.
data BufferViewTarget
  = ArrayBuffer
  | ElementArrayBuffer
  deriving (Eq, Ord, Show)

instance FromJSON BufferViewTarget where
  parseJSON value = do
    number <- parseJSON value
    case number :: Int of
      34962   -> pure ArrayBuffer
      34963   -> pure ElementArrayBuffer
      invalid -> failWithContext "BufferViewTarget" invalid

-- | A view into a buffer generally representing a subset of the buffer.
data BufferView = BufferView
  { buffer     :: BufferIx
  , byteOffset :: Int
  , byteLength :: Int
  , byteStride :: Maybe Int
  , target     :: Maybe BufferViewTarget
  , name       :: Maybe T.Text
  , extensions :: Maybe Object
  , extras     :: Maybe Value
  }
  deriving (Eq, Ord, Show)

instance FromJSON BufferView where
  parseJSON = withObject "BufferView" $ \v ->
    BufferView
      <$> v .:  "buffer"
      <*> v .:? "byteOffset" .!= 0
      <*> v .:  "byteLength"
      <*> v .:? "byteStride"
      <*> v .:? "target"
      <*> v .:? "name"
      <*> v .:? "extensions"
      <*> v .:? "extras"

instance Index BufferViewIx (V.Vector BufferView) BufferView where
  get i vec = vec V.! i.value
  {-# INLINE get #-}