{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Hagato.GLTF.Accessor where

-- aeson
import Data.Aeson (FromJSON(parseJSON), Object, Value, (.:), (.:?), (.!=), withObject)

-- text
import Data.Text qualified as T

-- vector
import Data.Vector          qualified as V
import Data.Vector.Storable qualified as S

import Hagato.GLTF.Aeson (failWithContext)
import Hagato.GLTF.Index (AccessorIx(value), BufferViewIx, Index, get)

-- | Represents a byte offset within a buffer.
type Offset = Int

-- | Specifies if an accessor's elements are scalars, vectors, or matrices.
data AttributeType
  = Scalar
  | Vec2
  | Vec3
  | Vec4
  | Mat2
  | Mat3
  | Mat4
  deriving (Eq, Ord, Show)

instance FromJSON AttributeType where
  parseJSON value = do
    text <- parseJSON value
    case text :: T.Text of
      "SCALAR" -> pure Scalar
      "VEC2"   -> pure Vec2
      "VEC3"   -> pure Vec3
      "VEC4"   -> pure Vec4
      "MAT2"   -> pure Mat2
      "MAT3"   -> pure Mat3
      "MAT4"   -> pure Mat4
      invalid  -> failWithContext "AttributeType" invalid

elementCount :: AttributeType -> Int
elementCount = \case
  Scalar -> 1
  Vec2   -> 2
  Vec3   -> 3
  Vec4   -> 4
  Mat2   -> 4
  Mat3   -> 9
  Mat4   -> 16

-- | The datatype of the an accessor's components.
data ComponentType
  = Byte
  | UnsignedByte
  | Short
  | UnsignedShort
  | UnsignedInt
  | Float
  deriving (Eq, Ord, Show)

instance FromJSON ComponentType where
  parseJSON value = do
    number <- parseJSON value
    case number :: Int of
      5120    -> pure Byte
      5121    -> pure UnsignedByte
      5122    -> pure Short
      5123    -> pure UnsignedShort
      5125    -> pure UnsignedInt
      5126    -> pure Float
      invalid -> failWithContext "ComponentType" invalid

componentByteSize :: ComponentType -> Int
componentByteSize = \case
  Byte          -> 1
  UnsignedByte  -> 1
  Short         -> 2
  UnsignedShort -> 2
  UnsignedInt   -> 4
  Float         -> 4

-- | Sparse storage of accessor values that deviate from their initialization value.
data Sparse = Sparse
  { count           :: Int
  , ixBufferView    :: BufferViewIx
  , ixByteOffset    :: Offset
  , ixComponentType :: ComponentType
  , valBufferView   :: BufferViewIx
  , valByteOffset   :: Offset
  }
  deriving (Eq, Ord, Show)

instance FromJSON Sparse where
  parseJSON = withObject "Sparse" $ \v -> do
    n      <- v .: "count"
    ixObj  <- v .: "indices"
    ixBuf  <- ixObj .: "bufferView"
    ixOff  <- ixObj .: "byteOffset"
    ixType <- ixObj .: "componentType"
    valObj <- v .: "values"
    valBuf <- valObj .: "bufferView"
    valOff <- valObj .: "byteOffset"
    pure $
      Sparse n ixBuf ixOff ixType valBuf valOff

-- | A typed view into a buffer view that contains raw binary data.
data Accessor = Accessor
  { bufferView    :: Maybe (BufferViewIx, Offset)
  , componentType :: ComponentType
  , normalized    :: Bool
  , count         :: Int
  , attributeType :: AttributeType
  , max           :: S.Vector Float
  , min           :: S.Vector Float
  , sparse        :: Maybe Sparse
  , name          :: Maybe T.Text
  , extensions    :: Maybe Object
  , extras        :: Maybe Value
  }
  deriving (Eq, Ord, Show)

instance FromJSON Accessor where
  parseJSON = withObject "Accessor" $ \v -> do
    Accessor
      <$> parseBufferView v
      <*> v .:  "componentType"
      <*> v .:? "normalized" .!= False
      <*> v .:  "count"
      <*> v .:  "type"
      <*> v .:? "max" .!= S.empty
      <*> v .:? "min" .!= S.empty
      <*> v .:? "sparse"
      <*> v .:? "name"
      <*> v .:? "extensions"
      <*> v .:? "extras"
    where
      parseBufferView v = do
        bufView <- v .:? "bufferView"
        bufOff  <- v .:? "byteOffset" .!= 0
        pure $ fmap (\bv -> (bv, bufOff)) bufView

instance Index AccessorIx (V.Vector Accessor) Accessor where
  get i vec = vec V.! i.value
  {-# INLINE get #-}