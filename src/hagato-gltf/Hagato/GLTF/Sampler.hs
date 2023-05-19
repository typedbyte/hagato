{-# LANGUAGE OverloadedStrings #-}
module Hagato.GLTF.Sampler where

-- aeson
import Data.Aeson (FromJSON(..), Object, Value, (.:?), (.!=), withObject)

-- text
import Data.Text qualified as T

-- vector
import Data.Vector qualified as V

import Hagato.GLTF.Aeson (failWithContext)
import Hagato.GLTF.Index (Index, SamplerIx(value), get)

-- | Magnification filter.
data MagnificationFilter
  = MagnificationNearest
  | MagnificationLinear
  | MagnificationDefault
  deriving (Eq, Ord, Show)

instance FromJSON MagnificationFilter where
  parseJSON value = do
    number <- parseJSON value
    case number :: Int of
      9728    -> pure MagnificationNearest
      9729    -> pure MagnificationLinear
      invalid -> failWithContext "MagnificationFilter" invalid

-- | Minification filter.
data MinificationFilter
  = MinificationNearest
  | MinificationLinear
  | NearestMipmapNearest
  | LinearMipmapNearest
  | NearestMipmapLinear
  | LinearMipmapLinear
  | MinificationDefault
  deriving (Eq, Ord, Show)

instance FromJSON MinificationFilter where
  parseJSON value = do
    number <- parseJSON value
    case number :: Int of
      9728    -> pure MinificationNearest
      9729    -> pure MinificationLinear
      9984    -> pure NearestMipmapNearest
      9985    -> pure LinearMipmapNearest
      9986    -> pure NearestMipmapLinear
      9987    -> pure LinearMipmapLinear
      invalid -> failWithContext "MinificationFilter" invalid

-- | S (U) / T (V) wrapping mode.
data WrappingMode
  = ClampToEdge
  | MirroredRepeat
  | Repeat
  deriving (Eq, Ord, Show)

instance FromJSON WrappingMode where
  parseJSON value = do
    number <- parseJSON value
    case number :: Int of
      33071   -> pure ClampToEdge
      33648   -> pure MirroredRepeat
      10497   -> pure Repeat
      invalid -> failWithContext "WrappingMode" invalid

-- | Texture sampler properties for filtering and wrapping modes.
data Sampler = Sampler
  { magFilter  :: MagnificationFilter
  , minFilter  :: MinificationFilter
  , wrapS      :: WrappingMode
  , wrapT      :: WrappingMode
  , name       :: Maybe T.Text
  , extensions :: Maybe Object
  , extras     :: Maybe Value
  }
  deriving (Eq, Ord, Show)

instance FromJSON Sampler where
  parseJSON = withObject "Sampler" $ \v ->
    Sampler
      <$> v .:? "magFilter" .!= MagnificationDefault
      <*> v .:? "minFilter" .!= MinificationDefault
      <*> v .:? "wrapS"     .!= Repeat
      <*> v .:? "wrapT"     .!= Repeat
      <*> v .:? "name"
      <*> v .:? "extensions"
      <*> v .:? "extras"

instance Index SamplerIx (V.Vector Sampler) Sampler where
  get i vec = vec V.! i.value
  {-# INLINE get #-}

defaultSampler :: Sampler
defaultSampler =
  Sampler
    { magFilter  = MagnificationDefault
    , minFilter  = MinificationDefault
    , wrapS      = Repeat
    , wrapT      = Repeat
    , name       = Nothing
    , extensions = Nothing
    , extras     = Nothing
    }