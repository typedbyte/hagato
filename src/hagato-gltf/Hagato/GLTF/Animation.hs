{-# LANGUAGE OverloadedStrings #-}
module Hagato.GLTF.Animation where

-- aeson
import Data.Aeson (FromJSON(..), Object, Value, (.:), (.:?), (.!=), withObject, withText)

-- text
import Data.Text qualified as T

-- vector
import Data.Vector qualified as V

import Hagato.GLTF.Aeson (failWithContext)
import Hagato.GLTF.Index (AccessorIx, AnimationIx(value), AnimationSamplerIx(value), Index(get), NodeIx)

-- | Interpolation algorithm.
data Interpolation
  = Linear
  | Step
  | CubicSpline
  deriving (Eq, Ord, Show)

instance FromJSON Interpolation where
  parseJSON = withText "Interpolation" $ \v ->
    case v of
      "LINEAR"      -> pure Linear
      "STEP"        -> pure Step
      "CUBICSPLINE" -> pure CubicSpline
      invalid       -> failWithContext "Interpolation" invalid

-- | An animation sampler combines timestamps with a sequence of output values and defines an interpolation algorithm.
data AnimationSampler = AnimationSampler
  { input         :: AccessorIx
  , interpolation :: Interpolation
  , output        :: AccessorIx
  , extensions    :: Maybe Object
  , extras        :: Maybe Value
  }
  deriving (Eq, Ord, Show)

instance FromJSON AnimationSampler where
  parseJSON = withObject "AnimationSampler" $ \v ->
    AnimationSampler
      <$> v .:  "input"
      <*> v .:? "interpolation" .!= Linear
      <*> v .:  "output"
      <*> v .:? "extensions"
      <*> v .:? "extras"

-- | The name of the node's TRS property to animate, or the weights of the Morph Targets it instantiates.
-- For the translation property, the values that are provided by the sampler are the translation along the X, Y, and Z axes.
-- For the rotation property, the values are a quaternion in the order (x, y, z, w), where w is the scalar.
-- For the scale property, the values are the scaling factors along the X, Y, and Z axes.
data Path
  = Translation
  | Rotation
  | Scale
  | MorphTargetWeights
  deriving (Eq, Ord, Show)

instance FromJSON Path where
  parseJSON = withText "Path" $ \v ->
    case v of
      "translation" -> pure Translation
      "rotation"    -> pure Rotation
      "scale"       -> pure Scale
      "weights"     -> pure MorphTargetWeights
      invalid       -> failWithContext "Path" invalid

-- | The descriptor of the animated property.
data AnimationTarget = AnimationTarget
  { node       :: Maybe NodeIx
  , path       :: Path
  , extensions :: Maybe Object
  , extras     :: Maybe Value
  }
  deriving (Eq, Ord, Show)

instance FromJSON AnimationTarget where
  parseJSON = withObject "AnimationTarget" $ \v ->
    AnimationTarget
      <$> v .:? "node"
      <*> v .:  "path"
      <*> v .:? "extensions"
      <*> v .:? "extras"

-- | An animation channel combines an animation sampler with a target property being animated.
data Channel = Channel
  { sampler    :: AnimationSamplerIx
  , target     :: AnimationTarget
  , extensions :: Maybe Object
  , extras     :: Maybe Value
  }
  deriving (Eq, Ord, Show)

instance FromJSON Channel where
  parseJSON = withObject "Channel" $ \v ->
    Channel
      <$> v .:  "sampler"
      <*> v .:  "target"
      <*> v .:? "extensions"
      <*> v .:? "extras"

-- | A keyframe animation.
data Animation = Animation
  { channels   :: V.Vector Channel
  , samplers   :: V.Vector AnimationSampler
  , name       :: Maybe T.Text
  , extensions :: Maybe Object
  , extras     :: Maybe Value
  }
  deriving (Eq, Ord, Show)

instance FromJSON Animation where
  parseJSON = withObject "Animation" $ \v ->
    Animation
      <$> v .:  "channels"
      <*> v .:  "samplers"
      <*> v .:? "name"
      <*> v .:? "extensions"
      <*> v .:? "extras"

instance Index AnimationSamplerIx Animation AnimationSampler where
  get i animation = animation.samplers V.! i.value
  {-# INLINE get #-}

instance Index AnimationIx (V.Vector Animation) Animation where
  get i vec = vec V.! i.value
  {-# INLINE get #-}