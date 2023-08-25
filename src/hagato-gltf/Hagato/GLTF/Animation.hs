{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hagato.GLTF.Animation
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Types and functions for handling animations found in glTF files.
-----------------------------------------------------------------------------
module Hagato.GLTF.Animation where

-- aeson
import Data.Aeson (FromJSON(..), Object, Value, (.:), (.:?), (.!=), withObject, withText)

-- text
import Data.Text qualified as T

-- vector
import Data.Vector qualified as V

import Hagato.GLTF.Aeson (failWithContext)
import Hagato.GLTF.Index (AccessorIx, AnimationIx(value), AnimationSamplerIx(value), Index(get), NodeIx)

-- | Represents an interpolation algorithm.
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

-- | An animation sampler combines timestamps with a sequence of output values
-- and defines an interpolation algorithm.
data AnimationSampler = AnimationSampler
  { input :: AccessorIx
    -- ^ The index of an accessor containing keyframe timestamps.
  , interpolation :: Interpolation
    -- ^ The interpolation algorithm.
  , output :: AccessorIx
    -- ^ The index of an accessor, containing keyframe output values.
  , extensions :: Maybe Object
    -- ^ A JSON object with extension-specific objects.
  , extras :: Maybe Value
    -- ^ Application-specific data.
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

-- | Represents the node's property to animate, or the weights of the morph
-- targets it instantiates.
data Path
  = Translation
    -- ^ Means that the values provided by the sampler are the translation along
    -- the X, Y, and Z axes.
  | Rotation
    -- ^ Means that the values are a quaternion in the order @(x, y, z, w)@, where
    -- @w@ is the scalar.
  | Scale
    -- ^ Means that the values are the scaling factors along the X, Y, and Z axes.
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

-- | Represents the descriptor of the animated property.
data AnimationTarget = AnimationTarget
  { node :: Maybe NodeIx
    -- ^ The index of the node to animate. When undefined, the animated object may
    -- be defined by an extension.
  , path :: Path
    -- ^ The node's property to animate.
  , extensions :: Maybe Object
    -- ^ A JSON object with extension-specific objects.
  , extras :: Maybe Value
    -- ^ Application-specific data.
  }
  deriving (Eq, Ord, Show)

instance FromJSON AnimationTarget where
  parseJSON = withObject "AnimationTarget" $ \v ->
    AnimationTarget
      <$> v .:? "node"
      <*> v .:  "path"
      <*> v .:? "extensions"
      <*> v .:? "extras"

-- | An animation channel combines an animation sampler with a target property
-- being animated.
data Channel = Channel
  { sampler :: AnimationSamplerIx
    -- ^ The index of a sampler in this animation used to compute the value for the target.
  , target :: AnimationTarget
    -- ^ The descriptor of the animated property.
  , extensions :: Maybe Object
    -- ^ A JSON object with extension-specific objects.
  , extras :: Maybe Value
    -- ^ Application-specific data.
  }
  deriving (Eq, Ord, Show)

instance FromJSON Channel where
  parseJSON = withObject "Channel" $ \v ->
    Channel
      <$> v .:  "sampler"
      <*> v .:  "target"
      <*> v .:? "extensions"
      <*> v .:? "extras"

-- | Represents a keyframe animation.
data Animation = Animation
  { channels :: V.Vector Channel
    -- ^ A vector of animation channels.
  , samplers :: V.Vector AnimationSampler
    -- ^ A vector of animation samplers.
  , name :: Maybe T.Text
    -- ^ The name of the animation.
  , extensions :: Maybe Object
    -- ^ A JSON object with extension-specific objects.
  , extras :: Maybe Value
    -- ^ Application-specific data.
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