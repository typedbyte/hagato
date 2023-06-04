{-# LANGUAGE RecordWildCards #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hagato.Core.Animation
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Animating values of arbitrary types.
-----------------------------------------------------------------------------
module Hagato.Core.Animation
  ( -- * Animations
    Animation(..)
  , Repeat(..)
  , constant
  , once
  , deltasVia
  , deltas
  , step
    -- * Animated Values
  , Animated(..)
  , rigid
  , animate
  , update
  , mapValue
  ) where

-- base
import Data.Fixed     (mod')
import Prelude hiding (repeat)

import Hagato.Core.Extra (clamp)

-- | Represents the repeat behaviour of an animation.
data Repeat
  = Times !Float
    -- ^ Indicates that an animation is repeated a specific amount of times. Note
    -- that 'Float' values are possible, which means that an animation can repeat,
    -- for example, @2.5@ times.
  | Forever
    -- ^ Indicates that an animation is repeated endlessly.

-- | Represents the animation of a value of type @a@.
data Animation a = Animation
  { value :: !a
    -- ^ The current value of the animation.
  , duration :: !Float
    -- ^ The duration of the animation in seconds.
  , repeat :: !Repeat
    -- ^ The repeat behaviour of the animation.
  , autoReverse :: !Bool
    -- ^ If 'True', the animation runs backwards after completing one iteration, then
    -- forward again the next iteration, and so on.
  , forward :: !(Float -> Float -> a)
    -- ^ The function @f(dt,t)@ producing an updated value @a@ when animating forward.
  , backward :: !(Float -> Float -> a)
    -- ^ The function @f(dt,t)@ producing an updated value @a@ when animating backwards.
  , elapsed :: !Float
    -- ^ The elapsed time in seconds since starting the animation.
  , done :: !Bool
    -- ^ If 'True', the animation has finished.
  }

instance Functor Animation where
  fmap f animation =
    animation
      { value    = f animation.value
      , forward  = \dt t -> f (animation.forward dt t)
      , backward = \dt t -> f (animation.backward dt t)
      }

-- | Samples an animation at a given time @t@, where @t@ ranges from @0@ (start)
-- to @1@ (end of first iteration, see 'Repeat').
at :: Float -> Animation a -> a
at t animation = animation.forward 0 real
  where real = clamp 0 1 t

-- | Creates a constant animation which always yields the provided value.
constant :: a -> Animation a
constant value =
  Animation
    { value       = value
    , duration    = 0
    , repeat      = Times 0
    , autoReverse = False
    , forward     = \_dt _t -> value
    , backward    = \_dt _t -> value
    , elapsed     = 0
    , done        = True
    }

-- | Creates an animation which runs exactly once.
once
  :: Float
  -- ^ The duration of the animation in seconds.
  -> (Float -> Float -> a)
  -- ^ The function @f(dt,t)@ describing the animation values. @t@ ranges from @0@ to @1@.
  -> Animation a
  -- ^ The resulting animation, which can be progressed and sampled.
once duration forward =
  Animation
    { value       = forward 0 0
    , duration    = max 0 duration
    , repeat      = Times 1
    , autoReverse = False
    , forward     = forward
    , backward    = \dt t -> forward dt (1-t)
    , elapsed     = 0
    , done        = False
    }

-- | Turns a function @a = f(t)@ into a function @b = f(dt,t)@ by numeric difference.
--
-- @
-- 'deltas' = 'deltasVia' (-)
-- @
deltas
  :: Num a
  => (Float -> a)
  -- ^ @a = f(t)@
  -> (Float -> Float -> a)
  -- ^ @a = f(dt,t)@
deltas = deltasVia (-)

-- | Turns a function @a = f(t)@ into a function @b = f(dt,t)@ by providing a
-- function that calculates @b@ via the difference @f(t+dt) - f(t)@.
deltasVia
  :: (a -> a -> b)
  -- ^ The difference function.
  -> (Float -> a)
  -- ^ @a = f(t)@
  -> (Float -> Float -> b)
  -- ^ @b = f(dt,t)@
deltasVia diff f =
  \dt t ->
    let
      x0 = f t
      x1 = f (t+dt)
    in
      x1 `diff` x0

-- | Progresses an animation with an elapsed time @dt@.
step :: Float -> Animation a -> Animation a
step dt animation@Animation{..}
  | done =
      animation
  | progress >= deadline =
      animation
        { value = forward delta deadline
        , done  = True
        }
  | elapsed < duration =
      animation
        { value   = forward delta progress
        , elapsed = leap
        }
  | autoReverse =
      animation
        { value    = backward delta restart
        , forward  = backward
        , backward = forward
        , repeat   = newRepeat
        , elapsed  = restart
        }
  | otherwise =
      animation
        { value   = forward delta restart
        , repeat  = newRepeat
        , elapsed = restart
        }
  where
    restart  = elapsed `mod'` duration
    progress = elapsed / duration
    leap     = elapsed + dt
    delta    = leap / duration - progress
    (deadline, newRepeat) =
      case repeat of
        Times i -> (i, Times (i-1))
        Forever -> (1, Forever)

-- | Represents a value of type @a@ which is currently affected by multiple
-- animations. Animations are applied one after another, potential conflicts
-- between them are not checked.
data Animated a = Animated
  { value :: !a
    -- ^ The current value of the animation.
  , animations :: ![Animation (a -> a)]
    -- ^ The animations that affect the value @a@.
  , stopped :: !Bool
    -- ^ If 'True', all animations are 'done'.
  }

-- | Creates a constant value, i.e. a value which is not affected by animations.
rigid :: a -> Animated a
rigid value =
  Animated
    { value      = value
    , animations = []
    , stopped    = True
    }

-- | Smart constuctor for an animated value.
animate :: a -> [Animation (a -> a)] -> Animated a
animate start animations =
  Animated
    { value      = initValue
    , animations = animations
    , stopped    = all (.done) animations
    }
  where
    initAnimations = fmap (at 0) animations
    initValue      = foldr ($) start initAnimations

-- | Updates an animated value by progressing its animations with an elapsed time @dt@.
update :: Float -> Animated a -> Animated a
update dt animated@Animated{..}
  | stopped =
      animated
  | otherwise =
      Animated
        { value      = updatedValue
        , animations = updatedAnimations
        , stopped    = all (.done) updatedAnimations
        }
      where
        updatedAnimations = fmap (step dt) animations
        updatedValue      = foldr (($) . (.value)) value updatedAnimations

-- | Updates a value which is currently affected by animations.
mapValue :: (a -> a) -> Animated a -> Animated a
mapValue f Animated{value, animations, stopped} =
  Animated
    { value      = f value
    , animations = animations
    , stopped    = stopped
    }
