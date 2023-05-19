{-# LANGUAGE RecordWildCards #-}
module Hagato.Core.Animation where

-- base
import Data.Fixed     (mod')
import Prelude hiding (repeat)

import Hagato.Core.Extra (clamp)

data Repeat
  = Times !Float
  | Forever

data Animation a = Animation
  { value       :: !a
  , duration    :: !Float
  , repeat      :: !Repeat
  , autoReverse :: !Bool
  , forward     :: !(Float -> Float -> a)
  , backward    :: !(Float -> Float -> a)
  , elapsed     :: !Float
  , done        :: !Bool
  }

instance Functor Animation where
  fmap f animation =
    animation
      { value    = f animation.value
      , forward  = \dt t -> f (animation.forward dt t)
      , backward = \dt t -> f (animation.backward dt t)
      }

at :: Float -> Animation a -> a
at t animation = animation.forward 0 real
  where real = clamp 0 1 t

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

once :: Float -> (Float -> Float -> a) -> Animation a
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

deltas :: Num a => (Float -> a) -> Float -> Float -> a
deltas = deltasVia (-)

deltasVia :: (a -> a -> b) -> (Float -> a) -> Float -> Float -> b
deltasVia diff f =
  \dt t ->
    let
      x0 = f t
      x1 = f (t+dt)
    in
      x1 `diff` x0

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

data Animated a = Animated
  { value      :: !a
  , animations :: ![Animation (a -> a)]
  , stopped    :: !Bool
  }

rigid :: a -> Animated a
rigid value =
  Animated
    { value      = value
    , animations = []
    , stopped    = True
    }

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

mapValue :: (a -> a) -> Animated a -> Animated a
mapValue f Animated{value, animations, stopped} =
  Animated
    { value      = f value
    , animations = animations
    , stopped    = stopped
    }
