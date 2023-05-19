{-# LANGUAGE TypeFamilies #-}
module ChessExample.Component.Screen where

-- apecs-effectful
import Apecs.Effectful (Component, Storage, Unique)

-- hagato:with-core
import Hagato.Core (Vec2)

-- The screen component holds window-related data. The dirty flag is used to
-- indicate a significant window change (e.g., resize) so we can react accordingly
-- (e.g., rebuild the renderer, that is, resize the framebuffers etc.).
data Screen = Screen
  { cursor   :: !(Maybe Vec2)
  , viewport :: !Vec2
  , dirty    :: !Bool
  }

instance Component Screen where
  type Storage Screen = Unique Screen
