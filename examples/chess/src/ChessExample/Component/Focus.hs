{-# LANGUAGE TypeFamilies #-}
module ChessExample.Component.Focus where

-- apecs-effectful
import Apecs.Effectful (Component, Map, Storage)

-- Indicates if an entity is of special interest, for example when
-- a) a chess piece is selected (Selected)
-- b) a king is threatened/checked (Threatened)
-- c) a player has lost (Lost)
-- Systems can make use of this information, e.g. the artist system can render
-- the king with red color if it is marked with the "Lost" focus.
data Focus
  = Selected
  | Threatened
  | Lost

instance Component Focus where
  type Storage Focus = Map Focus
