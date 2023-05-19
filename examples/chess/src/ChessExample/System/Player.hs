{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE OverloadedLists #-}
module ChessExample.System.Player (play, select) where

-- apecs-effectful
import Apecs.Effectful (ECS, Not(Not), cmap, cmapM_, get, global, newEntity, set, tryGet)

-- base
import Control.Monad  (forM_)
import Prelude hiding (lookup)

-- chessica
import Chess

-- containers
import Data.Map.Strict qualified as M

-- effectful-core
import Effectful (Eff, (:>))

-- hagato:with-core
import Hagato.Core.Animation       (once)
import Hagato.Core.Animation.Vec3  (linear, quadraticBezier)
import Hagato.Core.Math.Quaternion (Quaternion(Quaternion))
import Hagato.Core.Math.Vec3       (Vec3(..), between)

import ChessExample.Component.Animation (FadeOut(FadeOut), Path(Path))
import ChessExample.Component.Focus     (Focus(Selected))
import ChessExample.Component.Index
import ChessExample.Component.Mesh      (MeshFactory)
import ChessExample.Component.Transform (Transform(Transform, translation))
import ChessExample.System.World        (World)

-- The player system represents the actions of a chess player. It executes chess
-- game commands and moves (i.e. animates) the chess pieces accordingly.

-- Plays a specific chess command by moving (=animating) the pieces.
play :: ECS World :> es => MeshFactory -> Command -> Eff es ()
play meshFactory command = do
  unfocus
  go command
    where
      go = \case
        EndTurn ->
          pure ()
        Move dst (Some piece) -> do
          index <- get global
          forM_ (lookupPiece piece.position index) $ \e -> do
            cmap $ removePiece piece.position
            cmap $ insertPiece dst e
            transform <- tryGet @_ @Transform e
            forM_ transform $ \t -> do
              let
                start  = t.translation
                end    = to3D dst
                middle = (between start end) { z = 3 }
                path   = once 0.5 $ const $
                  if piece.type' `equals` Knight
                  then quadraticBezier start middle end
                  else linear start end
              set e (Path path)
        Destroy (Some piece) -> do
          index <- get global
          forM_ (lookupPiece piece.position index) $ \e -> do
            cmap $ removePiece piece.position
            transform <- tryGet @_ @Transform e
            forM_ transform $ \t ->
              let
                path = once 1.0 $ const $ linear t.translation (Vec3 0 0 15)
                fade = once 1.0 $ \_ _ -> ()
              in
                set e (Path path, FadeOut fade)
        Spawn pos piece ->
          forM_ (M.lookup piece meshFactory) $ \mesh ->
            let
              start     = Vec3 0 0 15
              transform = Transform start (Quaternion 1 0 0 0) 1
              path      = once 1.0 $ const $ linear start (to3D pos)
            in do
              e <- newEntity (Path path, mesh, transform)
              cmap $ insertPiece pos e
        Promote piece@(Some old) new -> do
          go [Destroy piece, Spawn old.position new]
        Sequence cmd1 cmd2 -> do
          go cmd1
          go cmd2
        Atomic cmd ->
          go cmd
        where
          to3D position =
            Vec3
              ( fromIntegral position.column - 3.5 )
              ( fromIntegral position.row - 3.5 )
              ( 0 )

-- Represents an action where the chess player interacts with a position on the chess
-- board (e.g., to select a chess piece or confirm a move of the selected chess piece).
-- The list of selected game state updates is returned, which may be empty.
select :: ECS World :> es => Rulebook -> Game -> Position -> Eff es [Update]
select rulebook game position = do
  index <- get global
  case lookupTargets position index of
    -- confirmation
    Just targets ->
      pure targets
    Nothing ->
      case lookup position game.board of
        -- (re-)selection
        Just (Some piece)
          |  piece.color == game.activePlayer.color
          && Just position /= index.source -> do
            reselect $ setFocus (Just position) (rulebook.updates position game)
            pure []
        -- deselection
        _ -> do
          reselect $ setFocus Nothing []
          pure []

unfocus :: ECS World :> es => Eff es ()
unfocus = do
  cmap $ setFocus Nothing []         -- reset index
  cmap $ \(_ :: Focus) -> Not @Focus -- reset fields/pieces

reselect :: ECS World :> es => (Index -> Index) -> Eff es ()
reselect f = do
  -- de-select (=unfocus) all currently selected fields
  cmap $ \case
    Selected -> Left  (Not @Focus)
    _        -> Right ()
  -- apply the new selections to the index
  cmap f
  -- select (=focus) the fields according to the new index
  cmapM_ $ \index ->
    forM_ (focusedFields index) $ \e ->
      set e Selected
