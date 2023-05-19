{-# LANGUAGE LambdaCase #-}
module ChessExample.System.Input where

-- apecs-effectful
import Apecs.Effectful (ECS)

-- base
import Control.Monad (foldM)

-- chessica
import Chess                   (Update(Update), lastUpdate, undo)
import Chess.Rulebook.Standard (standardRulebook)

-- hagato:with-core
import Hagato.Core.Input
import Hagato.Core.Math.Vec2 (y)

-- effectful-core
import Effectful (Eff, (:>))

import ChessExample.Component.Mesh   (MeshFactory)
import ChessExample.GameState        (GameState(game, done))
import ChessExample.System.Animator  qualified as Animator
import ChessExample.System.Director  qualified as Director
import ChessExample.System.Player    qualified as Player
import ChessExample.System.Referee   qualified as Referee
import ChessExample.System.World     (World)

-- The input system maps the input of the window (keyboard, mouse, etc.) to the
-- game state, thus creating a new game state. It does this by delegating the work
-- to other systems depending on the input.
process :: ECS World :> es => MeshFactory -> Input -> GameState -> Eff es GameState
process meshFactory input initState = do
  Director.moveCursor input.cursor
  foldM handle initState input.events
    where
      handle state = \case
        -- Escape -> Exit game loop.
        KeyEvent Key'Escape _ _ _ ->
          pure state { done = True }
        -- Backspace -> Take back last move.
        KeyEvent Key'Backspace _ Key'Pressed _ ->
          case state.game.lastUpdate of
            Nothing ->
              pure state
            Just (Update game command) -> do
              Player.play meshFactory (undo command)
              Referee.judge game
              pure state { game = game }
        -- Left click -> Play chess by selecting pieces and committing moves.
        MouseEvent cursor Mouse'Left Mouse'Pressed _  -> do
          position <- Director.target cursor
          case position of
            Nothing  -> pure state
            Just pos -> do
              updates <- Player.select standardRulebook state.game pos
              -- To keep it simple, we always play the first game update. If we
              -- have multiple possible updates (e.g., options for promoting a piece),
              -- we would need some user involvement for selecting one -> TODO.
              case updates of
                [] ->
                  pure state
                Update game command : _ -> do
                  Player.play meshFactory command
                  Referee.judge game
                  pure state { game = game }
        -- Right click -> Activate rotation mode.
        MouseEvent cursor Mouse'Right Mouse'Pressed _ ->
          Director.rotate (Just cursor) >> pure state
        -- Right click release -> Deactivate rotation mode.
        MouseEvent _ Mouse'Right Mouse'Released _ ->
          Director.rotate Nothing >> pure state
        -- Mouse/touchpad scroll -> Zoom.
        ScrollEvent _ vec ->
          Director.zoom vec.y >> pure state
        -- Window resize -> Mark the window state as dirty.
        ResizeEvent size ->
          Director.resize size >> pure state
        -- Window close -> Exit game loop.
        CloseEvent ->
          pure state { done = True }
        -- Tick (=time elapsed) -> Progress animations.
        TickEvent dt -> do
          Animator.animate dt >> pure state
        -- Otherwise -> nothing to do.
        _ ->
          pure state
