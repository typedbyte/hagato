module ChessExample.System.Referee where

-- apecs-effectful
import Apecs.Effectful (ECS, cmapM_, set)

-- base
import Control.Monad (forM_, when)
import Data.List     (find)

-- chessica
import Chess                         qualified as Chess
import Chess                         (Game, Some(Some), isOfType, piecesOf)
import Chess.Rulebook.Standard       (standardRulebook)
import Chess.Rulebook.Standard.Check (checked)

-- effectful-core
import Effectful (Eff, (:>))

import ChessExample.Component.Focus (Focus(Lost, Threatened))
import ChessExample.Component.Index (lookupPiece)
import ChessExample.System.World    (World)

-- The referee system judges the state of the chess game and sets the focus to
-- the world entities accordingly (e.g., declares a player the winner by marking
-- the opponent's king with the status "Lost").
judge :: ECS World :> es => Game -> Eff es ()
judge game =
  case standardRulebook.status game of
    Chess.Turn player ->
      when (checked player game.board) $
        cmapM_ $ setFocus Threatened player
    Chess.Draw ->
      cmapM_ $ \index -> do
        setFocus Threatened game.activePlayer index
        setFocus Threatened game.passivePlayer index
    Chess.Win player ->
      cmapM_ $ setFocus Lost (other player)
  where
    other player
      | player == game.activePlayer = game.passivePlayer
      | otherwise                   = game.activePlayer
    setFocus focus player index =
      forM_ (kingEntity index player) $ \e ->
        set e focus
    kingEntity index player = do
      Some king <-
        find
          ( \(Some p) -> isOfType Chess.King p.piece )
          ( piecesOf player.color game.board )
      lookupPiece king.position index
