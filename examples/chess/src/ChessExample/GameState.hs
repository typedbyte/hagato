module ChessExample.GameState where

-- apecs-effectful
import Apecs.Effectful (ECS)

-- base
import Control.Monad (forM_)

-- chessica
import Chess                   (Game, spawnCommands)
import Chess.Rulebook          (newGame)
import Chess.Rulebook.Standard (standardRulebook)

-- effectful-core
import Effectful (Eff, (:>))

import ChessExample.Component.Mesh  (MeshFactory)
import ChessExample.System.Player   (play)
import ChessExample.System.World    (World)
import ChessExample.Vulkan.Renderer (Renderer)

-- The game state which will be updated in every game loop iteration, if necessary. 
data GameState = GameState
  { game     :: Game     -- The pure chess game state. No I/O-related stuff here.
  , renderer :: Renderer -- Holds Vulkan-related things to render the chess game state.
  , done     :: Bool     -- A flag indicating the exit of the game loop.
  }

-- A smart constructor for an initial game state which populates the world with
-- the initial chess pieces.
newGameState :: ECS World :> es => Renderer -> MeshFactory -> Eff es GameState
newGameState renderer meshFactory = do
  let game = standardRulebook.newGame
  forM_ (spawnCommands game) (play meshFactory)
  pure (GameState game renderer False)
