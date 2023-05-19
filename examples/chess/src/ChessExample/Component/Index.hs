{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE TypeFamilies #-}
module ChessExample.Component.Index where

-- apecs-effectful
import Apecs.Effectful (Component, Entity, Storage, Unique)

-- base
import Data.Maybe (catMaybes, maybeToList)

-- chessica
import Chess (Command(..), Position, Update(command))

-- containers
import Data.Map.Strict qualified as M

-- The index component is a helper data structure so we can quickly find a world
-- entity which corresponds to a specific position of the chess board. "source"
-- and "targets" refer to the selected field and possible target fields during a
-- chess piece move, respectively, where each target is associated with the possible
-- chess game updates that can occur when commiting to a move.
data Index = Index
  { fields  :: !(M.Map Position Entity)
  , pieces  :: !(M.Map Position Entity)
  , source  :: !(Maybe Position)
  , targets :: !(M.Map Position [Update])
  }

instance Component Index where
  type Storage Index = Unique Index

-- The following functions are pure helper functions to query and manipulate the index.

emptyIndex :: Index
emptyIndex = Index M.empty M.empty Nothing M.empty

insertField :: Position -> Entity -> Index -> Index
insertField position entity index =
  index { fields = M.insert position entity index.fields }

lookupField :: Position -> Index -> Maybe Entity
lookupField position index =
  M.lookup position index.fields

insertPiece :: Position -> Entity -> Index -> Index
insertPiece position entity index =
  index { pieces = M.insert position entity index.pieces }

removePiece :: Position -> Index -> Index
removePiece position index =
  index { pieces = M.delete position index.pieces }

lookupPiece :: Position -> Index -> Maybe Entity
lookupPiece position index =
  M.lookup position index.pieces

lookupTargets :: Position -> Index -> Maybe [Update]
lookupTargets position index =
  M.lookup position index.targets

setFocus :: Maybe Position -> [Update] -> Index -> Index
setFocus position updates
  = setSource position
  . setTargets updates

setSource :: Maybe Position -> Index -> Index
setSource position index =
  index { source = position }

setTargets :: [Update] -> Index -> Index
setTargets updates index =
  index { targets = foldr f M.empty updates }
    where
      f up acc  = go acc up up.command
      go acc up = \case
        Move dst _ ->
          M.insertWith (++) dst [up] acc
        Sequence cmd1 cmd2 ->
          go (go acc up cmd1) up cmd2
        Atomic cmd ->
          go acc up cmd
        _ ->
          acc

focusedPositions :: Index -> [Position]
focusedPositions index =
  maybeToList index.source ++ M.keys index.targets

focusedFields :: Index -> [Entity]
focusedFields index =
  catMaybes $
    fmap
      ( \pos -> M.lookup pos index.fields )
      ( focusedPositions index )
