{-# LANGUAGE FieldSelectors  #-}
{-# LANGUAGE TemplateHaskell #-}
module ChessExample.System.World where

-- apecs-effectful
import Apecs.Effectful

import ChessExample.Component.Animation   (FadeOut, Forward, Path)
import ChessExample.Component.Camera      (Camera)
import ChessExample.Component.Environment (Environment)
import ChessExample.Component.Focus       (Focus)
import ChessExample.Component.Index       (Index)
import ChessExample.Component.Mesh        (Mesh)
import ChessExample.Component.Screen      (Screen)
import ChessExample.Component.Transform   (Transform)

-- Type synonym that references all components. Used to delete entities, i.e. to
-- delete all components of an entity.
type All =
  (
    ( Camera
    , Environment
    , FadeOut
    , Focus
    , Forward
    , Index
    , Mesh
    , Path
    ),
    ( Screen
    , Transform
    )
  )

makeWorld "World"
  [ ''Camera
  , ''Environment
  , ''FadeOut
  , ''Focus
  , ''Forward
  , ''Index
  , ''Mesh
  , ''Path
  , ''Screen
  , ''Transform
  ]
