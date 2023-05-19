{-# LANGUAGE TypeFamilies #-}
module ChessExample.Component.Environment where

-- apecs-effectful
import Apecs.Effectful (Component, Storage, Unique)

-- vulkan
import Vulkan qualified as Vk

import ChessExample.Component.Mesh (Mesh)

-- Environment describes the visual environment of a 3D scene. In this example
-- it is used for the skybox surrounding the chess board.
data Environment = Environment
  { skyboxMesh :: !Mesh
  , skyboxSet  :: !Vk.DescriptorSet
  , textureSet :: !Vk.DescriptorSet
  }

instance Component Environment where
  type Storage Environment = Unique Environment
