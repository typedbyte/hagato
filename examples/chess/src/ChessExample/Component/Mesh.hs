{-# LANGUAGE TypeFamilies #-}
module ChessExample.Component.Mesh where

-- apecs-effectful
import Apecs.Effectful (Component, Map, Storage)

-- base
import Data.Word       (Word32)
import Foreign.C.Types (CInt)

-- chessica
import Chess (Piece, Some)

-- containers
import Data.Map.Strict qualified as M

-- vector
import Data.Vector qualified as V

-- vulkan
import Vulkan qualified as Vk

-- The mesh factory is used to lookup the 3D mesh for a specific chess piece.
type MeshFactory = M.Map (Some Piece) Mesh

-- A mesh consists of multiple primitives (standalone 3D parts with separate
-- vertices, materials, etc.). In the chess example, we always have one primitive
-- per mesh, but in general we cannot guarantee this.
newtype Mesh = Mesh { primitives :: V.Vector Primitive }

instance Component Mesh where
  type Storage Mesh = Map Mesh

-- A primitive is a 3D object with a geometry (vertices/normals) and a material
-- (here: texture). Material can be absent for simple geometries which only describe
-- the vertices (used in this example for the skybox cube, where the cubemap
-- is applied separately).
data Primitive = Primitive
  { geometry :: !Geometry
  , material :: !(Maybe Material)
  }

-- A geometry describes the vertices and normals of a 3D object, possibly using an
-- additional index buffer.
data Geometry = Geometry
  { vertexBuffer :: !Vk.Buffer
  , vertexCount  :: !Word32
  , indexInfo    :: !(Maybe (Vk.Buffer, Word32, Vk.IndexType))
  , normalBuffer :: !Vk.Buffer
  }

-- In this example, a material is just a texture and its corresonding coordinates.
-- Note that we refer to a texture using an index which points into a big buffer
-- holding all the textures for the current scene.
data Material = Material
  { texIndex       :: !CInt
  , texCoordBuffer :: !Vk.Buffer
  }
