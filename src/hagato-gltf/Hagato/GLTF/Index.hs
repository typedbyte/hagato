{-# LANGUAGE FunctionalDependencies #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hagato.GLTF.Index
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Types and functions for handling indices pointing to objects in a glTF file.
-----------------------------------------------------------------------------
module Hagato.GLTF.Index where

-- aeson
import Data.Aeson (FromJSON)

-- base
import Foreign.Storable (Storable)

-- | Indicates that a value of type @m@ can be indexed using a value of type @i@
-- in order to obtain a value of type @a@.
class Index i m a | i m -> a where
  -- | Retrieves the value at index @i@ from @m@.
  get :: i -> m -> a

-- | Represents an index pointing to an accessor.
newtype AccessorIx = AccessorIx { value :: Int }
  deriving (Eq, Ord, Show, FromJSON, Storable)

-- | Represents an index pointing to an animation.
newtype AnimationIx = AnimationIx { value :: Int }
  deriving (Eq, Ord, Show, FromJSON, Storable)

-- | Represents an index pointing to an animation sampler.
newtype AnimationSamplerIx = AnimationSamplerIx { value :: Int }
  deriving (Eq, Ord, Show, FromJSON, Storable)

-- | Represents an index pointing to a buffer.
newtype BufferIx = BufferIx { value :: Int }
  deriving (Eq, Ord, Show, FromJSON, Storable)

-- | Represents an index pointing to a buffer view.
newtype BufferViewIx = BufferViewIx { value :: Int }
  deriving (Eq, Ord, Show, FromJSON, Storable)

-- | Represents an index pointing to a camera.
newtype CameraIx = CameraIx { value :: Int }
  deriving (Eq, Ord, Show, FromJSON, Storable)

-- | Represents an index pointing to an image.
newtype ImageIx = ImageIx { value :: Int }
  deriving (Eq, Ord, Show, FromJSON, Storable)

-- | Represents an index pointing to a material.
newtype MaterialIx = MaterialIx { value :: Int }
  deriving (Eq, Ord, Show, FromJSON, Storable)

-- | Represents an index pointing to a mesh.
newtype MeshIx = MeshIx { value :: Int }
  deriving (Eq, Ord, Show, FromJSON, Storable)

-- | Represents an index pointing to a node.
newtype NodeIx = NodeIx { value :: Int }
  deriving (Eq, Ord, Show, FromJSON, Storable)

-- | Represents an index pointing to a sampler.
newtype SamplerIx = SamplerIx { value :: Int }
  deriving (Eq, Ord, Show, FromJSON, Storable)

-- | Represents an index pointing to a scene.
newtype SceneIx = SceneIx { value :: Int }
  deriving (Eq, Ord, Show, FromJSON, Storable)

-- | Represents an index pointing to a skin.
newtype SkinIx = SkinIx { value :: Int }
  deriving (Eq, Ord, Show, FromJSON, Storable)

-- | Represents an index pointing to a texture.
newtype TextureIx = TextureIx { value :: Int }
  deriving (Eq, Ord, Show, FromJSON, Storable)