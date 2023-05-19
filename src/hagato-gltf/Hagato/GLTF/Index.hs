{-# LANGUAGE FunctionalDependencies #-}
module Hagato.GLTF.Index where

-- aeson
import Data.Aeson (FromJSON)

-- base
import Foreign.Storable (Storable)

class Index i m a | i m -> a where
  get :: i -> m -> a

-- | An index pointing to an accessor.
newtype AccessorIx = AccessorIx { value :: Int }
  deriving (Eq, Ord, Show, FromJSON, Storable)

-- | An index pointing to an animation.
newtype AnimationIx = AnimationIx { value :: Int }
  deriving (Eq, Ord, Show, FromJSON, Storable)

-- | An index pointing to an animation sampler.
newtype AnimationSamplerIx = AnimationSamplerIx { value :: Int }
  deriving (Eq, Ord, Show, FromJSON, Storable)

-- | An index pointing to a buffer.
newtype BufferIx = BufferIx { value :: Int }
  deriving (Eq, Ord, Show, FromJSON, Storable)

-- | An index pointing to a buffer view.
newtype BufferViewIx = BufferViewIx { value :: Int }
  deriving (Eq, Ord, Show, FromJSON, Storable)

-- | An index pointing to a camera.
newtype CameraIx = CameraIx { value :: Int }
  deriving (Eq, Ord, Show, FromJSON, Storable)

-- | An index pointing to an image.
newtype ImageIx = ImageIx { value :: Int }
  deriving (Eq, Ord, Show, FromJSON, Storable)

-- | An index pointing to a material.
newtype MaterialIx = MaterialIx { value :: Int }
  deriving (Eq, Ord, Show, FromJSON, Storable)

-- | An index pointing to a mesh.
newtype MeshIx = MeshIx { value :: Int }
  deriving (Eq, Ord, Show, FromJSON, Storable)

-- | An index pointing to a node.
newtype NodeIx = NodeIx { value :: Int }
  deriving (Eq, Ord, Show, FromJSON, Storable)

-- | An index pointing to a sampler.
newtype SamplerIx = SamplerIx { value :: Int }
  deriving (Eq, Ord, Show, FromJSON, Storable)

-- | An index pointing to a scene.
newtype SceneIx = SceneIx { value :: Int }
  deriving (Eq, Ord, Show, FromJSON, Storable)

-- | An index pointing to a skin.
newtype SkinIx = SkinIx { value :: Int }
  deriving (Eq, Ord, Show, FromJSON, Storable)

-- | An index pointing to a texture.
newtype TextureIx = TextureIx { value :: Int }
  deriving (Eq, Ord, Show, FromJSON, Storable)