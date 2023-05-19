{-# LANGUAGE OverloadedStrings #-}
module Hagato.GLTF.Mesh where

-- aeson
import Data.Aeson (FromJSON(..), Object, Value, (.:), (.:?), (.!=), withObject)

-- containers
import Data.Map.Strict qualified as M

-- text
import Data.Text qualified as T

-- vector
import Data.Vector          qualified as V
import Data.Vector.Storable qualified as S

import Hagato.GLTF.Aeson     (failWithContext)
import Hagato.GLTF.Attribute (Attribute)
import Hagato.GLTF.Index     (AccessorIx, Index, MaterialIx, MeshIx(value), get)

-- | The topology type of primitives to render.
data PrimitiveMode
  = Points
  | Lines
  | LineLoop
  | LineStrip
  | Triangles
  | TriangleStrip
  | TriangleFan
  deriving (Eq, Ord, Show)

instance FromJSON PrimitiveMode where
  parseJSON value = do
    number <- parseJSON value
    case number :: Int of
      0 -> pure Points
      1 -> pure Lines
      2 -> pure LineLoop
      3 -> pure LineStrip
      4 -> pure Triangles
      5 -> pure TriangleStrip
      6 -> pure TriangleFan
      i -> failWithContext "PrimitiveMode" i

-- | Geometry to be rendered with the given material.
data Primitive = Primitive
  { attributes :: M.Map Attribute AccessorIx
  , indices    :: Maybe AccessorIx
  , material   :: Maybe MaterialIx
  , mode       :: PrimitiveMode
  , targets    :: V.Vector (M.Map Attribute AccessorIx)
  , extensions :: Maybe Object
  , extras     :: Maybe Value
  }
  deriving (Eq, Ord, Show)

instance FromJSON Primitive where
  parseJSON = withObject "Primitive" $ \v ->
    Primitive
      <$> v .:  "attributes"
      <*> v .:? "indices"
      <*> v .:? "material"
      <*> v .:? "mode"    .!= Triangles
      <*> v .:? "targets" .!= V.empty
      <*> v .:? "extensions"
      <*> v .:? "extras"

getAttribute :: Attribute -> Primitive -> Maybe AccessorIx
getAttribute attribute = M.lookup attribute . (.attributes)

-- | A set of primitives to be rendered. It's global transform is defined by a node that references it.
data Mesh = Mesh
  { primitives :: V.Vector Primitive
  , weights    :: S.Vector Float
  , name       :: Maybe T.Text
  , extensions :: Maybe Object
  , extras     :: Maybe Value
  }
  deriving (Eq, Ord, Show)

instance FromJSON Mesh where
  parseJSON = withObject "Mesh" $ \v ->
    Mesh
      <$> v .:  "primitives"
      <*> v .:? "weights" .!= S.empty
      <*> v .:? "name"
      <*> v .:? "extensions"
      <*> v .:? "extras"

instance Index MeshIx (V.Vector Mesh) Mesh where
  get i vec = vec V.! i.value
  {-# INLINE get #-}