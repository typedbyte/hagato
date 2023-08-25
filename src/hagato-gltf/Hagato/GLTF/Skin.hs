{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hagato.GLTF.Skin
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Types and functions for handling skins found in glTF files.
-----------------------------------------------------------------------------
module Hagato.GLTF.Skin where

-- aeson
import Data.Aeson (FromJSON(..), Object, Value, (.:), (.:?), withObject)

-- text
import Data.Text qualified as T

-- vector
import Data.Vector qualified as V

import Hagato.GLTF.Index (AccessorIx, Index, NodeIx, SkinIx(value), get)

-- | Represents joints and matrices defining a skin.
data Skin = Skin
  { matrices :: Maybe AccessorIx
    -- ^ The index of the accessor containing the floating-point 4x4 inverse-bind matrices.
  , skeleton :: Maybe NodeIx
    -- ^ The index of the node used as a skeleton root.
  , joints :: V.Vector NodeIx
    -- ^ The indices of skeleton nodes, used as joints in this skin.
  , name :: Maybe T.Text
    -- ^ The name of the skin.
  , extensions :: Maybe Object
    -- ^ A JSON object with extension-specific objects.
  , extras :: Maybe Value
    -- ^ Application-specific data.
  }
  deriving (Eq, Ord, Show)

instance FromJSON Skin where
  parseJSON = withObject "Skin" $ \v ->
    Skin
      <$> v .:? "inverseBindMatrices"
      <*> v .:? "skeleton"
      <*> v .:  "joints"
      <*> v .:? "name"
      <*> v .:? "extensions"
      <*> v .:? "extras"

instance Index SkinIx (V.Vector Skin) Skin where
  get i vec = vec V.! i.value
  {-# INLINE get #-}