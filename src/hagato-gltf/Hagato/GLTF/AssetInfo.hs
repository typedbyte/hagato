{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hagato.GLTF.AssetInfo
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Types and functions for handling metadata of glTF assets.
-----------------------------------------------------------------------------
module Hagato.GLTF.AssetInfo where

-- aeson
import Data.Aeson (FromJSON(..), Object, Value, (.:), (.:?), withObject, withText)

-- text
import Data.Text qualified as T

import Hagato.GLTF.Aeson (failWithContext, readInt)

-- | Represents a glTF version.
data Version = Version
  { major :: Int
  , minor :: Int
  }
  deriving (Eq, Ord, Show)

instance FromJSON Version where
  parseJSON = withText "Version" $ \v ->
    case T.split (== '.') v of
      [ma, mi] ->
        Version
          <$> readInt ma
          <*> readInt mi
      _ ->
        failWithContext "Version" v

-- | Represents metadata about a glTF asset.
data AssetInfo = AssetInfo
  { copyright :: Maybe T.Text
    -- ^ A copyright message suitable for display to credit the content creator.
  , generator :: Maybe T.Text
    -- ^ The tool that generated this glTF model.
  , version :: Version
    -- ^ The glTF version that this asset targets.
  , minVersion :: Maybe Version
    -- ^ The minimum glTF version that this asset targets.
  , extensions :: Maybe Object
    -- ^ A JSON object with extension-specific objects.
  , extras :: Maybe Value
    -- ^ Application-specific data.
  }
  deriving (Eq, Ord, Show)

instance FromJSON AssetInfo where
  parseJSON = withObject "AssetInfo" $ \v ->
    AssetInfo
      <$> v .:? "copyright"
      <*> v .:? "generator"
      <*> v .:  "version"
      <*> v .:? "minVersion"
      <*> v .:? "extensions"
      <*> v .:? "extras"
