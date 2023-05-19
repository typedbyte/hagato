{-# LANGUAGE OverloadedStrings #-}
module Hagato.GLTF.AssetInfo where

-- aeson
import Data.Aeson (FromJSON(..), Object, Value, (.:), (.:?), withObject, withText)

-- text
import Data.Text qualified as T

import Hagato.GLTF.Aeson (failWithContext, readInt)

-- | The glTF version.
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

-- | Metadata about the glTF asset.
data AssetInfo = AssetInfo
  { copyright  :: Maybe T.Text
  , generator  :: Maybe T.Text
  , version    :: Version
  , minVersion :: Maybe Version
  , extensions :: Maybe Object
  , extras     :: Maybe Value
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
