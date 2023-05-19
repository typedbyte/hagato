{-# LANGUAGE OverloadedStrings #-}
module Hagato.GLTF.Attribute where

-- aeson
import Data.Aeson (FromJSONKey(..), FromJSONKeyFunction(..))

-- text
import Data.Text qualified as T

import Hagato.GLTF.Aeson (failWithContext, readInt)

-- | Represents the different types of vertex attributes.
data Attribute
  = Position
  | Normal
  | Tangent
  | Texcoord Int
  | Color Int
  | Joints Int
  | Weights Int
  | Custom T.Text
  deriving (Eq, Ord, Show)

instance FromJSONKey Attribute where
  fromJSONKey =
    FromJSONKeyTextParser $ \v ->
      case v of
        "POSITION" -> pure Position
        "NORMAL"   -> pure Normal
        "TANGENT"  -> pure Tangent
        t ->
          case T.takeWhile (/= '_') t of
            "TEXCOORD" -> Texcoord <$> readInt (T.drop 9 t)
            "COLOR"    -> Color    <$> readInt (T.drop 6 t)
            "JOINTS"   -> Joints   <$> readInt (T.drop 7 t)
            "WEIGHTS"  -> Weights  <$> readInt (T.drop 8 t)
            ""         -> pure $ Custom (T.drop 1 t)
            _          -> failWithContext "Attribute" t
            
  fromJSONKeyList =
    FromJSONKeyTextParser $
      failWithContext "Attribute"
