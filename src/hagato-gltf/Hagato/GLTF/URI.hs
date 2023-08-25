{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hagato.GLTF.URI
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Types and functions for handling URIs found in glTF files.
-----------------------------------------------------------------------------
module Hagato.GLTF.URI where

-- aeson
import Data.Aeson (FromJSON)

-- base
import Control.Monad.IO.Class (MonadIO, liftIO)

-- base64
import Data.ByteString.Base64 (decodeBase64)

-- bytestring
import Data.ByteString qualified as BS

-- filepath
import System.FilePath ((</>))

-- text
import Data.Text          qualified as T
import Data.Text.Encoding qualified as T

-- | Represents a URI found in glTF files.
newtype URI = URI T.Text
  deriving (Eq, Ord, Show, FromJSON)

-- | Loads the resource of the specified URI. The provided file path is the root
-- path from where relative paths are resolved.
load :: (MonadFail m, MonadIO m) => Maybe FilePath -> URI -> m BS.ByteString
load ctx (URI uri) =
  if T.isPrefixOf "data:" uri then
    case T.breakOn ";base64," uri of
      (_, "") ->
        fail $ "URI " ++ T.unpack uri ++ " is invalid."
      (_, payload) ->
        case decodeBase64 $ T.encodeUtf8 (T.drop 8 payload) of
          Right bs -> pure bs
          Left err -> fail $ T.unpack uri ++ ": " ++ T.unpack err
  else
    liftIO $ BS.readFile $
      case ctx of
        Just fp -> fp </> T.unpack uri
        Nothing -> T.unpack uri
