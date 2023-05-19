{-# LANGUAGE OverloadedStrings #-}
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

newtype URI = URI { unURI :: T.Text }
  deriving (Eq, Ord, Show, FromJSON)

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
