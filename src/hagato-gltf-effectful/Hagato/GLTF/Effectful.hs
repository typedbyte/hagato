{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE TypeFamilies #-}
module Hagato.GLTF.Effectful
  ( GLTF
  , runGLTF
  , ask
  , fetch
  , fromMaybe
  , getNode
  , loadAccessor
  , loadAccessorIx
  , loadBuffer
  , loadBufferIx
  , loadBufferView
  , loadBufferViewIx
  , loadImage
  , loadURI
  , module Hagato.GLTF
  ) where

-- base
import Control.Monad.IO.Class (liftIO)

-- bytestring
import Data.ByteString      qualified as BS
import Data.ByteString.Lazy qualified as BL

-- effectful-core
import Effectful                 (Dispatch(..), DispatchOf, Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Static (SideEffects(..), StaticRep, evalStaticRep,
                                  getStaticRep, putStaticRep, unsafeEff_)
import Effectful.Fail            (Fail)

-- hagato:with-gltf
import Hagato.GLTF
import Hagato.GLTF qualified as GLTF

-- text
import Data.Text (Text)

data GLTF :: Effect

type instance DispatchOf GLTF = Static WithSideEffects

newtype instance StaticRep GLTF = GLTF (GLTF.Transmission, GLTF.Cache)

runGLTF :: IOE :> es => GLTF.Source -> Eff (GLTF : es) a -> Eff es a
runGLTF source m =
  case source of
    GLTF.FromBytes bytes ->
      go $ GLTF.parse Nothing (BS.fromStrict bytes)
    GLTF.FromFile file -> do
      bytes <- liftIO $ BL.readFile file
      go $ GLTF.parse (Just file) bytes
  where
    go = \case
      Left err ->
        GLTF.throw $ GLTF.InvalidFormat err
      Right tm ->
        evalStaticRep (GLTF (tm, GLTF.empty)) m

ask :: GLTF :> es => Eff es GLTF.Transmission
ask = do
  GLTF (tm, _) <- getStaticRep
  pure tm

fetch :: (GLTF :> es, GLTF.Index i GLTF.Asset a) => i -> Eff es a
fetch i = do
  GLTF (tm, _) <- getStaticRep
  pure $ GLTF.fetchM i tm

fromMaybe :: Fail :> es => String -> Maybe a -> Eff es a
fromMaybe = GLTF.fromMaybeM

getNode :: GLTF :> es => Text -> Eff es GLTF.Node
getNode name = do
  GLTF (tm, _) <- getStaticRep
  unsafeEff_ $ GLTF.getNodeM name tm

withStaticRep :: GLTF :> es => (GLTF.Transmission -> GLTF.Cache -> IO (a, GLTF.Cache)) -> Eff es a
withStaticRep f = do
  GLTF (tm, cache) <- getStaticRep
  (a, newCache)    <- unsafeEff_ $ f tm cache
  putStaticRep $ GLTF (tm, newCache)
  pure a

loadAccessor :: GLTF :> es => GLTF.Accessor -> Eff es BS.ByteString
loadAccessor = withStaticRep . GLTF.loadAccessorM

loadAccessorIx :: GLTF :> es => GLTF.AccessorIx -> Eff es BS.ByteString
loadAccessorIx = withStaticRep . GLTF.loadAccessorIxM

loadBuffer :: GLTF :> es => GLTF.Buffer -> Eff es BS.ByteString
loadBuffer = withStaticRep . GLTF.loadBufferM

loadBufferIx :: GLTF :> es => GLTF.BufferIx -> Eff es BS.ByteString
loadBufferIx = withStaticRep . GLTF.loadBufferIxM

loadBufferView :: GLTF :> es => GLTF.BufferView -> Eff es BS.ByteString
loadBufferView = withStaticRep . GLTF.loadBufferViewM

loadBufferViewIx :: GLTF :> es => GLTF.BufferViewIx -> Eff es BS.ByteString
loadBufferViewIx = withStaticRep . GLTF.loadBufferViewIxM

loadImage :: GLTF :> es => GLTF.Image -> Eff es BS.ByteString
loadImage image =
  case image.imageData of
    GLTF.ImageBuffer bufferViewIx _mime -> loadBufferViewIx bufferViewIx
    GLTF.ImageURI uri _maybeMime        -> loadURI uri

loadURI :: GLTF :> es => GLTF.URI -> Eff es BS.ByteString
loadURI = withStaticRep . GLTF.loadURIM