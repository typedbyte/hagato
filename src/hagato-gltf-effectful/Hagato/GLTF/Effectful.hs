{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hagato.GLTF.Effectful
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- A glTF effect for the effectful ecosystem which allows to process glTF files.
-----------------------------------------------------------------------------
module Hagato.GLTF.Effectful
  ( -- * glTF Effect
    GLTF
  , runGLTF
    -- * glTF Operations
  , ask
  , fetch
  , getNode
  , loadAccessor
  , loadAccessorIx
  , loadBuffer
  , loadBufferIx
  , loadBufferView
  , loadBufferViewIx
  , loadImage
  , loadURI
  , fromMaybe
  , module Hagato.GLTF
  ) where

-- bytestring
import Data.ByteString qualified as BS

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

-- | The glTF effect.
data GLTF :: Effect

type instance DispatchOf GLTF = Static WithSideEffects

newtype instance StaticRep GLTF = GLTF (GLTF.Transmission, GLTF.Cache)

-- | Runs the glTF effect on a specific glTF transmission and extracts the final
-- value from it.
runGLTF :: IOE :> es => GLTF.Source -> Eff (GLTF : es) a -> Eff es a
runGLTF source m =
  GLTF.parseSource source >>= \case
    Left err ->
      GLTF.throw $ GLTF.InvalidFormat err
    Right tm ->
      evalStaticRep (GLTF (tm, GLTF.empty)) m

-- | Fetches the glTF transmission which is currently processed.
ask :: GLTF :> es => Eff es GLTF.Transmission
ask = do
  GLTF (tm, _) <- getStaticRep
  pure tm

-- | Gets an indexed value from the asset in the glTF transmission.
fetch :: (GLTF :> es, GLTF.Index i GLTF.Asset a) => i -> Eff es a
fetch i = do
  GLTF (tm, _) <- getStaticRep
  pure $ GLTF.fetchM i tm

-- | A utility function that extracts the value from a 'Maybe', or fails with a string.
fromMaybe :: Fail :> es => String -> Maybe a -> Eff es a
fromMaybe = GLTF.fromMaybeM

-- | Gets the node with the specified name from the glTF transmission.
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

-- | Loads the data that is associated with the specified accessor.
loadAccessor :: GLTF :> es => GLTF.Accessor -> Eff es BS.ByteString
loadAccessor = withStaticRep . GLTF.loadAccessorM

-- | Loads the data that is associated with the specified accessor index.
loadAccessorIx :: GLTF :> es => GLTF.AccessorIx -> Eff es BS.ByteString
loadAccessorIx = withStaticRep . GLTF.loadAccessorIxM

-- | Loads the data that is associated with the specified buffer.
loadBuffer :: GLTF :> es => GLTF.Buffer -> Eff es BS.ByteString
loadBuffer = withStaticRep . GLTF.loadBufferM

-- | Loads the data that is associated with the specified buffer index.
loadBufferIx :: GLTF :> es => GLTF.BufferIx -> Eff es BS.ByteString
loadBufferIx = withStaticRep . GLTF.loadBufferIxM

-- | Loads the data that is associated with the specified buffer view.
loadBufferView :: GLTF :> es => GLTF.BufferView -> Eff es BS.ByteString
loadBufferView = withStaticRep . GLTF.loadBufferViewM

-- | Loads the data that is associated with the specified buffer view index.
loadBufferViewIx :: GLTF :> es => GLTF.BufferViewIx -> Eff es BS.ByteString
loadBufferViewIx = withStaticRep . GLTF.loadBufferViewIxM

-- | Loads the data that is associated with the specified image.
loadImage :: GLTF :> es => GLTF.Image -> Eff es BS.ByteString
loadImage image =
  case image.imageData of
    GLTF.ImageBuffer bufferViewIx _mime -> loadBufferViewIx bufferViewIx
    GLTF.ImageURI uri _maybeMime        -> loadURI uri

-- | Loads the data that is associated with the specified URI.
loadURI :: GLTF :> es => GLTF.URI -> Eff es BS.ByteString
loadURI = withStaticRep . GLTF.loadURIM