{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}
module Hagato.Vulkan.Effectful.Window
  ( Window
  , runWindow
  , createWindow
  , destroyWindow
  , allocateWindow
  , manageWindow
  , getExtensions
  , createSurface
  , destroySurface
  , allocateSurface
  , manageSurface
  , resized
  , poll
  , tickPoll
  , Vk.WindowStrategy(..)
  , Vk.WindowWidth
  , Vk.WindowHeight
  , Vk.WindowTitle
  ) where

-- base
import Data.Kind (Type)

-- effectful-core
import Effectful                 (Dispatch(..), DispatchOf, Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Static (SideEffects(..), StaticRep, evalStaticRep,
                                  getStaticRep, unsafeEff_)

-- hagato
import Hagato.Core.Input (Input)

-- hagato:with-vulkan
import Hagato.Vulkan qualified as Vk

-- resource-effectful
import Effectful.Resource (Key, Resource, allocate)

data Window (w :: Type) :: Effect

type instance DispatchOf (Window w) = Static WithSideEffects
newtype instance StaticRep (Window w) = Window (Vk.WindowStrategy w)

runWindow :: IOE :> es => Vk.WindowStrategy w -> Eff (Window w : es) a -> Eff es a
runWindow = evalStaticRep . Window

createWindow :: Window w :> es => Vk.WindowWidth -> Vk.WindowHeight -> Vk.WindowTitle -> Eff es w
createWindow width height title = do
  Window strategy <- getStaticRep
  unsafeEff_ $ Vk.createWindow strategy width height title

destroyWindow :: Window w :> es => w -> Eff es ()
destroyWindow window = do
  Window strategy <- getStaticRep
  unsafeEff_ $ Vk.destroyWindow strategy window

allocateWindow :: (Window w :> es, Resource :> es) => Vk.WindowWidth -> Vk.WindowHeight -> Vk.WindowTitle -> Eff es (w, Key)
allocateWindow width height title = do
  Window strategy <- getStaticRep
  allocate
    ( Vk.createWindow strategy width height title )
    ( Vk.destroyWindow strategy )

manageWindow :: (Window w :> es, Resource :> es) => Vk.WindowWidth -> Vk.WindowHeight -> Vk.WindowTitle -> Eff es w
manageWindow width height title =
  fst <$> allocateWindow width height title

getExtensions :: Window w :> es => w -> Eff es [String]
getExtensions window = do
  Window strategy <- getStaticRep
  unsafeEff_ $ Vk.getExtensions strategy window

createSurface :: Window w :> es => w -> Vk.Instance -> Eff es Vk.SurfaceKHR
createSurface window vk = do
  Window strategy <- getStaticRep
  unsafeEff_ $ Vk.createSurface strategy window vk

destroySurface :: Window w :> es => w -> Vk.Instance -> Vk.SurfaceKHR -> Eff es ()
destroySurface window vk surface = do
  Window strategy <- getStaticRep
  unsafeEff_ $ Vk.destroySurface strategy window vk surface

allocateSurface :: (Window w :> es, Resource :> es) => w -> Vk.Instance -> Eff es (Vk.SurfaceKHR, Key)
allocateSurface window vk = do
  Window strategy <- getStaticRep
  allocate
    ( Vk.createSurface strategy window vk )
    ( Vk.destroySurface strategy window vk )

manageSurface :: (Window w :> es, Resource :> es) => w -> Vk.Instance -> Eff es Vk.SurfaceKHR
manageSurface window vk =
  fst <$> allocateSurface window vk

resized :: Window w :> es => w -> Eff es Bool
resized window = do
  Window strategy <- getStaticRep
  unsafeEff_ $ Vk.resized strategy window
{-# INLINE resized #-}

poll :: Window w :> es => w -> Eff es Input
poll window = do
  Window strategy <- getStaticRep
  unsafeEff_ $ Vk.poll strategy window
{-# INLINE poll #-}

tickPoll :: Window w :> es => Float -> w -> Eff es Input
tickPoll dt window = do
  Window strategy <- getStaticRep
  unsafeEff_ $ Vk.tickPoll strategy dt window
{-# INLINE tickPoll #-}