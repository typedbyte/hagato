{-# LANGUAGE ExplicitNamespaces #-}
module Effectful.Extra (type (<:)) where

-- effectful-core
import Effectful ((:>))

-- This is just a type synonym for nicely-aligned effects in type signatures.
-- With this, instead of ...
--
-- ABCDEF :> es
-- GH     :> es
-- IJK    :> es
-- ...
--
-- ... we can write ...
--
-- es <: ABCDEF
-- es <: GH
-- es <: IJK
-- ...
type es <: eff = eff :> es
