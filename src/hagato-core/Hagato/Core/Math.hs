-----------------------------------------------------------------------------
-- |
-- Module      : Hagato.Core.Math
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Re-export of @Hagato.Core.Math.*@ sub-modules.
-----------------------------------------------------------------------------
module Hagato.Core.Math
  ( module Hagato.Core.Math.Mat4
  , module Hagato.Core.Math.Plane
  , module Hagato.Core.Math.Quaternion
  , module Hagato.Core.Math.Ray
  , module Hagato.Core.Math.Vec2
  , module Hagato.Core.Math.Vec3
  , module Hagato.Core.Math.Vec4
  ) where

import Hagato.Core.Math.Mat4
import Hagato.Core.Math.Plane
import Hagato.Core.Math.Quaternion
import Hagato.Core.Math.Ray
import Hagato.Core.Math.Vec2 hiding (dot, length, normalize, scalarMultiply)
import Hagato.Core.Math.Vec3 hiding (dot, length, normalize, scalarMultiply)
import Hagato.Core.Math.Vec4 hiding (dot, normalize)