-----------------------------------------------------------------------------
-- |
-- Module      : Hagato.Vulkan.Camera
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Types and functions for handling Vulkan-compatible cameras.
-----------------------------------------------------------------------------
module Hagato.Vulkan.Camera
  ( projectionMatrix
  , inverseProjectionMatrix
  , inverseFromProjection
  , ray
  , rayFromInverse
  ) where

-- hagato:with-core
import Hagato.Core

-- | Computes a Vulkan-compatible projection matrix from a given camera.
projectionMatrix :: Camera -> Mat4
projectionMatrix Camera{frustum} =
  case frustum of
    Perspective near maybeFar fov ratio ->
      case maybeFar of
        Just far -> perspective fov near far ratio
        Nothing  -> infinitePerspective fov near ratio
    Orthographic near far fov ratio ->
      let
        width      = ratio * fov
        halfWidth  = width / 2
        halfHeight = fov / 2
        left       = -halfWidth
        right      =  halfWidth
        top        =  halfHeight
        bottom     = -halfHeight
      in
        orthographic left right top bottom near far

perspective :: Float -> Float -> Float -> Float -> Mat4
perspective fov n f aspectRatio = 
  let
    focalLength = 1 / tan (0.5 * fov)
  in
    rowMajor
      (focalLength / aspectRatio)             0             0                 0
                               0  (-focalLength)            0                 0
                               0              0  (n / (f - n)) (n * f / (f - n))
                               0              0           (-1)                0

infinitePerspective :: Float -> Float -> Float -> Mat4
infinitePerspective fov n aspectRatio = 
  let
    focalLength = 1 / tan (0.5 * fov)
  in
    rowMajor
      (focalLength / aspectRatio)             0    0  0
                               0  (-focalLength)   0  0
                               0              0    0  n
                               0              0  (-1) 0

orthographic :: Float -> Float -> Float -> Float -> Float -> Float -> Mat4
orthographic left right top bottom near far =
  let
    rml = right - left
    bmt = bottom - top
    fmn = far - near
  in
    rowMajor
      (2 / rml)       0         0  (-(right + left) / rml)
             0 (2 / bmt)        0  (-(bottom + top) / bmt)
             0        0  (1 / fmn)             (far / fmn)
             0        0         0                       1

-- | Computes a Vulkan-compatible inverse projection matrix from a given camera.
--
-- If you have already obtained the projection matrix of the camera (see 'projectionMatrix'),
-- use 'inverseFromProjection' instead for better performance.
inverseProjectionMatrix :: Camera -> Mat4
inverseProjectionMatrix camera =
  inverseFromProjection
    ( camera )
    ( projectionMatrix camera )

-- | Computes a Vulkan-compatible inverse projection matrix from a given camera and
-- its projection matrix.
inverseFromProjection :: Camera -> Mat4 -> Mat4
inverseFromProjection camera
  ( Mat4
      x _ _ tx
      _ y _ ty
      _ _ z tz
      _ _ _ _
  )
  =
    case camera.frustum of
      Perspective _ _ _ _ ->
        rowMajor
          (1/x)    0      0      0
             0  (1/y)     0      0
             0     0      0    (-1)
             0     0  (1/tz) (z/tz)
      Orthographic _ _ _ _ ->
        rowMajor
          (1/x)    0     0  (-tx/x)
             0  (1/y)    0  (-ty/y)
             0     0  (1/z) (-tz/z)
             0     0     0       1

-- | Computes a ray going from a given camera through a specified viewport point.
-- Note that it is not guaranteed that the origin of the ray is at the camera position.
--
-- If you have already obtained the inverse view matrix and the inverse projection matrix
-- of the camera (see 'inverseViewMatrix' and 'inverseProjectionMatrix'), use 'rayFromInverse'
-- instead for better performance.
ray
  :: Vec2   -- ^ The point within the viewport (x/y from top left).
  -> Vec2   -- ^ The viewport size (width/height).
  -> Camera -- ^ The camera from which the ray is cast to the point.
  -> Ray    -- ^ The computed ray.
ray point viewport camera =
  rayFromInverse
    ( point )
    ( viewport )
    ( inverseViewMatrix camera )
    ( inverseProjectionMatrix camera )

-- | Computes a ray going from a given camera through a specified viewport point.
-- Note that it is not guaranteed that the origin of the ray is at the camera position.
rayFromInverse
  :: Vec2 -- ^ The point within the viewport (x/y from top left).
  -> Vec2 -- ^ The viewport size (width/height).
  -> Mat4 -- ^ The inverse view matrix of the camera.
  -> Mat4 -- ^ The inverse projection matrix of the camera.
  -> Ray  -- ^ The computed ray.
rayFromInverse (Vec2 x y) (Vec2 width height) invView invProj =
  Ray origin direction
  where
    xNDC        = 2 * x / width - 1
    yNDC        = 2 * y / height - 1
    originNDC   = Vec4 xNDC yNDC 0.25 1
    targetNDC   = Vec4 xNDC yNDC 0.75 1
    originWorld = invView `multiplyVector` invProj `multiplyVector` originNDC
    targetWorld = invView `multiplyVector` invProj `multiplyVector` targetNDC
    origin      = perspectiveDivide originWorld
    target      = perspectiveDivide targetWorld
    direction   = target - origin