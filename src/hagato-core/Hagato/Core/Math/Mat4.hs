-----------------------------------------------------------------------------
-- |
-- Module      : Hagato.Core.Math.Mat4
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Types and functions for handling 4x4 matrices.
-----------------------------------------------------------------------------
module Hagato.Core.Math.Mat4
  ( -- * Matrix Construction
    Mat4(..)
  , rowMajor
  , columnMajor
  , fromTranslation
  , fromRotation
  , fromScale
  , fromTRS
    -- * Matrix Inspection
  , toTranslation
  , toRotation
  , toScale
  , toTRS
    -- * Matrix Update
  , setTranslation
    -- * Matrix Operations
  , multiply
  , multiplyVector
  , inverseTransform
    -- * (De-)Serialization
  , RowMajor(..)
  , ColumnMajor(..)
  ) where

-- base
import Foreign.Ptr      (castPtr)
import Foreign.Storable (Storable(..))

import Hagato.Core.Math.Quaternion (Quaternion(Quaternion))
import Hagato.Core.Math.Vec3       (Vec3(Vec3))
import Hagato.Core.Math.Vec4       (Vec4(Vec4))

-- | Represents a row-major 4x4 matrix.
data Mat4 =
  Mat4
    {-# UNPACK #-} !Float {-# UNPACK #-} !Float {-# UNPACK #-} !Float {-# UNPACK #-} !Float
    {-# UNPACK #-} !Float {-# UNPACK #-} !Float {-# UNPACK #-} !Float {-# UNPACK #-} !Float
    {-# UNPACK #-} !Float {-# UNPACK #-} !Float {-# UNPACK #-} !Float {-# UNPACK #-} !Float
    {-# UNPACK #-} !Float {-# UNPACK #-} !Float {-# UNPACK #-} !Float {-# UNPACK #-} !Float
  deriving
    (Eq, Ord, Read, Show)

-- | Constructs a 4x4 row-major matrix.
rowMajor
  :: Float -> Float -> Float -> Float
  -> Float -> Float -> Float -> Float
  -> Float -> Float -> Float -> Float
  -> Float -> Float -> Float -> Float
  -> Mat4
rowMajor = Mat4
{-# INLINE rowMajor #-}

-- | Constructs a 4x4 column-major matrix.
columnMajor
  :: Float -> Float -> Float -> Float
  -> Float -> Float -> Float -> Float
  -> Float -> Float -> Float -> Float
  -> Float -> Float -> Float -> Float
  -> Mat4
columnMajor
  m11 m21 m31 m41
  m12 m22 m32 m42
  m13 m23 m33 m43
  m14 m24 m34 m44
    =
      Mat4
        m11 m12 m13 m14
        m21 m22 m23 m24
        m31 m32 m33 m34
        m41 m42 m43 m44
{-# INLINE columnMajor #-}

-- | Multiplies two matrices.
multiply :: Mat4 -> Mat4 -> Mat4
multiply
  ( Mat4
      a11 a12 a13 a14
      a21 a22 a23 a24
      a31 a32 a33 a34
      a41 a42 a43 a44
  )
  ( Mat4
      b11 b12 b13 b14
      b21 b22 b23 b24
      b31 b32 b33 b34
      b41 b42 b43 b44
  )
  =
    let
      dot a b c d w y x z = a*w + b*y + c*x + d*z
      m11 = dot a11 a12 a13 a14 b11 b21 b31 b41
      m12 = dot a11 a12 a13 a14 b12 b22 b32 b42
      m13 = dot a11 a12 a13 a14 b13 b23 b33 b43
      m14 = dot a11 a12 a13 a14 b14 b24 b34 b44
      m21 = dot a21 a22 a23 a24 b11 b21 b31 b41
      m22 = dot a21 a22 a23 a24 b12 b22 b32 b42
      m23 = dot a21 a22 a23 a24 b13 b23 b33 b43
      m24 = dot a21 a22 a23 a24 b14 b24 b34 b44
      m31 = dot a31 a32 a33 a34 b11 b21 b31 b41
      m32 = dot a31 a32 a33 a34 b12 b22 b32 b42
      m33 = dot a31 a32 a33 a34 b13 b23 b33 b43
      m34 = dot a31 a32 a33 a34 b14 b24 b34 b44
      m41 = dot a41 a42 a43 a44 b11 b21 b31 b41
      m42 = dot a41 a42 a43 a44 b12 b22 b32 b42
      m43 = dot a41 a42 a43 a44 b13 b23 b33 b43
      m44 = dot a41 a42 a43 a44 b14 b24 b34 b44
    in
      Mat4
        m11 m12 m13 m14
        m21 m22 m23 m24
        m31 m32 m33 m34
        m41 m42 m43 m44
{-# INLINE multiply #-}

-- | Multiplies a matrix by a vector.
infixr 9 `multiplyVector`
multiplyVector :: Mat4 -> Vec4 -> Vec4
multiplyVector
  ( Mat4
      a11 a12 a13 a14
      a21 a22 a23 a24
      a31 a32 a33 a34
      a41 a42 a43 a44
  )
  ( Vec4 x y z w )
  =
    let
      dot x1 y1 z1 w1 x2 y2 z2 w2 = x1*x2 + y1*y2 + z1*z2 + w1*w2
      vx = dot a11 a12 a13 a14 x y z w
      vy = dot a21 a22 a23 a24 x y z w
      vz = dot a31 a32 a33 a34 x y z w
      vw = dot a41 a42 a43 a44 x y z w
    in
      Vec4 vx vy vz vw
{-# INLINE multiplyVector #-}

-- | Constructs a transformation matrix from a rotation.
fromRotation :: Quaternion -> Mat4
fromRotation (Quaternion w x y z) =
  rowMajor
    (1-2*(y2+z2))   (2*(xy-zw))   (2*(xz+yw)) 0
      (2*(xy+zw)) (1-2*(x2+z2))   (2*(yz-xw)) 0
      (2*(xz-yw))   (2*(yz+xw)) (1-2*(x2+y2)) 0
                0             0             0 1
  where
    x2 = x*x
    y2 = y*y
    z2 = z*z
    xy = x*y
    xz = x*z
    xw = x*w
    yz = y*z
    yw = y*w
    zw = z*w
{-# INLINE fromRotation #-}

-- | Constructs a transformation matrix from a @x\/y\/z@ scale.
fromScale :: Vec3 -> Mat4
fromScale (Vec3 sx sy sz) =
  rowMajor
    sx  0  0 0
     0 sy  0 0
     0  0 sz 0
     0  0  0 1
{-# INLINE fromScale #-}

-- | Constructs a transformation matrix from a @x\/y\/z@ translation.
fromTranslation :: Vec3 -> Mat4
fromTranslation (Vec3 tx ty tz) =
  rowMajor
    1 0 0 tx
    0 1 0 ty
    0 0 1 tz
    0 0 0  1
{-# INLINE fromTranslation #-}

-- | Constructs a transformation matrix from a @x\/y\/z@ translation, rotation and @x\/y\/z@ scale.
fromTRS :: Vec3 -> Quaternion -> Vec3 -> Mat4
fromTRS translation rotation scale =
  fromTranslation translation `multiply`
  fromRotation rotation `multiply`
  fromScale scale
{-# INLINE fromTRS #-}

-- | Reconstructs the rotation from a transformation matrix when knowing the @x\/y\/z@ scale.
withScaleToRotation :: Vec3 -> Mat4 -> Quaternion
withScaleToRotation
  ( Vec3 sx sy sz )
  ( Mat4
      x1 x2 x3 _
      y1 y2 y3 _
      z1 z2 z3 _
      _  _  _  _
  )
  =
    let
      r11 = x1 / sx
      r12 = x2 / sy
      r13 = x3 / sz
      r21 = y1 / sx
      r22 = y2 / sy
      r23 = y3 / sz
      r31 = z1 / sx
      r32 = z2 / sy
      r33 = z3 / sz
      
      q0 = sqrt $ max 0 ((r11 + r22 + r33 + 1) / 4)
      q1 = sqrt $ max 0 ((r11 - r22 - r33 + 1) / 4)
      q2 = sqrt $ max 0 ((-r11 + r22 - r33 + 1) / 4)
      q3 = sqrt $ max 0 ((-r11 - r22 + r33 + 1) / 4)
      
      sign x = if x >= 0 then 1 else -1
      
      (qr0, qr1, qr2, qr3) =
        if q0 >= q1 && q0 >= q2 && q0 >= q3 then
          ( q0
          , q1 * sign (r32 - r23)
          , q2 * sign (r13 - r31)
          , q3 * sign (r21 - r12)
          )
        else if q1 >= q0 && q1 >= q2 && q1 >= q3 then
          ( q0 * sign (r32 - r23)
          , q1
          , q2 * sign (r21 + r12)
          , q3 * sign (r13 + r31)
          )
        else if q2 >= q0 && q2 >= q1 && q2 >= q3 then
          ( q0 * sign (r13 - r31)
          , q1 * sign (r21 + r12)
          , q2
          , q3 * sign (r32 + r23)
          )
        else
          ( q0 * sign (r21 - r12)
          , q1 * sign (r31 + r13)
          , q2 * sign (r32 + r23)
          , q3
          )

      r = sqrt $ qr0*qr0 + qr1*qr1 + qr2*qr2 + qr3*qr3
    in
      Quaternion (qr0/r) (qr1/r) (qr2/r) (qr3/r)
{-# INLINE withScaleToRotation #-}

-- | Reconstructs the rotation from a transformation matrix.
toRotation :: Mat4 -> Quaternion
toRotation matrix =
  withScaleToRotation (toScale matrix) matrix
{-# INLINE toRotation #-}

-- | Reconstructs the @x\/y\/z@ scale from a transformation matrix.
toScale :: Mat4 -> Vec3
toScale
  ( Mat4
      x1 x2 x3 _
      y1 y2 y3 _
      z1 z2 z3 _
      _  _  _  _
  )
  =
    Vec3
      ( sqrt $ x1*x1 + y1*y1 + z1*z1 )
      ( sqrt $ x2*x2 + y2*y2 + z2*z2 )
      ( sqrt $ x3*x3 + y3*y3 + z3*z3 )
{-# INLINE toScale #-}

-- | Reconstructs the @x\/y\/z@ translation from a transformation matrix.
toTranslation :: Mat4 -> Vec3
toTranslation
  ( Mat4
      _ _ _ tx
      _ _ _ ty
      _ _ _ tz
      _ _ _ _
  )
  =
    Vec3 tx ty tz
{-# INLINE toTranslation #-}

-- | Reconstructs the @x\/y\/z@ translation, rotation and @x\/y\/z@ scale from a transformation matrix.
toTRS :: Mat4 -> (Vec3, Quaternion, Vec3)
toTRS matrix = (translation, rotation, scale)
  where
    translation = toTranslation matrix
    rotation    = withScaleToRotation scale matrix
    scale       = toScale matrix
{-# INLINE toTRS #-}

-- | Updates the translation component of a transformation matrix.
setTranslation :: Vec3 -> Mat4 -> Mat4
setTranslation
  ( Vec3 tx ty tz )
  ( Mat4
      x1 x2 x3  _
      y1 y2 y3  _
      z1 z2 z3  _
      a   b  c  d
  )
  =
    rowMajor
      x1 x2 x3 tx
      y1 y2 y3 ty
      z1 z2 z3 tz
      a   b  c  d
{-# INLINE setTranslation #-}

-- | Calculates the inverse matrix of a transformation matrix.
--
-- Note that this function cannot be used for calculating inverse matrices in general.
inverseTransform :: Mat4 -> Mat4
inverseTransform
  ( Mat4
      x1 y1 z1 t1
      x2 y2 z2 t2
      x3 y3 z3 t3
      _  _  _  _ 
  )
  =
    let
      squareLength x y z = x*x + y*y + z*z
      valueDot ax ay az bx by bz = ax*bx + ay*by + az*bz
      xLen2 = squareLength x1 x2 x3
      yLen2 = squareLength y1 y2 y3
      zLen2 = squareLength z1 z2 z3
      nt1 = -t1
      nt2 = -t2
      nt3 = -t3
      x1D = x1 / xLen2
      x2D = x2 / xLen2
      x3D = x3 / xLen2
      y1D = y1 / yLen2
      y2D = y2 / yLen2
      y3D = y3 / yLen2
      z1D = z1 / zLen2
      z2D = z2 / zLen2
      z3D = z3 / zLen2
    in
      rowMajor
        x1D x2D x3D (valueDot nt1 nt2 nt3 x1D x2D x3D)
        y1D y2D y3D (valueDot nt1 nt2 nt3 y1D y2D y3D)
        z1D z2D z3D (valueDot nt1 nt2 nt3 z1D z2D z3D)
          0   0   0                                  1

-- | Represents a row-major value. Can be used to pick the correct 'Storable' instance.
newtype RowMajor a = RowMajor a
  deriving
    (Eq, Ord, Read, Show)

instance Storable (RowMajor Mat4) where
  sizeOf _    = 64
  {-# INLINE sizeOf #-}
  
  alignment _ =  4
  {-# INLINE alignment #-}
  
  peek ptr =
    let
      floatPtr = castPtr ptr
    in do
      m11 <- peek        floatPtr
      m12 <- peekByteOff floatPtr  4
      m13 <- peekByteOff floatPtr  8
      m14 <- peekByteOff floatPtr 12
      m21 <- peekByteOff floatPtr 16
      m22 <- peekByteOff floatPtr 20
      m23 <- peekByteOff floatPtr 24
      m24 <- peekByteOff floatPtr 28
      m31 <- peekByteOff floatPtr 32
      m32 <- peekByteOff floatPtr 36
      m33 <- peekByteOff floatPtr 40
      m34 <- peekByteOff floatPtr 44
      m41 <- peekByteOff floatPtr 48
      m42 <- peekByteOff floatPtr 52
      m43 <- peekByteOff floatPtr 56
      m44 <- peekByteOff floatPtr 60
      pure $
        RowMajor $
          Mat4
            m11 m12 m13 m14
            m21 m22 m23 m24
            m31 m32 m33 m34
            m41 m42 m43 m44
  {-# INLINE peek #-}
  
  poke ptr
    ( RowMajor
        ( Mat4
            m11 m12 m13 m14
            m21 m22 m23 m24
            m31 m32 m33 m34
            m41 m42 m43 m44
        )
    )
    =
      let
        floatPtr = castPtr ptr
      in do
        poke        floatPtr    m11
        pokeByteOff floatPtr  4 m12
        pokeByteOff floatPtr  8 m13
        pokeByteOff floatPtr 12 m14
        pokeByteOff floatPtr 16 m21
        pokeByteOff floatPtr 20 m22
        pokeByteOff floatPtr 24 m23
        pokeByteOff floatPtr 28 m24
        pokeByteOff floatPtr 32 m31
        pokeByteOff floatPtr 36 m32
        pokeByteOff floatPtr 40 m33
        pokeByteOff floatPtr 44 m34
        pokeByteOff floatPtr 48 m41
        pokeByteOff floatPtr 52 m42
        pokeByteOff floatPtr 56 m43
        pokeByteOff floatPtr 60 m44
  {-# INLINE poke #-}

-- | Represents a column-major value. Can be used to pick the correct 'Storable' instance.
newtype ColumnMajor a = ColumnMajor a
  deriving
    (Eq, Ord, Read, Show)

instance Storable (ColumnMajor Mat4) where
  sizeOf _    = 64
  {-# INLINE sizeOf #-}
  
  alignment _ =  4
  {-# INLINE alignment #-}
  
  peek ptr =
    let
      floatPtr = castPtr ptr
    in do
      m11 <- peek        floatPtr
      m21 <- peekByteOff floatPtr  4
      m31 <- peekByteOff floatPtr  8
      m41 <- peekByteOff floatPtr 12
      m12 <- peekByteOff floatPtr 16
      m22 <- peekByteOff floatPtr 20
      m32 <- peekByteOff floatPtr 24
      m42 <- peekByteOff floatPtr 28
      m13 <- peekByteOff floatPtr 32
      m23 <- peekByteOff floatPtr 36
      m33 <- peekByteOff floatPtr 40
      m43 <- peekByteOff floatPtr 44
      m14 <- peekByteOff floatPtr 48
      m24 <- peekByteOff floatPtr 52
      m34 <- peekByteOff floatPtr 56
      m44 <- peekByteOff floatPtr 60
      pure $
        ColumnMajor $
          Mat4
            m11 m12 m13 m14
            m21 m22 m23 m24
            m31 m32 m33 m34
            m41 m42 m43 m44
  {-# INLINE peek #-}
  
  poke ptr
    ( ColumnMajor
        ( Mat4
            m11 m12 m13 m14
            m21 m22 m23 m24
            m31 m32 m33 m34
            m41 m42 m43 m44
        )
    )
    =
      let
        floatPtr = castPtr ptr
      in do
        poke        floatPtr    m11
        pokeByteOff floatPtr  4 m21
        pokeByteOff floatPtr  8 m31
        pokeByteOff floatPtr 12 m41
        pokeByteOff floatPtr 16 m12
        pokeByteOff floatPtr 20 m22
        pokeByteOff floatPtr 24 m32
        pokeByteOff floatPtr 28 m42
        pokeByteOff floatPtr 32 m13
        pokeByteOff floatPtr 36 m23
        pokeByteOff floatPtr 40 m33
        pokeByteOff floatPtr 44 m43
        pokeByteOff floatPtr 48 m14
        pokeByteOff floatPtr 52 m24
        pokeByteOff floatPtr 56 m34
        pokeByteOff floatPtr 60 m44
  {-# INLINE poke #-}
