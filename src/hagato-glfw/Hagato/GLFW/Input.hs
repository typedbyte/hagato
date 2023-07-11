{-# LANGUAGE LambdaCase #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hagato.GLFW.Input
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Functions for mapping GLFW-specific input to generic hagato-based input.
-----------------------------------------------------------------------------
module Hagato.GLFW.Input where

-- GLFW-b
import Graphics.UI.GLFW qualified as GLFW

-- hagato:with-core
import Hagato.Core qualified as Hagato

-- | Maps a GLFW-based key to a hagato-based key.
mapKey :: GLFW.Key -> Hagato.Key
mapKey = \case
  GLFW.Key'Unknown      -> Hagato.Key'Unknown
  GLFW.Key'Space        -> Hagato.Key'Space
  GLFW.Key'Apostrophe   -> Hagato.Key'Apostrophe
  GLFW.Key'Comma        -> Hagato.Key'Comma
  GLFW.Key'Minus        -> Hagato.Key'Minus
  GLFW.Key'Period       -> Hagato.Key'Period
  GLFW.Key'Slash        -> Hagato.Key'Slash
  GLFW.Key'0            -> Hagato.Key'0
  GLFW.Key'1            -> Hagato.Key'1
  GLFW.Key'2            -> Hagato.Key'2
  GLFW.Key'3            -> Hagato.Key'3
  GLFW.Key'4            -> Hagato.Key'4
  GLFW.Key'5            -> Hagato.Key'5
  GLFW.Key'6            -> Hagato.Key'6
  GLFW.Key'7            -> Hagato.Key'7
  GLFW.Key'8            -> Hagato.Key'8
  GLFW.Key'9            -> Hagato.Key'9
  GLFW.Key'Semicolon    -> Hagato.Key'Semicolon
  GLFW.Key'Equal        -> Hagato.Key'Equal
  GLFW.Key'A            -> Hagato.Key'A
  GLFW.Key'B            -> Hagato.Key'B
  GLFW.Key'C            -> Hagato.Key'C
  GLFW.Key'D            -> Hagato.Key'D
  GLFW.Key'E            -> Hagato.Key'E
  GLFW.Key'F            -> Hagato.Key'F
  GLFW.Key'G            -> Hagato.Key'G
  GLFW.Key'H            -> Hagato.Key'H
  GLFW.Key'I            -> Hagato.Key'I
  GLFW.Key'J            -> Hagato.Key'J
  GLFW.Key'K            -> Hagato.Key'K
  GLFW.Key'L            -> Hagato.Key'L
  GLFW.Key'M            -> Hagato.Key'M
  GLFW.Key'N            -> Hagato.Key'N
  GLFW.Key'O            -> Hagato.Key'O
  GLFW.Key'P            -> Hagato.Key'P
  GLFW.Key'Q            -> Hagato.Key'Q
  GLFW.Key'R            -> Hagato.Key'R
  GLFW.Key'S            -> Hagato.Key'S
  GLFW.Key'T            -> Hagato.Key'T
  GLFW.Key'U            -> Hagato.Key'U
  GLFW.Key'V            -> Hagato.Key'V
  GLFW.Key'W            -> Hagato.Key'W
  GLFW.Key'X            -> Hagato.Key'X
  GLFW.Key'Y            -> Hagato.Key'Y
  GLFW.Key'Z            -> Hagato.Key'Z
  GLFW.Key'LeftBracket  -> Hagato.Key'LeftBracket
  GLFW.Key'Backslash    -> Hagato.Key'Backslash
  GLFW.Key'RightBracket -> Hagato.Key'RightBracket
  GLFW.Key'GraveAccent  -> Hagato.Key'GraveAccent
  GLFW.Key'World1       -> Hagato.Key'World1
  GLFW.Key'World2       -> Hagato.Key'World2
  GLFW.Key'Escape       -> Hagato.Key'Escape
  GLFW.Key'Enter        -> Hagato.Key'Enter
  GLFW.Key'Tab          -> Hagato.Key'Tab
  GLFW.Key'Backspace    -> Hagato.Key'Backspace
  GLFW.Key'Insert       -> Hagato.Key'Insert
  GLFW.Key'Delete       -> Hagato.Key'Delete
  GLFW.Key'Right        -> Hagato.Key'Right
  GLFW.Key'Left         -> Hagato.Key'Left
  GLFW.Key'Down         -> Hagato.Key'Down
  GLFW.Key'Up           -> Hagato.Key'Up
  GLFW.Key'PageUp       -> Hagato.Key'PageUp
  GLFW.Key'PageDown     -> Hagato.Key'PageDown
  GLFW.Key'Home         -> Hagato.Key'Home
  GLFW.Key'End          -> Hagato.Key'End
  GLFW.Key'CapsLock     -> Hagato.Key'CapsLock
  GLFW.Key'ScrollLock   -> Hagato.Key'ScrollLock
  GLFW.Key'NumLock      -> Hagato.Key'NumLock
  GLFW.Key'PrintScreen  -> Hagato.Key'PrintScreen
  GLFW.Key'Pause        -> Hagato.Key'Pause
  GLFW.Key'F1           -> Hagato.Key'F1
  GLFW.Key'F2           -> Hagato.Key'F2
  GLFW.Key'F3           -> Hagato.Key'F3
  GLFW.Key'F4           -> Hagato.Key'F4
  GLFW.Key'F5           -> Hagato.Key'F5
  GLFW.Key'F6           -> Hagato.Key'F6
  GLFW.Key'F7           -> Hagato.Key'F7
  GLFW.Key'F8           -> Hagato.Key'F8
  GLFW.Key'F9           -> Hagato.Key'F9
  GLFW.Key'F10          -> Hagato.Key'F10
  GLFW.Key'F11          -> Hagato.Key'F11
  GLFW.Key'F12          -> Hagato.Key'F12
  GLFW.Key'F13          -> Hagato.Key'F13
  GLFW.Key'F14          -> Hagato.Key'F14
  GLFW.Key'F15          -> Hagato.Key'F15
  GLFW.Key'F16          -> Hagato.Key'F16
  GLFW.Key'F17          -> Hagato.Key'F17
  GLFW.Key'F18          -> Hagato.Key'F18
  GLFW.Key'F19          -> Hagato.Key'F19
  GLFW.Key'F20          -> Hagato.Key'F20
  GLFW.Key'F21          -> Hagato.Key'F21
  GLFW.Key'F22          -> Hagato.Key'F22
  GLFW.Key'F23          -> Hagato.Key'F23
  GLFW.Key'F24          -> Hagato.Key'F24
  GLFW.Key'F25          -> Hagato.Key'F25
  GLFW.Key'Pad0         -> Hagato.Key'Pad0
  GLFW.Key'Pad1         -> Hagato.Key'Pad1
  GLFW.Key'Pad2         -> Hagato.Key'Pad2
  GLFW.Key'Pad3         -> Hagato.Key'Pad3
  GLFW.Key'Pad4         -> Hagato.Key'Pad4
  GLFW.Key'Pad5         -> Hagato.Key'Pad5
  GLFW.Key'Pad6         -> Hagato.Key'Pad6
  GLFW.Key'Pad7         -> Hagato.Key'Pad7
  GLFW.Key'Pad8         -> Hagato.Key'Pad8
  GLFW.Key'Pad9         -> Hagato.Key'Pad9
  GLFW.Key'PadDecimal   -> Hagato.Key'PadDecimal
  GLFW.Key'PadDivide    -> Hagato.Key'PadDivide
  GLFW.Key'PadMultiply  -> Hagato.Key'PadMultiply
  GLFW.Key'PadSubtract  -> Hagato.Key'PadSubtract
  GLFW.Key'PadAdd       -> Hagato.Key'PadAdd
  GLFW.Key'PadEnter     -> Hagato.Key'PadEnter
  GLFW.Key'PadEqual     -> Hagato.Key'PadEqual
  GLFW.Key'LeftShift    -> Hagato.Key'LeftShift
  GLFW.Key'LeftControl  -> Hagato.Key'LeftControl
  GLFW.Key'LeftAlt      -> Hagato.Key'LeftAlt
  GLFW.Key'LeftSuper    -> Hagato.Key'LeftSuper
  GLFW.Key'RightShift   -> Hagato.Key'RightShift
  GLFW.Key'RightControl -> Hagato.Key'RightControl
  GLFW.Key'RightAlt     -> Hagato.Key'RightAlt
  GLFW.Key'RightSuper   -> Hagato.Key'RightSuper
  GLFW.Key'Menu         -> Hagato.Key'Menu

-- | Maps a GLFW-based key state to a hagato-based key state.
mapKeyState :: GLFW.KeyState -> Hagato.KeyState
mapKeyState = \case
  GLFW.KeyState'Pressed   -> Hagato.Key'Pressed
  GLFW.KeyState'Released  -> Hagato.Key'Released
  GLFW.KeyState'Repeating -> Hagato.Key'Repeating

-- | Maps GLFW-based modifier keys to hagato-based modifier keys.
mapModifiers :: GLFW.ModifierKeys -> Hagato.Modifiers
mapModifiers = \case
  GLFW.ModifierKeys shift ctrl alt super caps num ->
    Hagato.Modifiers shift ctrl alt super caps num

-- | Maps a GLFW-based mouse button to a hagato-based mouse button.
mapMouseButton :: GLFW.MouseButton -> Hagato.MouseButton
mapMouseButton = \case
  GLFW.MouseButton'1 -> Hagato.Mouse'Left
  GLFW.MouseButton'2 -> Hagato.Mouse'Right
  GLFW.MouseButton'3 -> Hagato.Mouse'Middle
  GLFW.MouseButton'4 -> Hagato.Mouse'Extra1
  GLFW.MouseButton'5 -> Hagato.Mouse'Extra2
  GLFW.MouseButton'6 -> Hagato.Mouse'Extra3
  GLFW.MouseButton'7 -> Hagato.Mouse'Extra4
  GLFW.MouseButton'8 -> Hagato.Mouse'Extra5

-- | Maps a GLFW-based mouse button state to a hagato-based mouse button state.
mapMouseButtonState :: GLFW.MouseButtonState -> Hagato.MouseButtonState
mapMouseButtonState = \case
  GLFW.MouseButtonState'Pressed  -> Hagato.Mouse'Pressed
  GLFW.MouseButtonState'Released -> Hagato.Mouse'Released
