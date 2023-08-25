-----------------------------------------------------------------------------
-- |
-- Module      : Hagato.GLTF.Aeson
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Utility functions for parsing glTF using the Aeson package.
-----------------------------------------------------------------------------
module Hagato.GLTF.Aeson where

-- aeson
import Data.Aeson.Types (Parser)

-- text
import Data.Text      qualified as T
import Data.Text.Read qualified as T

-- | Constructs a parser that fails with an error text containing some context
-- and a showable value for easier debugging if the glTF specification is parsed
-- incorrectly. Library users should never need this.
failWithContext :: Show s => String -> s -> Parser a
failWithContext ctx s = 
  fail $ ctx ++ " " ++ show s ++ " is not according to the specification"

-- | Constructs a parser that parses an integer given by a text.
-- Library users should never need this.
readInt :: T.Text -> Parser Int
readInt text =
  case T.decimal text of
    Right (i,_) -> pure i
    Left err    -> fail err
