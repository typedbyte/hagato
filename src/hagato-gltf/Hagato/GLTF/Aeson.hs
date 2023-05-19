module Hagato.GLTF.Aeson where

-- aeson
import Data.Aeson.Types (Parser)

-- text
import Data.Text      qualified as T
import Data.Text.Read qualified as T

failWithContext :: Show s => String -> s -> Parser a
failWithContext ctx s = 
  fail $ ctx ++ " " ++ show s ++ " is not according to the specification"

readInt :: T.Text -> Parser Int
readInt text =
  case T.decimal text of
    Right (i,_) -> pure i
    Left err    -> fail err
