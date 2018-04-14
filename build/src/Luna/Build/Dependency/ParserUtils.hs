module Luna.Build.Dependency.ParserUtils where

import Prologue

import qualified Text.Read as Read

import Text.Megaparsec.Text (Parser)
import Text.Megaparsec      (space, string, char, digitChar)

------------------------------
-- === Parser Utilities === --
------------------------------

spaces :: Parser ()
spaces = void space

and :: Parser ()
and = spaces *> string "&&" *> spaces

dot :: Parser Char
dot = char '.'

natural :: Parser Word64
natural = Read.read <$> some digitChar

