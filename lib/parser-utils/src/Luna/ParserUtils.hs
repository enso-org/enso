{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

module Luna.ParserUtils where

import Prologue

import qualified Text.Read as Read

import Text.Megaparsec      (Parsec, ParseError)
import Text.Megaparsec.Char (space, string, char, digitChar)



------------------------------
-- === Parser Utilities === --
------------------------------

-- === Definition === --

type Parser = Parsec Void Text
type Error  = ParseError Char Void


-- === API === ---

spaces :: Parser ()
spaces = void space

and :: Parser ()
and = spaces *> string "&&" *> spaces

dot :: Parser Char
dot = char '.'

natural :: Parser Word64
natural = Read.read <$> some digitChar

