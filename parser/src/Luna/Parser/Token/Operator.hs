{-# LANGUAGE NoMonomorphismRestriction #-}

module Luna.Parser.Token.Operator where

import Prelude.Luna

import Luna.Parser.Combinators
import Text.Parser.Token           (highlight, reserve, token)
import Text.Parser.Char            (oneOf)

import qualified Text.Parser.Token.Highlight as Highlight
import qualified Luna.Parser.Token.Layout    as Layout

-- === Operators === --

opChars  = "!#$%&*+/<=>?\\^|-~"
opLetter = oneOf opChars
opStart  = opLetter


opName = (:) <$> opStart <*> many opLetter <?> "operator name"
operator = token (highlight Highlight.Operator opName) <?> "operator"


