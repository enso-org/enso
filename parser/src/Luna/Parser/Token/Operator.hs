{-# LANGUAGE NoMonomorphismRestriction #-}

module Luna.Parser.Token.Operator where

import Prelude.Luna

import Luna.Parser.Combinators
import Text.Parser.Token           (highlight, reserve)
import Text.Parser.Char            (oneOf)

import qualified Text.Parser.Token.Highlight as Highlight
import qualified Luna.Parser.Token.Layout    as Layout


-- === Operators === --

opChars  = "!#$%&*+/<=>?\\^|-~"
opLetter = oneOf opChars
opStart  = opLetter

operator = fmap fromString $ opToken ((:) <$> opStart <*> many opLetter <?> "operator")



opToken p = Layout.block (highlight Highlight.Operator p)

