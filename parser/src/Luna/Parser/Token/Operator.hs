{-# LANGUAGE NoMonomorphismRestriction #-}

module Luna.Parser.Token.Operator where

import Prelude.Luna

import Luna.Parser.Combinators
import Text.Parser.Token           (highlight, reserve, token)
import Text.Parser.Char            (oneOf)

import qualified Text.Parser.Token.Highlight as Highlight
import qualified Luna.Parser.Token.Layout    as Layout

-- === Operators === --

charList  = "!#$%&*+/<=>?\\^|-~"
char      = oneOf charList
startChar = char


name  = (:) <$> startChar <*> many char <?> "operator name"
ident = token (highlight Highlight.Operator name) <?> "operator"


