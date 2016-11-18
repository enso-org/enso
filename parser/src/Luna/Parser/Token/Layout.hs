{-# LANGUAGE NoMonomorphismRestriction #-}

module Luna.Parser.Token.Layout (module Luna.Parser.Token.Layout, module X) where

import Luna.Prelude hiding (noneOf, lex)

import qualified Luna.Parser.Indent as Indent

import Luna.Parser.Combinators
import Luna.Parser.Lex
import Text.Parser.Char        (char, string, oneOf, noneOf, anyChar, CharParsing)
import Data.List               (nub)
import Luna.Parser.Char        as X

----------------------
-- === Comments === --
----------------------

lineCom :: CharParsing m => m String
lineCom   = (\a b -> a : (b ++ "\n")) <$> lineComStart <*> manyTill anyChar (eol_ <|> eof)


mlineCom = (++) <$> try mlineComStart <*> mlineComBody
--mlineCom = State.registerComment <=< try mlineComStart *> mlineComBody

mlineComBody = try mlineComEnd
                 <|> ((++) <$> mlineCom                <*> mlineComBody)
                 <|> ((++) <$> many1 (noneOf startEnd) <*> mlineComBody)
                 <|> oneOf startEnd                     *> mlineComBody
                 <?> "end of comment block"
                 where startEnd = nub (mlineComEndLetter ++ mlineComStartLetter)


lineComStart :: CharParsing m => m Char
lineComStart        = lex '#'
mlineComStartLetter = "#{"
mlineComEndLetter   = "#}"
mlineComStart = string mlineComStartLetter
mlineComEnd   = string mlineComEndLetter




--------------------
-- === Layout === --
--------------------

--spaces     = concat <$> many tokBase <?> ""

--tokBase = many1 space <|> try mlineCom <|> lineCom <?> ""


--block p = p <* (try (spaces *> Indent.checkIndented) <|> pure ())



eol :: CharParsing m => m String
eol = seq '\n' '\r' <|> seq '\r' '\n' <?> "end of line" where
    seq s t = (:) <$> char s <*> ((return <$> char t) <|> pure [])

eol_ :: CharParsing m => m ()
eol_ = () <$ eol
