{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Luna.Parser.Char where

import           Prelude.Luna            hiding (lex)
import           Text.Parser.Combinators
import           Text.Parser.Char        hiding (spaces)


----------------------------------------------------------------------
-- Type classes
----------------------------------------------------------------------

class Lexical a where
    lex :: CharParsing m => a -> m a


----------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------

eolSeq :: CharParsing m => m String
eolSeq =   try (lex "\n\r")
       <|> try (lex "\r\n")
       <|> lex "\n"
       <|> lex "\r"
       <?> "end of line sequence"

eol = eolSeq *> return () <?> "end of line"


----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

instance Lexical Char where
    lex = char

instance Lexical String where
    lex = string

instance Lexical Text where
    lex s = fromString <$> lex (toString s)
