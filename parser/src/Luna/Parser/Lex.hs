module Luna.Parser.Lex where

import           Luna.Prelude            hiding (lex)
import           Text.Parser.Combinators
import           Text.Parser.Char


-- === Definitions === --

class Lexical a where
    lex :: CharParsing m => a -> m a


-- === Instances === --

instance Lexical Char   where lex = char                          ; {-# INLINE lex #-}
instance Lexical String where lex = string                        ; {-# INLINE lex #-}
instance Lexical Text   where lex = fromString <∘> lex ∘ toString ; {-# INLINE lex #-}
