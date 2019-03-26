{-# LANGUAGE Strict       #-}
{-# LANGUAGE NoStrictData #-}

module Luna.Syntax.Text.Lexer.Token where

import Prologue hiding (Symbol, span)

import qualified Luna.Syntax.Text.Lexer.Symbol as Symbol

import Data.Text.Position            (Delta)
import Luna.Syntax.Text.Lexer.Symbol (Symbol)



-------------------
-- === Token === --
-------------------

-- === Definition === --

data Token a = Token
    { _span    :: !Delta
    , _offset  :: !Delta
    , _element :: !a
    } deriving (Eq, Generic, Ord)
makeLenses ''Token


-- === Instances === --

instance NFData a => NFData (Token a)
instance Show a => Show (Token a) where
    showsPrec d t = showParen' d
        $ showString "Token "
        . showsPrec' (t ^. span)
        . showString " "
        . showsPrec' (t ^. offset)
        . showString " "
        . showsPrec' (t ^. element)
    {-# INLINE showsPrec #-}

instance Symbol.HasSymbol a => Symbol.HasSymbol (Token a) where
    symbol = element . Symbol.symbol ; {-# INLINE symbol #-}


-- === Special tokens === --

etx :: Token Symbol
etx = Token 0 0 Symbol.ETX ; {-# INLINE etx #-}

