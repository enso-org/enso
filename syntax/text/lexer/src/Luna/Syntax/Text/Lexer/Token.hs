{-# LANGUAGE Strict #-}

module Luna.Syntax.Text.Lexer.Token where

import Prologue hiding (Symbol, element, span)

import qualified Luna.Syntax.Text.Lexer.Symbol as Symbol

import Data.Text.Position             (Delta)
import Luna.Syntax.Text.Lexer.Grammar (EntryStack)
import Luna.Syntax.Text.Lexer.Symbol  (Symbol)


-------------------
-- === Token === --
-------------------

-- === Definition === --

data Token = Token
    { _span       :: !Delta
    , _offset     :: !Delta
    , _column     :: !Delta
    , _row        :: !Delta
    , _symbol     :: !Symbol
    , _entryStack :: !EntryStack
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''Token


-- === Instances === --

instance NFData Token
instance Symbol.HasSymbol Token where
    symbol = symbol
    {-# INLINE symbol #-}


-- === Special tokens === --

etx :: Token
etx = Token 0 0 0 0 Symbol.ETX mempty
{-# INLINE etx #-}
