{-# LANGUAGE Strict #-}

module Luna.Syntax.Text.Lexer.Token where

import Prologue hiding (Symbol, element, span)

import qualified Luna.Syntax.Text.Lexer.Symbol as Symbol

import Data.Text.Position            (Delta)
import Luna.Syntax.Text.Lexer.Symbol (Symbol)



-------------------
-- === Token === --
-------------------

-- === Definition === --

data TokenInfo = TokenInfo
    { __span     :: Delta
    , __offset   :: Delta
    , __column   :: Delta
    , __row      :: Delta
    , __topLevel :: Bool
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''TokenInfo

data Token = Token
    { __info   :: TokenInfo
    , __symbol :: Symbol
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''Token


-- === API === --

token :: Delta -> Delta -> Delta -> Delta -> Bool -> Symbol -> Token
token = \span offset column row topLevel
      -> Token (TokenInfo span offset column row topLevel)
{-# INLINE token #-}

etx :: Token
etx = Token mempty Symbol.ETX
{-# INLINE etx #-}


-- === IsToken === --

class IsToken t where
    info :: Lens' t TokenInfo

span     :: IsToken t => Lens' t Delta
offset   :: IsToken t => Lens' t Delta
column   :: IsToken t => Lens' t Delta
row      :: IsToken t => Lens' t Delta
topLevel :: IsToken t => Lens' t Bool
span     = info . tokenInfo_span
offset   = info . tokenInfo_offset
column   = info . tokenInfo_column
row      = info . tokenInfo_row
topLevel = info . tokenInfo_topLevel
{-# INLINE span     #-}
{-# INLINE offset   #-}
{-# INLINE column   #-}
{-# INLINE row      #-}
{-# INLINE topLevel #-}


-- === Instances === --

instance NFData TokenInfo
instance NFData Token

instance Symbol.HasSymbol Token where
    symbol = token_symbol
    {-# INLINE symbol #-}

instance Mempty TokenInfo where
    mempty = TokenInfo 0 0 0 0 True
    {-# INLINE mempty #-}

instance IsToken Token where
    info = token_info
    {-# INLINE info #-}

