{-# LANGUAGE Strict #-}

module Luna.Syntax.Text.Lexer.Token where

import Data.Text.Position (Delta)
import Prologue           hiding (element, span)


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

