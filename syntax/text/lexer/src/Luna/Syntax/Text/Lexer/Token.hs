{-# LANGUAGE Strict #-}

module Luna.Syntax.Text.Lexer.Token where

import Prologue_old hiding (span, element)
import Data.Text.Position (Delta)


-------------------
-- === Token === --
-------------------

-- === Definition === --

data Token a = Token
    { _span    :: !Delta
    , _offset  :: !Delta
    , _element :: !a
    } deriving (Eq, Generic)
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

-- FIXME: Required by Megaparsec in luna-parser, it sould not be.
deriving instance Ord a => Ord (Token a)
