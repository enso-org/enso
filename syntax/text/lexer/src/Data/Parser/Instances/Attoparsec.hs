module Data.Parser.Instances.Attoparsec where

import Prelude
import Data.Parser
import qualified Data.Attoparsec.Text as SText
import Data.Attoparsec.Internal.Types (Parser)
import qualified Data.Text as Strict
import Control.Monad (void)


-- === Strict.Text === --

type instance Token  (Parser Strict.Text) = Char
type instance Tokens (Parser Strict.Text) = Strict.Text

instance TokenParser (Parser Strict.Text) where
    satisfy    = SText.satisfy       ; {-# INLINE satisfy    #-}
    takeWhile  = SText.takeWhile     ; {-# INLINE takeWhile  #-}
    takeWhile1 = SText.takeWhile1    ; {-# INLINE takeWhile1 #-}
    anyToken   = SText.anyChar       ; {-# INLINE anyToken   #-}
    token_     = void . SText.char   ; {-# INLINE token_     #-}
    tokens_    = void . SText.string ; {-# INLINE tokens_    #-}
    peekToken  = SText.peekChar'     ; {-# INLINE peekToken  #-}
    peekToken' = SText.peekChar      ; {-# INLINE peekToken' #-}

instance BacktrackParser (Parser a) where
    try = id ; {-# INLINE try #-}
