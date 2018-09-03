{-# LANGUAGE NoStrict             #-}
{-# LANGUAGE NoStrictData         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Parser.Instances.Attoparsec where

import Prelude hiding ((.))

import           Control.Monad (void)
import           Control.Monad.Identity
import qualified Data.Attoparsec.Combinator     as Atto
import           Data.Attoparsec.Internal.Types (Parser, IResult, Chunk)
import qualified Data.Attoparsec.Text           as SText
import qualified Data.Attoparsec.Text32         as T32
import           Data.Functor.Utils
import           Data.Parser
import qualified Data.Text                      as Strict
import           Data.Text32                    (Text32)


-- === Universal === --

type instance Tokens        (Parser t) = t
type instance BaseMonad     (Parser t) = Identity
type instance PartialResult (Parser t) = IResult t
type instance Result        (Parser t) = IResult t

instance BacktrackParser (Parser a) where
    try = id ; {-# INLINE try #-}

instance Chunk a => FiniteParser (Parser a) where
    endOfInput = Atto.endOfInput ; {-# INLINE endOfInput #-}


-- === Strict.Text === --

type instance Token  (Parser Strict.Text) = Char

instance TokenParser (Parser Strict.Text) where
    satisfy    = SText.satisfy       ; {-# INLINE satisfy    #-}
    takeWhile  = SText.takeWhile     ; {-# INLINE takeWhile  #-}
    takeWhile1 = SText.takeWhile1    ; {-# INLINE takeWhile1 #-}
    anyToken   = SText.anyChar       ; {-# INLINE anyToken   #-}
    token_     = void . SText.char   ; {-# INLINE token_     #-}
    tokens_    = void . SText.string ; {-# INLINE tokens_    #-}
    peekToken  = SText.peekChar'     ; {-# INLINE peekToken  #-}
    peekToken' = SText.peekChar      ; {-# INLINE peekToken' #-}

instance PartialParser (Parser Strict.Text) where
    parsePartialT = pure .: SText.parse      ; {-# INLINE parsePartialT #-}
    feedPartialT  = pure .: Atto.feed        ; {-# INLINE feedPartialT  #-}
    closePartialT = flip feedPartialT mempty ; {-# INLINE closePartialT #-}


-- === Text32 === --

type instance Token (Parser Text32) = Char

instance TokenParser (Parser Text32) where
    satisfy    = T32.satisfy       ; {-# INLINE satisfy    #-}
    takeWhile  = T32.takeWhile     ; {-# INLINE takeWhile  #-}
    takeWhile1 = T32.takeWhile1    ; {-# INLINE takeWhile1 #-}
    anyToken   = T32.anyChar       ; {-# INLINE anyToken   #-}
    token_     = void . T32.char   ; {-# INLINE token_     #-}
    tokens_    = void . T32.string ; {-# INLINE tokens_    #-}
    peekToken  = T32.peekChar'     ; {-# INLINE peekToken  #-}
    peekToken' = T32.peekChar      ; {-# INLINE peekToken' #-}

instance PartialParser (Parser Text32) where
    parsePartialT = pure .: T32.parse        ; {-# INLINE parsePartialT #-}
    feedPartialT  = pure .: Atto.feed        ; {-# INLINE feedPartialT  #-}
    closePartialT = flip feedPartialT mempty ; {-# INLINE closePartialT #-}
