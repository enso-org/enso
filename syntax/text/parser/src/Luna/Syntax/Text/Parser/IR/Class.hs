{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoStrict                  #-}
{-# LANGUAGE NoStrictData              #-}
{-# LANGUAGE UndecidableInstances      #-}

module Luna.Syntax.Text.Parser.IR.Class where

import Prologue

import qualified Luna.Syntax.Text.Lexer               as Lexer
import qualified Luna.Syntax.Text.Lexer.Token         as Token
import qualified Luna.Syntax.Text.Parser.State.Marker as Marker
import qualified Text.Megaparsec                      as Parsec

import Control.Monad.State.Layered              (StatesT)
import Data.Text.Position                       (FileOffset)
import Luna.Syntax.Text.Parser.Data.CodeSpan    (CodeSpanRange)
import Luna.Syntax.Text.Parser.State.Indent     (Indent)
import Luna.Syntax.Text.Parser.State.LastOffset (LastOffset)
import Luna.Syntax.Text.Parser.State.Reserved   (Reserved)
import Luna.Syntax.Text.Scope                   (Scope)
import Text.Megaparsec                          (ParsecT)
import Text.Megaparsec                          (MonadParsec)
import Text.Parser.Backend.Megaparsec           ()



-------------------------
-- === TokenParser === --
-------------------------

-- === Definition === --

type Token       = Lexer.Token Lexer.Symbol
type Stream      = [Token]
type Error       = Void
type MonadParser = MonadParsec Error Stream
type ParserBase  = ParsecT Error Stream IO
type Parser      = StatesT ParserStates ParserBase
type ParserStates
    = '[ Indent
       , FileOffset
       , Marker.State
       , LastOffset
       , Scope
       , Reserved
       , CodeSpanRange
       ]


-- === Instances === --

app_tmp a (x,y) = (a:x, y)

-- FIXME[WD]: Describe the hacks
instance Parsec.Stream Stream where
    type Token  Stream = Token
    type Tokens Stream = [Token]

    tokenToChunk  _ = pure   ; {-# INLINE tokenToChunk  #-}
    tokensToChunk _ = id     ; {-# INLINE tokensToChunk #-}
    chunkToTokens _ = id     ; {-# INLINE chunkToTokens #-}
    chunkLength   _ = length ; {-# INLINE chunkLength   #-}
    chunkEmpty    _ = null   ; {-# INLINE chunkEmpty    #-}
    positionAt1   _ p _ = p  -- FIXME
    positionAtN   _ p _ = p  -- FIXME
    advance1      _ _ p _ = p  -- FIXME
    advanceN      _ _ p _ = p  -- FIXME
    take1_              = \case (a:as) -> Just (a,as)
                                []     -> Just (Token.etx, [])
    takeN_        0 s      = Just ([],s)
    takeN_        1 []     = Just ([Token.etx],[])
    takeN_        i (a:as) = app_tmp a <$> Parsec.takeN_ (i - 1) as
    takeN_        _ _      = Nothing

    takeWhile_    f     = \case []     -> ([], [])
                                (a:as) -> if f a then app_tmp a (Parsec.takeWhile_ f as)
                                                 else ([], (a:as))

    -- takeWhile_ :: (Token s -> Bool) -> s -> (Tokens s, s)
    -- uncons = List.uncons
    -- updatePos _ _ cpos _ = (cpos, cpos)
    -- updatePos _ _ (cpos@(Parser.SourcePos n l c)) (Lexer.Token (Lexer.Span w o) _) = (cpos, Parser.SourcePos n (Parser.unsafePos . unsafeConvert $ unwrap o + 1)
    --                                                                                                            (Parser.unsafePos $ Parser.unPos c + (unsafeConvert $ unwrap $ w + o)))

instance Parsec.ShowToken Token where
    showTokens = show

