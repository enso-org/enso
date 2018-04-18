{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# EXT      InlineAll                 #-}

module Luna.Syntax.Text.Parser.Class where

import Prologue

import qualified Control.Monad.State.Layered         as State
import qualified Luna.IR                             as IR
import qualified Luna.IR.Component.Term.Construction as Term
import qualified Luna.Pass                           as Pass
import qualified Luna.Syntax.Text.Lexer              as Lexer
import qualified Luna.Syntax.Text.Parser.Pass.Class  as Parser
import qualified Text.Megaparsec                     as Parsec

import Control.Monad.State.Layered              (StatesT)
import Control.Monad.State.Layered              (StateT)
import Data.Text.Position                       (FileOffset)
import Luna.Pass                                (Pass)
import Luna.Syntax.Text.Parser.CodeSpan         (CodeSpan, CodeSpanRange)
import Luna.Syntax.Text.Parser.Hardcoded        (hardcode)
import Luna.Syntax.Text.Parser.Marker           (MarkedExprMap, MarkerState,
                                                 UnmarkedExprs)
import Luna.Syntax.Text.Parser.State.LastOffset (LastOffset)
import Luna.Syntax.Text.Parser.State.Reserved   (Reserved)
import Luna.Syntax.Text.Scope                   (Scope)
import Text.Megaparsec                          (ParseError, ParsecT)
import Text.Megaparsec                          (MonadParsec)
import Text.Parser.Backend.Megaparsec           ()
import Text.Parser.Indent                       (Indent)

-- type ParserBase = ParsecT Error Text (StateT Scope IO)
type ParserBase = ParsecT Error Stream IO

-- type Parser = StatesT '[Indent, FileOffset, Position, MarkerState
--     , LastOffset, Scope, Reserved, CodeSpanRange] ParserBase
type Parser = StatesT ParserStates ParserBase
type ParserStates
    = '[ Indent
       , FileOffset
       , MarkerState
       , LastOffset
       , Scope
       , Reserved
       , CodeSpanRange
       ]










-- type Symbol      = Lexer.Symbol
type Stream      = [Tok]
type Error       = Void
type Tok         = Lexer.Token Lexer.Symbol
type MonadParser = MonadParsec Error Stream

app_tmp a (x,y) = (a:x, y)

-- FIXME[WD]: Describe the hacks
instance Parsec.Stream Stream where
    type Token  Stream = Tok
    type Tokens Stream = [Tok]

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
                                []     -> Nothing
    takeN_        0 s      = Just ([],s)
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

instance Parsec.ShowToken Tok where
    showTokens = show





