{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# EXT      InlineAll                 #-}

module Luna.Syntax.Text.Parser.Parser (module Luna.Syntax.Text.Parser.Parser, module X) where

import Prologue

import qualified Control.Monad.State.Layered         as State
import qualified Luna.IR                             as IR
import qualified Luna.IR.Component.Term.Construction as Term
import qualified Luna.Pass                           as Pass
import qualified Luna.Syntax.Text.Lexer              as Lexer
import qualified Luna.Syntax.Text.Parser.Class       as Parser
import qualified Text.Megaparsec                     as Parser

import Control.Monad.State.Layered       (StatesT)
import Control.Monad.State.Layered       (StateT)
import Data.Text.Position                (FileOffset)
import Luna.Pass                         (Pass)
import Luna.Syntax.Text.Parser.Class     as X (Error)
import Luna.Syntax.Text.Parser.Class     (Stream)
import Luna.Syntax.Text.Parser.CodeSpan  (CodeSpan, CodeSpanRange)
import Luna.Syntax.Text.Parser.Hardcoded (hardcode)
import Luna.Syntax.Text.Parser.Loc       (LeftSpanner)
import Luna.Syntax.Text.Parser.Marker    (MarkedExprMap, MarkerState,
                                          UnmarkedExprs)
import Luna.Syntax.Text.Parser.Reserved  (Reservation)
import Luna.Syntax.Text.Scope            (Scope)
import Text.Megaparsec                   (ParseError, ParsecT)
import Text.Parser.Backend.Megaparsec    ()
import Text.Parser.Indent                (Indent)



-- type ParserBase = ParsecT Error Text (StateT Scope IO)
type ParserBase = ParsecT Error Stream IO

-- type Parser = StatesT '[Indent, FileOffset, Position, MarkerState
--     , LeftSpanner, Scope, Reservation, CodeSpanRange] ParserBase
type Parser = StatesT ParserStates ParserBase
type ParserStates
    = '[ Indent
       , FileOffset
       , MarkerState
       , LeftSpanner
       , Scope
       , Reservation
       , CodeSpanRange
       ]



