{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE UndecidableInstances      #-}

module Luna.Syntax.Text.Parser.IR.Ast (module Luna.Syntax.Text.Parser.IR.Ast, module X) where

import qualified Prelude  as P
import           Prologue hiding (Text, imp, seq, some, span, takeWhile)

import qualified Data.Text32                           as Text
import qualified Luna.IR.Term.Ast.Invalid              as Invalid
import qualified Luna.Syntax.Text.Parser.Data.CodeSpan as CodeSpan

import Luna.IR                               (SomeTerm, Term)
import Luna.Syntax.Text.Parser.Data.CodeSpan (CodeSpan (CodeSpan),
                                              CodeSpanRange (CodeSpanRange))
import OCI.Data.Name                         (Name)


--

import qualified Control.Monad.State.Layered            as State
import qualified Data.Attoparsec.Internal.Types         as AttoParsec
import qualified Data.Attoparsec.Text32                 as Parsec
import qualified Data.Char                              as Char
import qualified Data.Set                               as Set
import qualified Data.Text.Position                     as Position
import qualified Data.Text.Span                         as Span
import qualified GHC.Exts                               as GHC
import qualified Luna.Syntax.Text.Lexer                 as Lexer
import qualified Luna.Syntax.Text.Lexer.Symbol          as Lexer
import qualified Luna.Syntax.Text.Parser.Data.Ast.Class as Atom
import qualified Luna.Syntax.Text.Parser.State.Marker   as Marker
import qualified Luna.Syntax.Text.Scope                 as Scope

import Control.Monad.State.Layered              (StateT, StatesT)
import Data.Set                                 (Set)
import Data.Text.Position                       (FileOffset (..))
import Data.Text.Position                       (Delta, Position)
import Luna.Syntax.Text.Parser.State.LastOffset (LastOffset (LastOffset))
import Text.Parser.State.Indent                 (Indent)

import Data.Parser             hiding (Result, Token, Tokens, endOfInput)
import Text.Parser.Combinators (some)

import qualified Luna.Syntax.Text.Parser.Data.Ast.Simple   as Simple
import           Luna.Syntax.Text.Parser.Data.Ast.Spanned  as X
import           Luna.Syntax.Text.Parser.State.TokenStream (TokenStream)

-- type Text = Text.Text32
data SyntaxVersion = Syntax1 | Syntax2 deriving (Show)






type Parser = StatesT
   '[ TokenStream
    , SyntaxVersion
    , Indent
    , Position
    , LastOffset
    , CodeSpanRange
    , Marker.State
    , FileOffset
    , Scope.Scope
    ] Parsec.Parser



class Monad m => KnownParserOffset m where
    getParserOffset :: m Delta

instance KnownParserOffset Parsec.Parser where
    getParserOffset = AttoParsec.Parser $ \t pos more _ succ ->
        succ t pos more (convert $! AttoParsec.fromPos pos)
    {-# INLINE getParserOffset #-}

instance (MonadTrans t, Monad (t m), KnownParserOffset m)
      => KnownParserOffset (t m) where
    getParserOffset = lift getParserOffset
    {-# INLINE getParserOffset #-}










-------------------
-- === Spans === --
-------------------

getLastOffset :: State.Getter LastOffset m => m Delta
getLastOffset = unwrap <$> State.get @LastOffset
{-# INLINE getLastOffset #-}

putLastOffset :: State.Setter LastOffset m => Delta -> m ()
putLastOffset = State.put @LastOffset . wrap
{-# INLINE putLastOffset #-}

spanned :: Parser a -> Parser (CodeSpan, a)
spanned = \parser -> do
    lastEndOff <- getLastOffset
    startOff   <- getParserOffset
    out        <- parser
    endOff     <- getParserOffset
    nextOff    <- whiteSpace
    putLastOffset endOff

    let spacing   = startOff - lastEndOff
        bodyLen   = endOff - startOff
        totalSpan = bodyLen + nextOff
        realSpan  = Span.leftSpacedSpan spacing bodyLen

    Position.incColumn totalSpan
    pure (CodeSpan realSpan realSpan, out)
{-# INLINE spanned #-}

newline :: Parser ()
newline = do
    len <- eol
    off <- whiteSpace
    Position.succLine
    Position.incColumn off
{-# INLINE newline #-}

lexeme :: Parser a -> Parser a
lexeme = \p -> p <* whiteSpace
{-# INLINE lexeme #-}

whiteSpace :: Parser Delta
whiteSpace = convert . Text.length <$> takeMany ' '
{-# INLINE whiteSpace #-}

computeSpan :: Parser a -> Parser (Spanned a)
computeSpan = \p -> uncurry Spanned <$> spanned p
{-# INLINE computeSpan #-}

eol :: Parser Delta
eol = (1 <$ n) <|> (2 <$ rn) <|> (1 <$ r) where
    n  = token '\n'
    r  = token '\r'
    rn = r >> n
{-# INLINE eol #-}

isEolBeginChar :: Char -> Bool
isEolBeginChar = (`elem` eolStartChars)
{-# INLINE isEolBeginChar #-}

eolStartChars :: [Char]
eolStartChars = ['\n', '\r', '\ETX']
{-# INLINE eolStartChars #-}
