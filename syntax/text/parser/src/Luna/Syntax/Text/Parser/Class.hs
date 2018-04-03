module Luna.Syntax.Text.Parser.Class where

import           Prologue hiding (String, Symbol, Tok, Type)
import qualified Prologue as P

import Luna.IR
import Luna.Syntax.Text.Layer.Loc
-- import OCI.Pass.Class
-- import OCI.Pass.Definition

import Luna.Syntax.Text.Parser.CodeSpan
-- import Luna.Syntax.Text.Parser.Marker   (MarkedExprMap)
import Luna.Syntax.Text.Source (Source)
-- import qualified OCI.IR                           as IR
import           Text.Megaparsec       hiding (Pos, Stream, parse, uncons,
                                        (<?>))
import qualified Text.Megaparsec       as Parser
import qualified Text.Megaparsec.Error as Error
import qualified Text.Megaparsec.Error as Error
import           Text.Megaparsec.Prim  (MonadParsec)

import qualified Luna.Syntax.Text.Lexer as Lexer

import qualified Data.List              as List
import           Data.Set               (Set)
import qualified Data.Set               as Set
import           Data.Text.Position     (Delta)
import qualified Luna.Syntax.Text.Lexer as Lexer


infix 1 <?>
(<?>) :: MonadParsec e s m => m a -> P.String -> m a
(<?>) = (Parser.<?>) ; {-# INLINE (<?>) #-}

type Symbol      = Lexer.Symbol
type Stream      = [Tok]
type Error       = Error.Dec
type Tok         = Lexer.Token Lexer.Symbol
type MonadParser = MonadParsec Error Stream


-- FIXME[WD]: Describe the hacks
instance Parser.Stream Stream where
    type Token Stream = Tok
    uncons = List.uncons
    updatePos _ _ cpos _ = (cpos, cpos)
    -- updatePos _ _ (cpos@(Parser.SourcePos n l c)) (Lexer.Token (Lexer.Span w o) _) = (cpos, Parser.SourcePos n (Parser.unsafePos . unsafeConvert $ unwrap o + 1)
    --                                                                                                            (Parser.unsafePos $ Parser.unPos c + (unsafeConvert $ unwrap $ w + o)))

instance Parser.ShowToken Tok where
    showTokens = show
