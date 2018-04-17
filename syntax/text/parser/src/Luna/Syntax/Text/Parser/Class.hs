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
import qualified Data.List              as List
import qualified Data.Set               as Set
import qualified Luna.Syntax.Text.Lexer as Lexer
import qualified Luna.Syntax.Text.Lexer as Lexer
import qualified Text.Megaparsec        as Parser
import qualified Text.Megaparsec.Error  as Error
import qualified Text.Megaparsec.Error  as Error


import Data.Set           (Set)
import Data.Text.Position (Delta)
import Text.Megaparsec    hiding (Pos, Stream, parse, uncons, (<?>))


infix 1 <?>
(<?>) :: MonadParsec e s m => m a -> P.String -> m a
(<?>) = (Parser.<?>) ; {-# INLINE (<?>) #-}

type Symbol      = Lexer.Symbol
type Stream      = [Tok]
type Error       = Void -- Error.Dec
type Tok         = Lexer.Token Lexer.Symbol
type MonadParser = MonadParsec Error Stream

app_tmp a (x,y) = (a:x, y)

-- FIXME[WD]: Describe the hacks
instance Parser.Stream Stream where
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
    takeN_        i (a:as) = app_tmp a <$> takeN_ (i - 1) as
    takeN_        _ _      = Nothing

    takeWhile_    f     = \case []     -> ([], [])
                                (a:as) -> if f a then app_tmp a (takeWhile_ f as)
                                                 else ([], (a:as)) where

    -- takeWhile_ :: (Token s -> Bool) -> s -> (Tokens s, s)
    -- uncons = List.uncons
    -- updatePos _ _ cpos _ = (cpos, cpos)
    -- -- updatePos _ _ (cpos@(Parser.SourcePos n l c)) (Lexer.Token (Lexer.Span w o) _) = (cpos, Parser.SourcePos n (Parser.unsafePos . unsafeConvert $ unwrap o + 1)
    --                                                                                                            (Parser.unsafePos $ Parser.unPos c + (unsafeConvert $ unwrap $ w + o)))

instance Parser.ShowToken Tok where
    showTokens = show




    