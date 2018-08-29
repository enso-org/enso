{-# LANGUAGE NoStrict     #-}
{-# LANGUAGE NoStrictData #-}

module Luna.Syntax.Text.Lexer.Runner where

import Prologue hiding (Symbol)

import qualified Control.Monad.State.Layered as State
import qualified Data.Text32                 as Text32

import Conduit                        hiding (liftIO)
import Data.Text32                    (Text32)
import Luna.Syntax.Text.IO
import Luna.Syntax.Text.Lexer.Grammar
import Luna.Syntax.Text.Lexer.Stream  (ParseError, conduitParserEither)
import Luna.Syntax.Text.Lexer.Symbol
import Luna.Syntax.Text.Lexer.Token   hiding (etx)



---------------------
-- === Running === --
---------------------

fromLexerResult :: Either ParseError a -> a
fromLexerResult = either (error . ("Impossible happened: lexer error: " <>) . show) id ; {-# INLINE fromLexerResult #-}

parseBase :: (Monad m, IsSourceBorder t) => Parser (t, Int) -> EntryStack -> ConduitM a Text32 m () -> ConduitM a c0 m [Either ParseError (Token t)]
parseBase p s f = f .| prependSTX (conduitParserEither s $ State.runT @EntryStack p) .| sinkList ; {-# INLINE parseBase #-}

parse        :: IsSourceBorder a =>                  Parser (a, Int) -> EntryStack -> Text32   ->   [Token a]
tryParse     :: IsSourceBorder a =>                  Parser (a, Int) -> EntryStack -> Text32   ->   Either ParseError [Token a]
parseFile    :: IsSourceBorder a => MonadIO m => Parser (a, Int) -> EntryStack -> FilePath -> m [Token a]
tryParseFile :: IsSourceBorder a => MonadIO m => Parser (a, Int) -> EntryStack -> FilePath -> m (Either ParseError [Token a])
parse              = fromLexerResult .:. tryParse                                                ; {-# INLINE parse        #-}
parseFile          = fromLexerResult .:: tryParseFile                                            ; {-# INLINE parseFile    #-}
tryParse     p s t = sequence . runConduitPure $ parseBase p s (sourceProducer t)                ; {-# INLINE tryParse     #-}
tryParseFile p s t = liftIO . fmap sequence . runConduitRes $ parseBase p s (sourceReader t) ; {-# INLINE tryParseFile #-}

runLexer     :: EntryStack -> Text32 -> [Token (Symbol, EntryStack)]
evalLexer    :: EntryStack -> Text32 -> [Token Symbol]
evalDefLexer ::               Text32 -> [Token Symbol]
runLexer     = parse lexerCont ; {-# INLINE runLexer     #-}
evalLexer    = parse lexer     ; {-# INLINE evalLexer    #-}
evalDefLexer = evalLexer def   ; {-# INLINE evalDefLexer #-}


-- === STX / ETX handling === --

prependSTX :: (Monad m, IsSourceBorder s) => ConduitM Text32 s m () -> ConduitM Text32 s m ()
prependSTX f = await >>= \case
    Nothing -> pure ()
    Just t  -> yield (stx $ Text32.length s) >> when (not $ Text32.null t') (leftover t') >> f where
        (s,t') = Text32.span (== ' ') t
{-# INLINE prependSTX #-}

class IsSourceBorder a where
    stx :: Int -> a
    etx :: a

instance IsSourceBorder r => IsSourceBorder (Either l r) where
    stx = Right . stx ; {-# INLINE stx #-}
    etx = Right   etx ; {-# INLINE etx #-}

instance IsSourceBorder t => IsSourceBorder (Token t) where
    stx i = Token mempty (convert i) (stx i) ; {-# INLINE stx #-}
    etx   = Token mempty mempty etx          ; {-# INLINE etx #-}

instance IsSourceBorder Symbol where
    stx _ = STX ; {-# INLINE stx #-}
    etx   = ETX ; {-# INLINE etx #-}

instance IsSourceBorder (Symbol, EntryStack) where
    stx i = (stx i, mempty) ; {-# INLINE stx #-}
    etx   = (etx, mempty)   ; {-# INLINE etx #-}

