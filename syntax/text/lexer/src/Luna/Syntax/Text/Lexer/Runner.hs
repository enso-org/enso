{-# LANGUAGE ScopedTypeVariables #-}

module Luna.Syntax.Text.Lexer.Runner where

import Prologue hiding (Symbol)

import qualified Control.Monad.State.Layered    as State
import qualified Data.Attoparsec.Internal.Types as Parser
import qualified Data.Text32                    as Text32
import qualified Luna.Syntax.Text.Lexer.Grammar as Grammar
import qualified Luna.Syntax.Text.Lexer.Symbol  as Symbol

import Data.Attoparsec.Internal.Types (IResult)
import Data.Parser                    (PartialParser, closePartial,
                                       parsePartial)
import Data.Text32                    (Text32)
import Luna.Syntax.Text.Lexer.Grammar (EntryStack, Parser, lexer)
import Luna.Syntax.Text.Lexer.Symbol  (Symbol)
import Luna.Syntax.Text.Lexer.Token   (Token (Token))

import Data.Parser.Instances.Attoparsec ()


---------------------
-- === Running === --
---------------------

parse2 :: Parser (a, Int) -> EntryStack -> Text32   -> IResult Text32 ((a, Int), EntryStack)
parse2 p x s = parsePartial (State.runT @EntryStack p x) s
{-# INLINE parse2 #-}


parse2' :: Text32 -> [Token]
parse2' = \s ->
    let s'  = Text32.dropWhile (== ' ') s
        off = Text32.length s - Text32.length s'
    in  stx off : evalDefLexer s'
{-# INLINE parse2' #-}


evalDefLexer :: Text32 -> [Token]
evalDefLexer = evalDefLexer_ mempty
{-# INLINE evalDefLexer #-}

evalDefLexer_ :: [Token] -> Text32 -> [Token]
evalDefLexer_ = go where
    go ts s = let

        handleDone s' ((!symbol, !off), !stack) =
            let span  = convert $! Text32.length s - Text32.length s' - off
                isEnd = Text32.null s'
                token = Token span (convert off) symbol stack
                ts'   = token : ts
            in if isEnd then ts' else go ts' s'
        {-# INLINE handleDone #-}

        handleOut f = \case
            Parser.Done !(s') !r -> handleDone s' r
            Parser.Fail !_ !_ !e -> error e
            Parser.Partial !g    -> f $! g mempty
        {-# INLINE handleOut  #-}

        runSteps = handleOut $! handleOut (const impossible)
        {-# INLINE runSteps #-}

        in runSteps $! parse2 lexer def s
{-# INLINE evalDefLexer_ #-}


-- === STX / ETX handling === --

class IsSourceBorder a where
    stx :: Int -> a
    etx :: a

instance IsSourceBorder r => IsSourceBorder (Either l r) where
    stx = Right . stx
    etx = Right   etx
    {-# INLINE stx #-}
    {-# INLINE etx #-}

instance IsSourceBorder Token where
    stx i = Token mempty (convert i) (stx i) mempty
    etx   = Token mempty mempty      etx     mempty
    {-# INLINE stx #-}
    {-# INLINE etx #-}

instance IsSourceBorder Symbol where
    stx _ = Symbol.STX
    etx   = Symbol.ETX
    {-# INLINE stx #-}
    {-# INLINE etx #-}

instance IsSourceBorder (Symbol, EntryStack) where
    stx i = (stx i, mempty)
    etx   = (etx, mempty)
    {-# INLINE stx #-}
    {-# INLINE etx #-}





-- prependSTX :: (Monad m, IsSourceBorder s) => ConduitM Text32 s m () -> ConduitM Text32 s m ()
-- prependSTX f = await >>= \case
--     Nothing -> pure ()
--     Just t  -> yield (stx $ Text32.length s) >> when (not $ Text32.null t') (leftover t') >> f where
--         (s,t') = Text32.span (== ' ') t
-- {-# INLINE prependSTX #-}


-- fromLexerResult :: Either ParseError a -> a
-- fromLexerResult = either (error . ("Impossible happened: lexer error: " <>) . show) id ; {-# INLINE fromLexerResult #-}

-- parseBase :: (Monad m, IsSourceBorder t) => Parser (t, Int) -> EntryStack -> ConduitM a Text32 m () -> ConduitM a c0 m [Either ParseError (Token t)]
-- parseBase p s f = f .| prependSTX (conduitParserEither s $ State.runT @EntryStack p) .| sinkList ; {-# INLINE parseBase #-}

-- parse        :: IsSourceBorder a =>                  Parser (a, Int) -> EntryStack -> Text32   ->   [Token a]
-- tryParse     :: IsSourceBorder a =>                  Parser (a, Int) -> EntryStack -> Text32   ->   Either ParseError [Token a]
-- parseFile    :: IsSourceBorder a => MonadIO m => Parser (a, Int) -> EntryStack -> FilePath -> m [Token a]
-- tryParseFile :: IsSourceBorder a => MonadIO m => Parser (a, Int) -> EntryStack -> FilePath -> m (Either ParseError [Token a])
-- parse              = fromLexerResult .:. tryParse                                                ; {-# INLINE parse        #-}
-- parseFile          = fromLexerResult .:: tryParseFile                                            ; {-# INLINE parseFile    #-}
-- tryParse     p s t = sequence . runConduitPure $ parseBase p s (sourceProducer t)                ; {-# INLINE tryParse     #-}
-- tryParseFile p s t = liftIO . fmap sequence . runConduitRes $ parseBase p s (sourceReader t) ; {-# INLINE tryParseFile #-}

-- runLexer     :: EntryStack -> Text32 -> [Token (Symbol, EntryStack)]
-- evalLexer    :: EntryStack -> Text32 -> [Token Symbol]
-- evalDefLexer ::               Text32 -> [Token Symbol]
-- runLexer     = parse lexerCont ; {-# INLINE runLexer     #-}
-- evalLexer    = parse lexer     ; {-# INLINE evalLexer    #-}
-- evalDefLexer = evalLexer def   ; {-# INLINE evalDefLexer #-}
