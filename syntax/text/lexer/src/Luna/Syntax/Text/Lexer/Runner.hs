{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Luna.Syntax.Text.Lexer.Runner where

import Prologue hiding (Symbol)

import qualified Control.Monad.State.Layered    as State
import qualified Data.Attoparsec.Internal.Types as Parser
import qualified Data.Text32                    as Text32
import qualified Luna.Syntax.Text.Lexer.Grammar as Grammar
import qualified Luna.Syntax.Text.Lexer.Symbol  as Symbol
import qualified Text.Parser.State.Indent       as State.Indent

import Data.Attoparsec.Internal.Types (IResult)
import Data.Parser                    (PartialParser, closePartial,
                                       parsePartial)
import Data.Text.Position             (Delta)
import Data.Text32                    (Text32)
import Luna.Syntax.Text.Lexer.Grammar (Parser, lexer)
import Luna.Syntax.Text.Lexer.Symbol  (Symbol)
import Luna.Syntax.Text.Lexer.Token   (Token, token)

import Data.Parser.Instances.Attoparsec ()



---------------------
-- === Running === --
---------------------

evalDefLexer :: Text32 -> [Token]
evalDefLexer = \s ->
    let s'  = Text32.dropWhile (== ' ') s
        off = Text32.length s - Text32.length s'
        go  = reverse . evalLexer__
    in  stx off : go s'
{-# INLINE evalDefLexer #-}

-- evalLexer__ :: EntryStack -> Delta -> Delta -> [Token] -> Text32 -> [Token]
-- evalLexer__ = go where
--     go stack col row toks txt = let

--         handleDone txt' ((!symbol, !ioff), !stack') =
--             let pdiff = convert $! Text32.length txt - Text32.length txt'
--                 off   = convert ioff
--                 span  = pdiff - off
--                 isEnd = Text32.null txt'
--                 tok   = token span off col row stack' symbol
--                 toks' = tok : toks
--                 (!col', !row') = if symbol == Symbol.EOL
--                     then (0, row + 1)
--                     else (col + pdiff, row)
--             in if isEnd then toks' else go stack' col' row' toks' txt'
--         {-# INLINE handleDone #-}

--         handleOut f = \case
--             Parser.Done !(txt') !r -> handleDone txt' r
--             Parser.Fail !_ !_ !e   -> error e
--             Parser.Partial !g      -> f $! g mempty
--         {-# INLINE handleOut  #-}

--         runSteps = handleOut $! handleOut (const impossible)
--         {-# INLINE runSteps #-}

--         in runSteps $! runner__ lexer stack txt col row
-- {-# INLINE evalLexer__ #-}

evalLexer__ :: Text32 -> [Token]
evalLexer__ = \txt -> case runner__ txt of
    Parser.Done !(txt') !r -> if (Text32.length txt' /= 0)
        then error $ "Panic. Not all input consumed by lexer: " <> show txt'
        else unwrap r
    Parser.Partial g -> case g mempty of
        Parser.Done !(txt') !r -> if (Text32.length txt' /= 0)
            then error $ "Panic. Not all input consumed by lexer: " <> show txt'
            else unwrap r
        Parser.Fail !_ !_ !e   -> error e
        Parser.Partial {} -> impossible

    Parser.Fail !_ !_ !e   -> error e

        -- handleDone txt' ((!symbol, !ioff), !stack') =
        --     let pdiff = convert $! Text32.length txt - Text32.length txt'
        --         off   = convert ioff
        --         span  = pdiff - off
        --         isEnd = Text32.null txt'
        --         tok   = token span off col row stack' symbol
        --         toks' = tok : toks
        --         (!col', !row') = if symbol == Symbol.EOL
        --             then (0, row + 1)
        --             else (col + pdiff, row)
        --     in if isEnd then toks' else go stack' col' row' toks' txt'
        -- {-# INLINE handleDone #-}

        -- handleOut f = \case
        --     Parser.Done !(txt') !r -> handleDone txt' r
        --     Parser.Fail !_ !_ !e   -> error e
        --     Parser.Partial !g      -> f $! g mempty
        -- {-# INLINE handleOut  #-}

        -- runSteps = handleOut $! handleOut (const impossible)
        -- {-# INLINE runSteps #-}

        -- in runSteps $! runner__ lexer stack txt col row
{-# INLINE evalLexer__ #-}

runner__ :: Text32 -> IResult Text32 Grammar.Result
runner__ = \txt
    -> flip parsePartial (txt <> "\ETX")
     $ flip (State.evalT @Grammar.Location) mempty
     $ State.Indent.eval
     $ flip (State.execT  @Grammar.Result) mempty
     $ lexer
{-# INLINE runner__ #-}


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
    stx i = token mempty (convert i) 0 0 True (stx i)
    etx   = token mempty mempty      0 0 True etx
    {-# INLINE stx #-}
    {-# INLINE etx #-}

instance IsSourceBorder Symbol where
    stx _ = Symbol.STX
    etx   = Symbol.ETX
    {-# INLINE stx #-}
    {-# INLINE etx #-}

-- instance IsSourceBorder (Symbol, EntryStack) where
--     stx i = (stx i, mempty)
--     etx   = (etx, mempty)
--     {-# INLINE stx #-}
--     {-# INLINE etx #-}





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
