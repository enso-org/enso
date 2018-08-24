{-# LANGUAGE NoStrict     #-}
{-# LANGUAGE NoStrictData #-}

module Luna.Syntax.Text.Parser.Loc where

import Prologue

import qualified Control.Monad.State.Layered            as State
import qualified Data.Set                               as Set
import qualified Data.Text.Position                     as Pos
import qualified Data.Text.Span                         as Span
import qualified Luna.IR.Term.Ast.Invalid               as Invalid
import qualified Luna.Syntax.Text.Lexer                 as Lexer
import qualified Luna.Syntax.Text.Lexer.Symbol          as Lexer
import qualified Luna.Syntax.Text.Lexer.Token           as Token
import qualified Luna.Syntax.Text.Parser.State.Marker   as Marker
import qualified Luna.Syntax.Text.Parser.State.Reserved as Reserved
import qualified Text.Megaparsec                        as Parser
import qualified Text.Megaparsec.Pos                    as Parser

import Data.Set                                 (Set)
import Data.Text.Position                       (Delta)
import Data.Text.Position                       (FileOffset)
import Luna.Syntax.Text.Parser.IR.Class         (MonadParser, Stream, Token)
import Luna.Syntax.Text.Parser.State.LastOffset (LastOffset (LastOffset))
import Luna.Syntax.Text.Parser.State.Reserved   (Reserved)
import Text.Megaparsec                          (MonadParsec, token,
                                                 withRecovery)
import Text.Megaparsec.Error                    (ErrorItem, ParseError)


-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- !!! TODO: Clean and refactor this file
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

-- -----------------
-- -- === Loc === --
-- -----------------

type MonadLoc m = (State.MonadStates '[FileOffset, Pos.Position, LastOffset, Marker.State] m, MonadIO m) -- FIXME[WD]: remove IO

-- -- === Utils === --


-- | Token overrides Megaparsec's one, with special position handling. We cannot do it another way around
--   because Megaparsec's `token` signature prevents any monadic action while consuming tokens.
token' :: (MonadParsec e Stream m, MonadLoc m, State.Getter Reserved m)
       => (Reserved -> Token -> Either (Maybe (ErrorItem Token), Set (ErrorItem Token)) a) -> m a
token' f = do
    s <- State.get @Reserved
    let f' t = (t,) <$> f s t
    (tok, a) <- token f' Nothing
    updatePositions tok
    Marker.clearLast
    dropMarkers
    return a

dropMarkers :: (MonadParsec e Stream m, MonadLoc m) => m ()
dropMarkers = previewNextToken >>= \t -> case t ^. Lexer.element of
    Lexer.Marker m -> Marker.setLast (t & Lexer.element .~ m) >> dropNextTokenAsMarker >> dropMarkers -- FIXME[WD]: should we handle the wrong markers?
    _              -> return ()

getStream :: MonadParsec e Stream m => m Stream
getStream = Parser.getInput

putStream :: MonadParsec e Stream m => Stream -> m ()
putStream = Parser.setInput

previewNextToken :: MonadParsec e Stream m => m Token
previewNextToken = fromJust Token.etx . head <$> previewTokens

previewTokens :: MonadParsec e Stream m => m [Token]
previewTokens = getStream

previewSymbols :: MonadParsec e Stream m => m [Lexer.Symbol]
previewSymbols = view Lexer.element <<$>> previewTokens

previewNextSymbol :: MonadParsec e Stream m => m Lexer.Symbol
previewNextSymbol = view Lexer.element <$> previewNextToken

getNextOffset :: MonadParsec e Stream m => m Delta
getNextOffset = view Lexer.offset <$> previewNextToken

checkNextOffset :: MonadParsec e Stream m => m Bool
checkNextOffset = (>0) <$> getNextOffset

getNextToken :: (MonadParsec e Stream m, MonadLoc m) => m (Maybe Token)
getNextToken = do
    tok <- getNextToken'
    Marker.clearLast
    dropMarkers
    return tok

getNextToken' :: (MonadParsec e Stream m, MonadLoc m) => m (Maybe Token)
getNextToken' = mapM handle . Parser.take1_ =<< getStream where
    handle (t,s) = do
        putStream s
        updatePositions t
        return t

getNextToken'' :: (MonadParsec e Stream m, MonadLoc m) => m (Maybe Token)
getNextToken'' = mapM handle . Parser.take1_ =<< getStream where
    handle (t,s) = do
        putStream s
        return t

getTokens :: (MonadParsec e Stream m, MonadLoc m) => Int -> m [Token]
getTokens i = catMaybes <$> sequence (replicate i getNextToken)

-- uncheckedGetNextToken :: (MonadParsec e Stream m, MonadLoc m) => m Token
-- uncheckedGetNextToken = maybe (error "Impossible happened: token stream end") id <$> getNextToken

-- uncheckedGetNextSymbol :: (MonadParsec e Stream m, MonadLoc m) => m Lexer.Symbol
-- uncheckedGetNextSymbol = view Lexer.element <$> uncheckedGetNextToken

-- uncheckedPreviewNextToken :: (MonadParsec e Stream m, MonadLoc m) => m Token
-- uncheckedPreviewNextToken = maybe (error "Impossible happened: token stream end") id <$> previewNextToken

-- uncheckedPreviewNextSymbol :: (MonadParsec e Stream m, MonadLoc m) => m Lexer.Symbol
-- uncheckedPreviewNextSymbol = view Lexer.element <$> uncheckedPreviewNextToken

getNextSymbol :: (MonadParsec e Stream m, MonadLoc m) => m (Maybe Lexer.Symbol)
getNextSymbol = view Lexer.element <<$>> getNextToken

unregisteredDropNextToken :: (MonadParsec e Stream m, MonadLoc m) => m ()
unregisteredDropNextToken = void getNextToken

dropNextTokenAsMarker :: (MonadParsec e Stream m, MonadLoc m) => m ()
dropNextTokenAsMarker = withJustM getNextToken'' go where
    go t = State.modify_ @FileOffset (+ (convert delta)) >> State.modify_ @LastOffset (wrapped %~ (+delta)) where
        delta = (t ^. Lexer.span) + (t ^. Lexer.offset)

unregisteredDropTokensUntil :: (MonadParsec e Stream m, MonadLoc m) => (Token -> Bool) -> m ()
unregisteredDropTokensUntil f = previewNextToken >>= \t -> if f t then return () else unregisteredDropNextToken >> unregisteredDropTokensUntil f

unregisteredDropSymbolsUntil :: (MonadParsec e Stream m, MonadLoc m) => (Lexer.Symbol -> Bool) -> m ()
unregisteredDropSymbolsUntil f = unregisteredDropTokensUntil $ f . view Lexer.element

unregisteredDropSymbolsUntil' :: (MonadParsec e Stream m, MonadLoc m) => (Lexer.Symbol -> Bool) -> m ()
unregisteredDropSymbolsUntil' f = unregisteredDropSymbolsUntil f >> unregisteredDropNextToken

dropSymbolsUntilAndGatherErrors :: (MonadParsec e Stream m, MonadLoc m) => (Lexer.Symbol -> Bool) -> [Invalid.Symbol] -> m (Either [Invalid.Symbol] [Invalid.Symbol])
dropSymbolsUntilAndGatherErrors f es = previewNextToken >>= go where
    f' a = f a || a == Lexer.ETX
    go t = if el == Lexer.ETX
        then pure $ Left es
        else if f' el
            then Right es <$ unregisteredDropNextToken
            else do
                let es' = maybe es (:es) (Lexer.matchInvalid el)
                unregisteredDropNextToken
                dropSymbolsUntilAndGatherErrors f es'
        where el = t ^. Lexer.element

getTokensUntil :: (MonadParsec e Stream m, MonadLoc m) => (Lexer.Symbol -> Bool) -> m [Token]
getTokensUntil f = previewNextToken >>=
               \t -> if f (t ^. Lexer.element)
                           then return mempty
                           else unregisteredDropNextToken
                             >> ((t:) <$> getTokensUntil f)
{-# INLINE getTokensUntil #-}


updatePositions :: (MonadParsec e Stream m, MonadLoc m) => Lexer.Token Lexer.Symbol -> m ()
updatePositions t = do
    let len = t ^. Lexer.span
        off = t ^. Lexer.offset

    State.modify_ @FileOffset (+ (convert $ len + off))
    p <- Parser.getPosition
    -- print ">>>"
    -- print $ Parser.unPos (Parser.sourceColumn p) + (unwrap $ len + off)
    -- print $ (off + 1)
    Parser.setPosition $ p { Parser.sourceColumn = Parser.mkPos $ Parser.unPos (Parser.sourceColumn p) + (unwrap $ len + off)
                           , Parser.sourceLine   = Parser.mkPos $ unwrap (off + 1)
                           }
    case t ^. Lexer.element of
        -- Lexer.Marker m -> withJust m Marker.setLast -- FIXME[WD]: should we handle the wrong markers?
        Lexer.EOL      -> State.modify_ @LastOffset (wrapped %~ (+ (len + off))) >> Pos.succLine >> Pos.incColumn off
        _              -> State.put @LastOffset (wrap off) >> Pos.incColumn (len + off)


-- -- FIXME[WD]: This is just a hack. We store file offset and last spacing in Megaparsec's file position datatype,
-- --            because we cannot implement recovery other way around now. After running with recovery function, our custom position
-- --            is defaulted to the one before error happened, which is incorrect.
-- withRecovery2 :: (MonadParsec e Stream m, MonadLoc m) => (ParseError Token e -> m a) -> m a -> m a
-- withRecovery2 f ma = do
--     pos  <- Parser.getPosition
--     out  <- withRecovery f ma
--     pos' <- Parser.getPosition
--     State.modify_ @FileOffset (+ convert (Parser.unPos (Parser.sourceColumn pos') - Parser.unPos (Parser.sourceColumn pos)))
--     State.put @LastOffset $ wrap (convert $ Parser.unPos (Parser.sourceLine pos') - 1)
--     return out

-- updateLineAndCol :: (MonadParsec e Stream m, MonadLoc m) => Lexer.Token Lexer.Lexer.Symbol -> m ()
-- updateLineAndCol t = do
--     Pos.incColumn (t ^. Lexer.span)
--     when ((t ^. Lexer.element) == Lexer.EOL) $ Pos.succLine
--     Pos.incColumn (t ^. Lexer.offset)

