-- {-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Text.Parser.Loc where

import Prologue hiding (Symbol)

import qualified Control.Monad.State.Layered      as State
import qualified Data.Set                         as Set
import qualified Data.Text.Position               as Pos
import qualified Data.Text.Span                   as Span
import qualified Luna.Syntax.Text.Lexer           as Lexer
import qualified Luna.Syntax.Text.Parser.Reserved as Reserved
import qualified Text.Megaparsec.Pos              as Parser
import qualified Text.Megaparsec.Prim             as Parser

import Data.Set (Set)
-- import Data.Text.Position
-- import Data.Text.Span                   (RightSpacedSpan,
--                                          SpacedSpan (SpacedSpan),
--                                          rightSpacedSpan)
import Luna.Syntax.Text.Parser.Class (MonadParser, Tok)
-- import Luna.Syntax.Text.Parser.Marker   (MarkerState, cleanLastTokenMarker,
--                                          newLastTokenMarker)
import Luna.Syntax.Text.Parser.Reserved (Reservation)
import Text.Megaparsec.Error            (ErrorItem, ParseError)
import Text.Megaparsec.Prim             (MonadParsec, token)
-- -- import           OCI.IR                           (Name)
import Text.Megaparsec (withRecovery)

import Data.Text.Position            (Delta)
import Data.Text.Position            (FileOffset)
import Luna.Syntax.Text.Parser.Class (Stream, Symbol)

newtype LeftSpanner = LeftSpanner Delta deriving (Show)
makeLenses ''LeftSpanner

instance Default LeftSpanner where def = LeftSpanner mempty


-- -----------------
-- -- === Loc === --
-- -----------------

-- type MonadLoc m = (MonadStates '[FileOffset, Position, LeftSpanner, MarkerState] m, MonadIO m) -- FIXME[WD]: remove IO
type MonadLoc m = (State.MonadStates '[Pos.Position, LeftSpanner, FileOffset] m, MonadIO m)

-- -- === Utils === --

-- | Token overrides Megaparsec's one, with special position handling. We cannot do it another way around
--   because Megaparsec's `token` signature prevents any monadic action while consuming tokens.
token' :: (MonadParsec e Stream m, MonadLoc m, State.Getter Reservation m)
       => (Reservation -> Tok -> Either (Set (ErrorItem Tok), Set (ErrorItem Tok), Set e) a) -> Maybe Tok -> m a
token' f mt = do
    s <- State.get @Reservation
    let f' t = (t,) <$> f s t
    (tok, a) <- token f' mt
    updatePositions tok
    -- cleanLastTokenMarker
    -- dropMarkers
    return a

-- dropMarkers :: (MonadParsec e Stream m, MonadLoc m) => m ()
-- dropMarkers = withJustM previewNextToken $ \t -> case t ^. Lexer.element of
--     Lexer.Marker m -> newLastTokenMarker (t & Lexer.element .~ m) >> dropNextTokenAsMarker >> dropMarkers -- FIXME[WD]: should we handle the wrong markers?
--     _              -> return ()

-- getStream :: MonadParsec e Stream m => m Stream
-- getStream = Parser.getInput

-- putStream :: MonadParsec e Stream m => Stream -> m ()
-- putStream = Parser.setInput

-- previewNextToken :: MonadParsec e Stream m => m (Maybe Tok)
-- previewNextToken = maybeHead <$> previewTokens

-- previewTokens :: MonadParsec e Stream m => m [Tok]
-- previewTokens = getStream

-- previewSymbols :: MonadParsec e Stream m => m [Symbol]
-- previewSymbols = view Lexer.element <<$>> previewTokens

-- previewNextSymbol :: MonadParsec e Stream m => m (Maybe Symbol)
-- previewNextSymbol = view Lexer.element <<$>> previewNextToken

-- getNextOffset :: MonadParsec e Stream m => m Delta
-- getNextOffset = maybe 0 (view $ Lexer.offset) <$> previewNextToken

-- checkNextOffset :: MonadParsec e Stream m => m Bool
-- checkNextOffset = (>0) <$> getNextOffset

-- getNextToken :: (MonadParsec e Stream m, MonadLoc m) => m (Maybe Tok)
-- getNextToken = do
--     tok <- getNextToken'
--     cleanLastTokenMarker
--     dropMarkers
--     return tok

-- getNextToken' :: (MonadParsec e Stream m, MonadLoc m) => m (Maybe Tok)
-- getNextToken' = mapM handle . uncons =<< getStream where
--     handle (t,s) = do
--         putStream s
--         updatePositions t
--         return t

-- getNextToken'' :: (MonadParsec e Stream m, MonadLoc m) => m (Maybe Tok)
-- getNextToken'' = mapM handle . uncons =<< getStream where
--     handle (t,s) = do
--         putStream s
--         return t

-- getTokens :: (MonadParsec e Stream m, MonadLoc m) => Int -> m [Tok]
-- getTokens i = catMaybes <$> sequence (replicate i getNextToken)

-- uncheckedGetNextToken :: (MonadParsec e Stream m, MonadLoc m) => m Tok
-- uncheckedGetNextToken = maybe (error "Impossible happened: token stream end") id <$> getNextToken

-- uncheckedGetNextSymbol :: (MonadParsec e Stream m, MonadLoc m) => m Symbol
-- uncheckedGetNextSymbol = view Lexer.element <$> uncheckedGetNextToken

-- uncheckedPreviewNextToken :: (MonadParsec e Stream m, MonadLoc m) => m Tok
-- uncheckedPreviewNextToken = maybe (error "Impossible happened: token stream end") id <$> previewNextToken

-- uncheckedPreviewNextSymbol :: (MonadParsec e Stream m, MonadLoc m) => m Symbol
-- uncheckedPreviewNextSymbol = view Lexer.element <$> uncheckedPreviewNextToken

-- getNextSymbol :: (MonadParsec e Stream m, MonadLoc m) => m (Maybe Symbol)
-- getNextSymbol = view Lexer.element <<$>> getNextToken

-- unregisteredDropNextToken :: (MonadParsec e Stream m, MonadLoc m) => m ()
-- unregisteredDropNextToken = void getNextToken

-- dropNextTokenAsMarker :: (MonadParsec e Stream m, MonadLoc m) => m ()
-- dropNextTokenAsMarker = withJustM getNextToken'' go where
--     go t = modify_ @FileOffset (+ (convert delta)) >> modify_ @LeftSpanner (wrapped %~ (+delta)) where
--         delta = (t ^. Lexer.span) + (t ^. Lexer.offset)

-- unregisteredDropTokensUntil :: (MonadParsec e Stream m, MonadLoc m) => (Tok -> Bool) -> m ()
-- unregisteredDropTokensUntil f = withJustM previewNextToken $ \t -> if f t then return () else unregisteredDropNextToken >> unregisteredDropTokensUntil f

-- unregisteredDropSymbolsUntil :: (MonadParsec e Stream m, MonadLoc m) => (Symbol -> Bool) -> m ()
-- unregisteredDropSymbolsUntil f = unregisteredDropTokensUntil $ f . view Lexer.element

-- unregisteredDropSymbolsUntil' :: (MonadParsec e Stream m, MonadLoc m) => (Symbol -> Bool) -> m ()
-- unregisteredDropSymbolsUntil' f = unregisteredDropSymbolsUntil f >> unregisteredDropNextToken

updatePositions :: (MonadParsec e Stream m, MonadLoc m) => Lexer.Token Lexer.Symbol -> m ()
updatePositions t = do
    let len = t ^. Lexer.span
        off = t ^. Lexer.offset

    State.modify_ @FileOffset (+ (convert $ len + off))
    p <- Parser.getPosition
    Parser.setPosition $ p { Parser.sourceColumn = Parser.unsafePos $ Parser.unPos (Parser.sourceColumn p) + (convert $ unwrap $ len + off)
                           , Parser.sourceLine   = Parser.unsafePos $ convert $ unwrap (off + 1)
                           }
    case t ^. Lexer.element of
        -- Lexer.Marker m -> withJust m newLastTokenMarker -- FIXME[WD]: should we handle the wrong markers?
        Lexer.EOL      -> State.modify_ @LeftSpanner (wrapped %~ (+ (len + off))) >> Pos.succLine >> Pos.incColumn off
        _              -> State.put @LeftSpanner (wrap off) >> Pos.incColumn (len + off)


-- FIXME[WD]: This is just a hack. We store file offset and last spacing in Megaparsec's file position datatype,
--            because we cannot implement recovery other way around now. After running with recovery function, our custom position
--            is defaulted to the one before error happened, which is incorrect.
withRecovery2 :: (MonadParsec e Stream m, MonadLoc m) => (ParseError Tok e -> m a) -> m a -> m a
withRecovery2 f ma = do
    pos  <- Parser.getPosition
    out  <- withRecovery f ma
    pos' <- Parser.getPosition
    State.modify_ @FileOffset (+ convert (unsafeConvertTo @Int $ Parser.unPos (Parser.sourceColumn pos') - Parser.unPos (Parser.sourceColumn pos)))
    State.put @LeftSpanner $ wrap (convert $ unsafeConvertTo @Int $ Parser.unPos (Parser.sourceLine pos') - 1)
    return out

updateLineAndCol :: (MonadParsec e Stream m, MonadLoc m) => Lexer.Token Lexer.Symbol -> m ()
updateLineAndCol t = do
    Pos.incColumn (t ^. Lexer.span)
    when ((t ^. Lexer.element) == Lexer.EOL) $ Pos.succLine
    Pos.incColumn (t ^. Lexer.offset)
