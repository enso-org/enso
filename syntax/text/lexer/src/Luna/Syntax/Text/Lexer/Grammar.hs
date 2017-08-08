{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE Strict                    #-}

module Luna.Syntax.Text.Lexer.Grammar where

import Prologue hiding (List, Type, Symbol, cons, span, range, catch, takeWhile, Text)
import qualified Prologue as P

import           Control.Monad.State.Layered
import qualified Data.Char                    as Char
import qualified Data.List                    as List
import qualified Data.Map.Strict              as Map
import           Data.Map.Strict              (Map)
import qualified Data.Text.Span               as Span
import           Data.Text.Position           (Delta, Position, Offset)
import qualified Data.Text.Position           as Position
import           Data.Vector                  (Vector)
import qualified Data.Vector                  as Vector
import           Data.Container.Text32        (Text32)
import qualified Data.Container.Text32        as Text
import           Luna.Syntax.Text.Lexer.Stream (ParseError, conduitParserEither)
import           Conduit
import qualified Data.Attoparsec.Text32 as Parsec
import           Luna.Syntax.Text.Lexer.Token

import Data.Parser hiding (Token)
import Data.Parser.Instances.Attoparsec ()
import Luna.Syntax.Text.Lexer.Symbol
import Luna.Syntax.Text.IO


-------------------------
-- === Lexer types === --
-------------------------

type Parser = StateT EntryStack Parsec.Parser
type Lexer  = Parser Symbol


------------------------
-- === EntryStack === --
------------------------

-- === Definition === --

data EntryPoint = TopLevelEntry
                | StrEntry !StrType !Int
                | StrCodeEntry !Int
                deriving (Show)

type EntryStack = [EntryPoint]


-- === Utils === --

liftEntry :: MonadState EntryStack m => EntryPoint -> m ()
liftEntry = modify_ @EntryStack . (:) ; {-# INLINE liftEntry #-}

unliftEntry :: MonadState EntryStack m => m ()
unliftEntry = modify_ @EntryStack $ \lst -> case maybeTail lst of
    Just p  -> p
    Nothing -> error "Impossible happened: trying to unlift global lexer entry. Please report this issue to Luna developers."
{-# INLINE unliftEntry #-}

getEntryPoint :: MonadState EntryStack m => m EntryPoint
getEntryPoint = maybe def id . maybeHead <$> get @EntryStack ; {-# INLINE getEntryPoint #-}


-- === Instances === --

instance Default EntryPoint where def = TopLevelEntry ; {-# INLINE def #-}


-----------------------
-- === Layouting === --
-----------------------

notNewlineStart :: Char -> Bool
notNewlineStart c = c /= '\n' && c /= '\r' ; {-# INLINE notNewlineStart #-}


----------------------
-- === Literals === --
----------------------

-- === Char by char checking === --

isDecDigitChar, isOctDigitChar, isBinDigitChar, isHexDigitChar, isIndentBodyChar :: Char -> Bool
isDecDigitChar   c = (c >= '0' && c <= '9')                                                           ; {-# INLINE isDecDigitChar   #-}
isOctDigitChar   c = (c >= '0' && c <= '7')                                                           ; {-# INLINE isOctDigitChar   #-}
isBinDigitChar   c = (c == '0' || c == '1')                                                           ; {-# INLINE isBinDigitChar   #-}
isHexDigitChar   c = isDecDigitChar c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')             ; {-# INLINE isHexDigitChar   #-}
isIndentBodyChar c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || isDecDigitChar c || c == '_' ; {-# INLINE isIndentBodyChar #-}

opChars :: [Char]
opChars = "!$%&*+-/<>?^~\\" ; {-# INLINE opChars #-}


-- === Numbers === --

lexNumber :: Lexer
lexNumber = check =<< number where
    number  = (token '0' *> choice [hex, oct, bin]) <|> dec
    dec     = NumRep Dec           <$> decBody <*> fracSfx     <*> expSfx
    hex     = NumRep Hex <$ hexPfx <*> hexBody <*> pure mempty <*> pure mempty
    oct     = NumRep Oct <$ octPfx <*> octBody <*> pure mempty <*> pure mempty
    bin     = NumRep Bin <$ binPfx <*> binBody <*> pure mempty <*> pure mempty
    hexPfx  = satisfy (\s -> s == 'x' || s == 'X')
    octPfx  = satisfy (\s -> s == 'o' || s == 'O')
    binPfx  = satisfy (\s -> s == 'b' || s == 'B')
    decBody = takeWhile1 isDecDigitChar
    hexBody = takeWhile1 isHexDigitChar
    octBody = takeWhile1 isOctDigitChar
    binBody = takeWhile1 isBinDigitChar
    expBody = maybe id Text.cons <$> optional sign <*> decBody
    fracSfx = option mempty $ token '.' *> decBody
    expSfx  = option mempty $ token 'e' *> expBody
    sign    = satisfy (\s -> s == '-' || s == '+')
    check n = option (Number n) $ (\s -> Incorrect $ "Unexpected characters '" <> s <> "' found on the end of number literal") <$> takeWhile1 Char.isAlphaNum
{-# INLINE lexNumber #-}


-- === String parsing utils === --

beginStr :: StrType -> Char -> Lexer
beginStr t c = Quote t Begin <$ (liftEntry . StrEntry t =<< beginQuotes c) ; {-# INLINE beginStr #-}

beginQuotes :: Char -> Parser Int
beginQuotes !c = beginMultiQuotes c <|> (1 <$ token c) ; {-# INLINE beginQuotes #-}

beginMultiQuotes :: Char -> Parser Int
beginMultiQuotes !c = do
    !len <- Text.length <$> takeMany1 c
    when (len == 2) $ fail "Empty string"
    return len
{-# INLINE beginMultiQuotes #-}


-- === Top level string parsers === --

natStrQuote, rawStrQuote, fmtStrQuote :: Char
natStrQuote = '`'  ; {-# INLINE natStrQuote #-}
rawStrQuote = '"'  ; {-# INLINE rawStrQuote #-}
fmtStrQuote = '\'' ; {-# INLINE fmtStrQuote #-}
escapeChar  = '\\' ; {-# INLINE escapeChar  #-}

natStr, rawStr, fmtStr :: Lexer
natStr = beginStr NatStr natStrQuote ; {-# INLINE natStr #-}
rawStr = beginStr RawStr rawStrQuote ; {-# INLINE rawStr #-}
fmtStr = beginStr FmtStr fmtStrQuote ; {-# INLINE fmtStr #-}


-- === Native String === --

natStrBody :: Int -> Lexer
natStrBody hlen = code <|> quotes where
    code   = Str <$> takeWhile1 (/= natStrQuote)
    quotes = do
        qs <- takeMany1 natStrQuote
        if Text.length qs == hlen
            then Quote NatStr End <$ unliftEntry
            else return $ Str qs
{-# INLINE natStrBody #-}


-- === Raw String === --

rawStrBody :: Int -> Lexer
rawStrBody hlen = choice [body, escape, quotes, linebr] where
    body   = Str <$> takeWhile1 (\c -> c /= rawStrQuote && notNewlineStart c && c /= escapeChar)
    linebr = EOL <$  newline
    escape = StrEsc <$ token escapeChar <*> esct
    esct   = (SlashEsc <$ token escapeChar)
         <|> (QuoteEscape RawStr . Text.length <$> takeMany1 rawStrQuote)
         <|> (QuoteEscape FmtStr . Text.length <$> takeMany1 fmtStrQuote)
    quotes = do
        qs <- takeMany1 rawStrQuote
        if Text.length qs == hlen
            then Quote RawStr End <$ unliftEntry
            else return $ Str qs
{-# INLINE rawStrBody #-}


-- === Fmt String === --

fmtStrBody :: Int -> Lexer
fmtStrBody hlen = choice [body, escape, quotes, linebr, code] where
    body   = Str <$> takeWhile1 (\c -> c /= fmtStrQuote && c /= natStrQuote && notNewlineStart c && c /= escapeChar)
    linebr = EOL <$  newline
    escape = token escapeChar *> esct
    esct   = (StrEsc   SlashEsc <$ token escapeChar)
         <|> (StrEsc . QuoteEscape RawStr . Text.length <$> takeMany1 rawStrQuote)
         <|> (StrEsc . QuoteEscape FmtStr . Text.length <$> takeMany1 fmtStrQuote)
         <|> lexEscSeq
    quotes = do
        qs <- takeMany1 fmtStrQuote
        if Text.length qs == hlen
            then Quote FmtStr End <$ unliftEntry
            else return $ Str qs
    code   = Block Begin <$ (liftEntry . StrCodeEntry =<< beginQuotes natStrQuote)
{-# INLINE fmtStrBody #-}

fmtStrCode :: Int -> Lexer
fmtStrCode hlen = ending <|> topEntryPoint where
    ending = do
        qs <- takeMany1 natStrQuote
        when (Text.length qs /= hlen) $ fail "Not an ending"
        Block End <$ unliftEntry
{-# INLINE fmtStrCode #-}


-- Escape maps

esc1Map, esc2Map, esc3Map :: Map String Int
esc1Map = Char.ord <$> fromList [ ("a", '\a'), ("b", '\b'), ("f", '\f'), ("n", '\n'), ("r", '\r'), ("t", '\t'), ("v", '\v') ]
esc2Map = Char.ord <$> fromList [ ("BS", '\BS'), ("HT", '\HT'), ("LF", '\LF'), ("VT", '\VT'), ("FF", '\FF'), ("CR", '\CR'), ("SO", '\SO'), ("SI", '\SI'), ("EM", '\EM'), ("FS", '\FS'), ("GS", '\GS'), ("RS", '\RS'), ("US", '\US'), ("SP", '\SP') ]
esc3Map = Char.ord <$> fromList [ ("NUL", '\NUL'), ("SOH", '\SOH'), ("STX", '\STX'), ("ETX", '\ETX'), ("EOT", '\EOT'), ("ENQ", '\ENQ'), ("ACK", '\ACK'), ("BEL", '\BEL'), ("DLE", '\DLE'), ("DC1", '\DC1'), ("DC2", '\DC2'), ("DC3", '\DC3'), ("DC4", '\DC4'), ("NAK", '\NAK'), ("SYN", '\SYN'), ("ETB", '\ETB'), ("CAN", '\CAN'), ("SUB", '\SUB'), ("ESC", '\ESC'), ("DEL", '\DEL') ]

lexEscSeq :: Lexer
lexEscSeq = numEsc <|> chrEcs <|> wrongEsc where
    numEsc   = StrEsc . NumStrEsc . read . convert <$> takeWhile1 isDecDigitChar
    chrEcs   = choice $ uncurry parseEsc <$> zip [1..] [esc1Map, esc2Map, esc3Map]
    wrongEsc = StrWrongEsc . Char.ord <$> anyToken

parseEsc :: Int -> Map String Int -> Lexer
parseEsc n m = do
    s <- count n anyToken
    case Map.lookup s m of
        Just i  -> return . StrEsc $ CharStrEsc i
        Nothing -> fail "Escape not matched"



--------------------
-- === Config === --
--------------------

markerBeginChar, markerEndChar :: Char
markerBeginChar = '«' ; {-# INLINE markerBeginChar #-}
markerEndChar   = '»' ; {-# INLINE markerEndChar   #-}

metadataHeader :: IsString s => s
metadataHeader = "META" ; {-# INLINE metadataHeader #-}

mkMetadata :: (IsString s, Semigroup s) => s -> s
mkMetadata s = "### " <> metadataHeader <> s ; {-# INLINE mkMetadata #-}

lexConfig :: Lexer
lexConfig = takeMany ' ' *> (lexMetadata {- <|> lexPragma -}) ; {-# INLINE lexConfig #-}

lexMetadata :: Lexer
lexMetadata = Metadata <$ tokens metadataHeader <* takeMany1 ' ' <*> takeLine ; {-# INLINE lexMetadata #-}

lexComment :: Lexer
lexComment = Doc <$> takeLine ; {-# INLINE lexComment #-}

lexMarker :: Lexer
lexMarker = token markerBeginChar *> (correct <|> incorrect) <* token markerEndChar where
    incorrect = Incorrect . ("Marker " <>) <$> takeTill (== markerEndChar)
    correct   = Marker . read . convert <$> takeWhile1 isDecDigitChar
{-# INLINE lexMarker #-}



-----------------------
-- === Operators === --
-----------------------

regularOperatorChars :: [Char]
regularOperatorChars = "!$%&*+-/<>?^~\\" ; {-# INLINE regularOperatorChars #-}

isRegularOperatorChar :: Char -> Bool
isRegularOperatorChar = (`elem` regularOperatorChars) ; {-# INLINE isRegularOperatorChar #-}

isOperator :: Convertible' s String => s -> Bool
isOperator = test . convertTo' @String where
    test s = all isRegularOperatorChar s
          || s `elem` [",", "..", "...", "=="]
          || (maybeLast s == Just '=') && isOperator (init s)
{-# INLINE isOperator #-}



---------------------------
-- === Top level map === --
---------------------------

-- === Symbol head char map === --

-- | The size of `symmap` - Vector-based map from head Char to related parser.
symmapSize :: Int
symmapSize = 200 ; {-# INLINE symmapSize #-}

symmap :: Vector (Lexer)
symmap = Vector.generate symmapSize $ \i -> let c = Char.chr i in if

    -- Layouting
    | c == ';'          -> Terminator  <$ dropToken
    | c == '{'          -> Block Begin <$ dropToken
    | c == '}'          -> Block End   <$ dropToken
    | c == '('          -> Group Begin <$ dropToken
    | c == ')'          -> Group End   <$ dropToken
    | c == '\n'         -> EOL         <$ dropToken
    | c == '\r'         -> EOL         <$ dropToken <* option_ (token '\n')
    | c == ':'          -> handleColons =<< takeMany ':'
    | c == markerBeginChar  -> lexMarker

    -- Identifiers & Keywords
    | varHead  c        -> checkSpecialVar <$> varBody
    | consHead c        -> Cons            <$> consBody

    -- Operators
    | c == '@'          -> TypeApp <$ dropToken
    | c == '|'          -> Merge   <$ dropToken
    | c == '.'          -> handleDots =<< takeMany '.'
    | c == '='          -> handleEqs  =<< takeMany '='
    | c `elem` opChars  -> handleOp <$> takeWhile1 isRegularOperatorChar <*> takeMany '='

    -- Literals
    | c == '['          -> List Begin   <$ dropToken
    | c == ']'          -> List End     <$ dropToken
    | c == ','          -> Operator "," <$ dropToken
    | c == rawStrQuote  -> rawStr
    | c == fmtStrQuote  -> fmtStr
    | c == natStrQuote  -> natStr
    | decHead c         -> lexNumber

    -- Meta
    | c == '#'          -> handleHash =<< takeMany '#'

    -- Utils
    | otherwise         -> dropToken >> unknownCharSym c

    where between  a l r    = a >= l && a <= r
          decHead  c        = between c '0' '9'
          varHead  c        = between c 'a' 'z' || c == '_'
          consHead c        = between c 'A' 'Z'
          consBody          = indentBaseBody
          varBody           = indentBaseBody <**> (option id $ flip Text.snoc <$> (token '?' <|> token '!'))
                                             <**> (option id $ flip (<>)      <$> takeMany1 '\'')
          indentBaseBody    = takeWhile isIndentBodyChar
          handleColons      = handleReps  [BlockStart, Typed]
          handleDots        = handleReps  [Accessor  , Range, Anything]
          handleEqs         = handleReps  [Assignment, Operator "=="]
          handleHash        = handleRepsM [pure Disable, lexComment, lexConfig]
          handleReps        = handleRepsM . fmap pure
          handleRepsM ts s  = fromMaybe (return $ Unknown s) $ ts ^? ix (Text.length s - 1)
          handleOp    op    = \case "="  -> Modifier op
                                    ""   -> Operator op
                                    s    -> Unknown (op <> s)
{-# NOINLINE symmap #-}


-- -- === Utils === --

unknownCharSym :: Char -> Lexer
unknownCharSym = pure . Unknown . convert ; {-# INLINE unknownCharSym #-}



-----------------------------------
-- === Top level combinators === --
-----------------------------------

lexer     :: Parser (Symbol, Int)
lexerCont :: Parser ((Symbol, EntryStack), Int)
lexer     = lexeme =<< lexEntryPoint ; {-# INLINE lexer #-}
lexerCont = do
    s         <- lexEntryPoint
    (s', off) <- lexeme s
    es        <- get @EntryStack
    return ((s', es), off)

lexEntryPoint :: Lexer
lexEntryPoint = getEntryPoint >>= \case
    TopLevelEntry  -> topEntryPoint
    StrCodeEntry i -> fmtStrCode i
    StrEntry   t i -> case t of
        RawStr -> rawStrBody i
        FmtStr -> fmtStrBody i
        NatStr -> natStrBody i
{-# INLINE lexEntryPoint #-}

topEntryPoint :: Lexer
topEntryPoint = peekToken >>= lexSymChar ; {-# INLINE topEntryPoint #-}

lexSymChar :: Char -> Lexer
lexSymChar c = if chord < symmapSize then Vector.unsafeIndex symmap chord else unknownCharSym c
    where chord = Char.ord c
{-# INLINE lexSymChar #-}

lexeme :: Symbol -> Parser (Symbol, Int)
lexeme s = case s of
    Quote _ Begin -> return (s, 0) -- Do not include whitespaces as offset inside text.
    Block End     -> return (s, 0) -- Do not include whitespaces as offset after inline text code.
    _             -> (s,) <$> spacing
{-# INLINE lexeme #-}

spacing :: Parser Int
spacing = Text.length <$> takeMany ' ' ; {-# INLINE spacing #-}



---------------------
-- === Running === --
---------------------

fromLexerResult :: Either ParseError a -> a
fromLexerResult = either (error . ("Impossible happened: lexer error: " <>) . show) id ; {-# INLINE fromLexerResult #-}

-- FIXME[WD]: concatenating STX to the end of list could be slow
parseBase :: (Monad m, IsSourceBorder t) => Parser (t, Int) -> EntryStack -> ConduitM a Text m () -> ConduitM a c0 m [Either ParseError (Token t)]
parseBase p s f = fmap (<> [etx]) $ f .| prependSTX (conduitParserEither s $ runStateT @EntryStack p) .| sinkList ; {-# INLINE parseBase #-}

parse        :: IsSourceBorder a =>              Parser (a, Int) -> EntryStack -> Text     ->   [Token a]
tryParse     :: IsSourceBorder a =>              Parser (a, Int) -> EntryStack -> Text     ->   Either ParseError [Token a]
parseFile    :: IsSourceBorder a => MonadIO m => Parser (a, Int) -> EntryStack -> FilePath -> m [Token a]
tryParseFile :: IsSourceBorder a => MonadIO m => Parser (a, Int) -> EntryStack -> FilePath -> m (Either ParseError [Token a])
parse              = fromLexerResult .:.   tryParse                                          ; {-# INLINE parse        #-}
parseFile          = fromLexerResult <∘∘∘> tryParseFile                                      ; {-# INLINE parseFile    #-}
tryParse     p s t = sequence . runConduitPure $ parseBase p s (sourceProducer t)            ; {-# INLINE tryParse     #-}
tryParseFile p s t = liftIO . fmap sequence . runConduitRes $ parseBase p s (sourceReader t) ; {-# INLINE tryParseFile #-}

runLexer     :: EntryStack -> Text -> [Token (Symbol, EntryStack)]
evalLexer    :: EntryStack -> Text -> [Token Symbol]
evalDefLexer ::               Text -> [Token Symbol]
runLexer     = parse lexerCont ; {-# INLINE runLexer     #-}
evalLexer    = parse lexer     ; {-# INLINE evalLexer    #-}
evalDefLexer = evalLexer def   ; {-# INLINE evalDefLexer #-}

parsePrim :: Text32 -> Either String [(Symbol, Int)]
parsePrim t = Parsec.parseOnly (flip (evalStateT @EntryStack) def (many lexer)) t ; {-# INLINE parsePrim #-}


-- === STX / ETX handling === --

prependSTX :: (Monad m, IsSourceBorder s) => ConduitM Text s m () -> ConduitM Text s m ()
prependSTX f = await >>= \case
    Nothing -> return ()
    Just t  -> yield (stx $ Text.length s) >> when (not $ Text.null t') (leftover t') >> f where
        (s,t') = Text.span (== ' ') t
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
    etx   = (etx, mempty) ; {-# INLINE etx #-}
