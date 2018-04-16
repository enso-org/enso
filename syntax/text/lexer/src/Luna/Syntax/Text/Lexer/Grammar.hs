{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE UndecidableInstances      #-}
-- {-# LANGUAGE Strict                    #-}

module Luna.Syntax.Text.Lexer.Grammar where

import Prologue hiding (Symbol, Text, Type, catch, range, span, takeWhile)

import qualified Control.Monad.State.Layered   as State
import qualified Data.Attoparsec.Text32        as Parsec
import qualified Data.Char                     as Char
import qualified Data.Map.Strict               as Map
import qualified Data.Text32                   as Text32
import qualified Data.Vector                   as Vector
import qualified Luna.Syntax.Text.Lexer.Symbol as Symbol

import Control.Monad.State.Layered      (StateT)
import Data.Map.Strict                  (Map)
import Data.Parser                      hiding (Token)
import Data.Parser.Instances.Attoparsec ()
import Data.Vector                      (Vector)
import Luna.Syntax.Text.Lexer.Symbol    (Symbol)


-------------------------
-- === Lexer types === --
-------------------------

type Parser = StateT EntryStack Parsec.Parser
type Lexer  = Parser Symbol


------------------------
-- === EntryStack === --
------------------------

-- === Definition === --

data EntryPoint
   = TopLevelEntry
   | StrEntry      !Symbol.StrType !Int
   | StrCodeEntry  !Int
   deriving (Generic, Show)

type EntryStack = [EntryPoint]


-- === Utils === --

liftEntry :: State.Monad EntryStack m => EntryPoint -> m ()
liftEntry = State.modify_ @EntryStack . (:) ; {-# INLINE liftEntry #-}

unliftEntry :: State.Monad EntryStack m => m ()
unliftEntry = State.modify_ @EntryStack $ \lst -> case tail lst of
    Just p  -> p
    Nothing -> error "Trying to unlift global lexer entry."
{-# INLINE unliftEntry #-}

getEntryPoint :: State.Monad EntryStack m => m EntryPoint
getEntryPoint = maybe def id . head <$> State.get @EntryStack ; {-# INLINE getEntryPoint #-}


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

isDecDigitChar, isOctDigitChar, isBinDigitChar, isHexDigitChar, isIdentBodyChar,
    isVarHead, isConsHead :: Char -> Bool
isIdentBodyChar c = Char.isAlphaNum c || c == '_' ; {-# INLINE isIdentBodyChar  #-}
isVarHead       c = Char.isLower c || c == '_'    ; {-# INLINE isVarHead        #-}
isConsHead      c = Char.isUpper c                ; {-# INLINE isConsHead       #-}
isDecDigitChar  c = (c >= '0' && c <= '9')        ; {-# INLINE isDecDigitChar   #-}
isOctDigitChar  c = (c >= '0' && c <= '7')        ; {-# INLINE isOctDigitChar   #-}
isBinDigitChar  c = (c == '0' || c == '1')        ; {-# INLINE isBinDigitChar   #-}
isHexDigitChar  c = isDecDigitChar c
                 || (c >= 'a' && c <= 'f')
                 || (c >= 'A' && c <= 'F')
{-# INLINE isHexDigitChar   #-}

opChars :: [Char]
opChars = "!$%&*+-/<>?^~\\" ; {-# INLINE opChars #-}


-- === Names === --

lexVariable :: Lexer
lexVariable = Symbol.checkSpecialVar <$> lexName where
    lexName = takeWhile isIdentBodyChar
         <**> option id (flip Text32.snoc <$> (token '?' <|> token '!'))
         <**> option id (flip (<>)        <$> takeMany1 '\'')
{-# INLINE lexVariable #-}

lexConstructor :: Lexer
lexConstructor = Symbol.Cons <$> takeWhile isIdentBodyChar
{-# INLINE lexConstructor #-}


-- === Numbers === --

lexNumber :: Lexer
lexNumber = check =<< number where
    number  = (token '0' *> choice [hex, oct, bin]) <|> dec
    n       = Symbol.NumRep
    dec     = n Symbol.Dec           <$> decBody <*> fracSfx     <*> expSfx
    hex     = n Symbol.Hex <$ hexPfx <*> hexBody <*> pure mempty <*> pure mempty
    oct     = n Symbol.Oct <$ octPfx <*> octBody <*> pure mempty <*> pure mempty
    bin     = n Symbol.Bin <$ binPfx <*> binBody <*> pure mempty <*> pure mempty
    hexPfx  = satisfy (\s -> s == 'x' || s == 'X')
    octPfx  = satisfy (\s -> s == 'o' || s == 'O')
    binPfx  = satisfy (\s -> s == 'b' || s == 'B')
    decBody = takeWhile1 isDecDigitChar
    hexBody = takeWhile1 isHexDigitChar
    octBody = takeWhile1 isOctDigitChar
    binBody = takeWhile1 isBinDigitChar
    expBody = maybe id Text32.cons <$> optional sign <*> decBody
    fracSfx = option mempty $ token '.' *> decBody
    expSfx  = option mempty $ (token 'e' <|> token 'E') *> expBody
    sign    = satisfy (\s -> s == '-' || s == '+')
    check n = option (Symbol.Number n)
            $ (\s -> Symbol.Incorrect $ "Unexpected characters '" <> s
                                     <> "' found on the end of number literal"
              ) <$> takeWhile1 Char.isAlphaNum
{-# INLINE lexNumber #-}


-- === String parsing utils === --

beginStr :: Symbol.StrType -> Char -> Lexer
beginStr t c = Symbol.Quote t Symbol.Begin
            <$ (liftEntry . StrEntry t =<< beginQuotes c) ; {-# INLINE beginStr #-}

beginQuotes :: Char -> Parser Int
beginQuotes !c = beginMultiQuotes c <|> (1 <$ token c) ; {-# INLINE beginQuotes #-}

beginMultiQuotes :: Char -> Parser Int
beginMultiQuotes !c = do
    !len <- Text32.length <$> takeMany1 c
    when_ (len == 2) $ fail "Empty string"
    pure len
{-# INLINE beginMultiQuotes #-}


-- === Top level string parsers === --

natStrQuote, rawStrQuote, fmtStrQuote, escapeChar :: Char
natStrQuote = '`'  ; {-# INLINE natStrQuote #-}
rawStrQuote = '"'  ; {-# INLINE rawStrQuote #-}
fmtStrQuote = '\'' ; {-# INLINE fmtStrQuote #-}
escapeChar  = '\\' ; {-# INLINE escapeChar  #-}

natStr, rawStr, fmtStr :: Lexer
natStr = beginStr Symbol.NatStr natStrQuote ; {-# INLINE natStr #-}
rawStr = beginStr Symbol.RawStr rawStrQuote ; {-# INLINE rawStr #-}
fmtStr = beginStr Symbol.FmtStr fmtStrQuote ; {-# INLINE fmtStr #-}


-- === Native String === --

natStrBody :: Int -> Lexer
natStrBody hlen = code <|> quotes where
    code   = Symbol.Str <$> takeWhile1 (/= natStrQuote)
    quotes = do
        qs <- takeMany1 natStrQuote
        if Text32.length qs == hlen
            then Symbol.Quote Symbol.NatStr Symbol.End <$ unliftEntry
            else pure $ Symbol.Str qs
{-# INLINE natStrBody #-}


-- === Raw String === --

rawStrBody :: Int -> Lexer
rawStrBody hlen = choice [body, escape, quotes, linebr] where
    bodyCheck c = c /= rawStrQuote && notNewlineStart c && c /= escapeChar
    body        = Symbol.Str <$> takeWhile1 bodyCheck
    linebr      = Symbol.EOL <$ newline
    escape      = token escapeChar *> option (Symbol.Str $ convert escapeChar)
                                             (Symbol.StrEsc <$> esct)
    esct        = (Symbol.SlashEsc <$ token escapeChar)
              <|> (Symbol.QuoteEscape Symbol.RawStr <$ token rawStrQuote)
    quotes      = do
        qs <- takeMany1 rawStrQuote
        if Text32.length qs == hlen
            then Symbol.Quote Symbol.RawStr Symbol.End <$ unliftEntry
            else pure $ Symbol.Str qs
{-# INLINE rawStrBody #-}


-- === Fmt String === --

fmtStrBody :: Int -> Lexer
fmtStrBody hlen = choice [body, escape, quotes, linebr, code] where
    bodyCheck c = c /= fmtStrQuote
               && c /= natStrQuote
               && c /= escapeChar
               && notNewlineStart c
    body        = Symbol.Str <$> takeWhile1 bodyCheck
    linebr      = Symbol.EOL <$  newline
    escape      = token escapeChar *> esct
    rawQEsc     = Symbol.QuoteEscape Symbol.RawStr
    fmtQEsc     = Symbol.QuoteEscape Symbol.FmtStr
    esct        = (Symbol.StrEsc Symbol.SlashEsc <$ token escapeChar)
              <|> (Symbol.StrEsc rawQEsc         <$ token rawStrQuote)
              <|> (Symbol.StrEsc fmtQEsc         <$ token fmtStrQuote)
              <|> lexEscSeq
    quotes      = do
        qs <- takeMany1 fmtStrQuote
        if Text32.length qs == hlen
            then Symbol.Quote Symbol.FmtStr Symbol.End <$ unliftEntry
            else pure $ Symbol.Str qs
    code        = Symbol.Block Symbol.Begin
               <$ (liftEntry . StrCodeEntry =<< beginQuotes natStrQuote)
{-# INLINE fmtStrBody #-}

fmtStrCode :: Int -> Lexer
fmtStrCode hlen = ending <|> topEntryPoint where
    ending = do
        qs <- takeMany1 natStrQuote
        when_ (Text32.length qs /= hlen) $ fail "Not an ending"
        Symbol.Block Symbol.End <$ unliftEntry
{-# INLINE fmtStrCode #-}


-- === Escape maps === --

esc1Map :: Map String Int
esc1Map = Char.ord <$> fromList
    [ ("a", '\a'), ("b", '\b'), ("f", '\f'), ("n", '\n'), ("r", '\r')
    , ("t", '\t'), ("v", '\v')
    ]

esc2Map :: Map String Int
esc2Map = Char.ord <$> fromList
    [ ("BS", '\BS'), ("HT", '\HT'), ("LF", '\LF'), ("VT", '\VT'), ("FF", '\FF')
    , ("CR", '\CR'), ("SO", '\SO'), ("SI", '\SI'), ("EM", '\EM'), ("FS", '\FS')
    , ("GS", '\GS'), ("RS", '\RS'), ("US", '\US'), ("SP", '\SP')
    ]

esc3Map :: Map String Int
esc3Map = Char.ord <$> fromList
    [ ("NUL", '\NUL'), ("SOH", '\SOH'), ("STX", '\STX'), ("ETX", '\ETX')
    , ("EOT", '\EOT'), ("ENQ", '\ENQ'), ("ACK", '\ACK'), ("BEL", '\BEL')
    , ("DLE", '\DLE'), ("DC1", '\DC1'), ("DC2", '\DC2'), ("DC3", '\DC3')
    , ("DC4", '\DC4'), ("NAK", '\NAK'), ("SYN", '\SYN'), ("ETB", '\ETB')
    , ("CAN", '\CAN'), ("SUB", '\SUB'), ("ESC", '\ESC'), ("DEL", '\DEL')
    ]

lexEscSeq :: Lexer
lexEscSeq = numEsc <|> chrEcs <|> badEsc where
    numEsc = Symbol.StrEsc . Symbol.NumStrEsc . unsafeRead . convert
         <$> takeWhile1 isDecDigitChar
    chrEcs = choice $ uncurry parseEsc <$> zip [1..] [esc1Map, esc2Map, esc3Map]
    badEsc = Symbol.StrWrongEsc . Char.ord <$> anyToken

parseEsc :: Int -> Map String Int -> Lexer
parseEsc n m = do
    s <- count n anyToken
    case Map.lookup s m of
        Just i  -> pure . Symbol.StrEsc $ Symbol.CharStrEsc i
        Nothing -> fail "Escape not matched"



--------------------
-- === Config === --
--------------------

markerBegin, markerEnd :: Char
markerBegin = '«' ; {-# INLINE markerBegin #-}
markerEnd   = '»' ; {-# INLINE markerEnd   #-}

metadataHeader :: IsString s => s
metadataHeader = "META" ; {-# INLINE metadataHeader #-}

mkMetadata :: (IsString s, Semigroup s) => s -> s
mkMetadata s = "### " <> metadataHeader <> s ; {-# INLINE mkMetadata #-}

lexConfig :: Lexer
lexConfig = takeMany ' ' *> lexMetadata ; {-# INLINE lexConfig #-}

lexMetadata :: Lexer
lexMetadata = Symbol.Metadata <$  tokens metadataHeader
                              <*  takeMany1 ' '
                              <*> takeLine
{-# INLINE lexMetadata #-}

lexComment :: Lexer
lexComment = Symbol.Doc <$> takeLine ; {-# INLINE lexComment #-}

lexMarker :: Lexer
lexMarker = token markerBegin *> (correct <|> incorrect) <* token markerEnd
    where incorrect = Symbol.Incorrect . ("Marker " <>)
                  <$> takeTill (== markerEnd)
          correct   = Symbol.Marker . unsafeRead . convert
                  <$> takeWhile1 isDecDigitChar
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
          || (last s == Just '=') && isOperator (unsafeInit s)
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
    | c == ';'          -> Symbol.Terminator  <$ dropToken
    | c == '{'          -> Symbol.Block Symbol.Begin <$ dropToken
    | c == '}'          -> Symbol.Block Symbol.End   <$ dropToken
    | c == '('          -> Symbol.Group Symbol.Begin <$ dropToken
    | c == ')'          -> Symbol.Group Symbol.End   <$ dropToken
    | c == '\n'         -> Symbol.EOL <$ dropToken
    | c == '\r'         -> Symbol.EOL <$ dropToken <* option_ (token '\n')
    | c == ':'          -> handleColons =<< takeMany ':'
    | c == markerBegin  -> lexMarker

    -- Identifiers & Keywords
    | isVarHead  c      -> lexVariable
    | isConsHead c      -> lexConstructor

    -- Operators
    | c == '@'          -> Symbol.TypeApp <$ dropToken
    | c == '|'          -> Symbol.Merge   <$ dropToken
    | c == '.'          -> handleDots =<< takeMany '.'
    | c == '='          -> handleEqs  =<< takeMany '='
    | c `elem` opChars  -> handleOp <$> takeWhile1 isRegularOperatorChar
                                    <*> takeMany '='

    -- Literals
    | c == '['          -> Symbol.List Symbol.Begin   <$ dropToken
    | c == ']'          -> Symbol.List Symbol.End     <$ dropToken
    | c == ','          -> Symbol.Operator "," <$ dropToken
    | c == rawStrQuote  -> rawStr
    | c == fmtStrQuote  -> fmtStr
    | c == natStrQuote  -> natStr
    | isDecDigitChar c  -> lexNumber

    -- Meta
    | c == '#'          -> handleHash =<< takeMany '#'

    -- Utils
    | otherwise         -> unknownCharSym c

    where
    handleColons = handleReps  [ Symbol.BlockStart, Symbol.Typed]
    handleDots   = handleReps  [ Symbol.Accessor, Symbol.Range, Symbol.Anything]
    handleEqs    = handleReps  [ Symbol.Assignment, Symbol.Operator "=="]
    handleHash   = handleRepsM [ lexComment, pure Symbol.Disable, lexConfig]
    handleReps   = handleRepsM . fmap pure
    handleRepsM  = \ts s -> fromJust (pure $ Symbol.Unknown s)
                 $ ts ^? ix (Text32.length s - 1)
    handleOp p s = if (p == "<" || p == ">") && s == "="
                       then Symbol.Operator (p <> s)
                       else case s of
                           "=" -> Symbol.Modifier p
                           ""  -> Symbol.Operator p
                           s   -> Symbol.Unknown (p <> s)
{-# NOINLINE symmap #-}


-- -- === Utils === --

unknownCharSym :: Char -> Lexer
unknownCharSym c = dropToken >> pure (Symbol.Unknown $ convert c) ; {-# INLINE unknownCharSym #-}



-----------------------------------
-- === Top level combinators === --
-----------------------------------

lexer     :: Parser (Symbol, Int)
lexerCont :: Parser ((Symbol, EntryStack), Int)
lexer     = lexeme =<< lexEntryPoint ; {-# INLINE lexer #-}
lexerCont = do
    s         <- lexEntryPoint
    (s', off) <- lexeme s
    es        <- State.get @EntryStack
    pure ((s', es), off)
{-# INLINE lexerCont #-}

lexEntryPoint :: Lexer
lexEntryPoint = getEntryPoint >>= \case
    TopLevelEntry  -> topEntryPoint
    StrCodeEntry i -> fmtStrCode i
    StrEntry   t i -> case t of
        Symbol.RawStr -> rawStrBody i
        Symbol.FmtStr -> fmtStrBody i
        Symbol.NatStr -> natStrBody i
{-# INLINE lexEntryPoint #-}

topEntryPoint :: Lexer
topEntryPoint = peekToken >>= lexSymChar ; {-# INLINE topEntryPoint #-}

-- | (1): fetch  lexers for ASCII from precomputed cache
--   (2): create lexers for unicode names on the fly
lexSymChar :: Char -> Lexer
lexSymChar c
  | chord < symmapSize = Vector.unsafeIndex symmap chord -- (1)
  | isVarHead  c       = lexVariable                     -- (2)
  | isConsHead c       = lexConstructor
  | otherwise          = unknownCharSym c
  where chord = Char.ord c
{-# INLINE lexSymChar #-}

-- | (1): Do not include whitespaces as offset inside text.
--   (2): Do not include whitespaces as offset after inline text code.
lexeme :: Symbol -> Parser (Symbol, Int)
lexeme s = case s of
    Symbol.Quote _ Symbol.Begin -> pure (s, 0) -- (1)
    Symbol.Block Symbol.End     -> pure (s, 0) -- (2)
    _                           -> (s,) <$> spacing
{-# INLINE lexeme #-}

spacing :: Parser Int
spacing = Text32.length <$> takeMany ' ' ; {-# INLINE spacing #-}
