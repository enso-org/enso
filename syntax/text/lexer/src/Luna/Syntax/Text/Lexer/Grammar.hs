{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Text.Lexer.Grammar where

import Prologue hiding (Symbol, Text, Type, break, catch, range, span,
                 takeWhile)

import qualified Control.Monad.State.Layered    as State
import qualified Data.Attoparsec.Internal.Types as AttoParsec
import qualified Data.Attoparsec.Text32         as Parsec
import qualified Data.Char                      as Char
import qualified Data.Map.Strict                as Map
import qualified Data.Text32                    as Txt
import qualified Data.Vector                    as Vector
import qualified Luna.IR.Term.Ast.Invalid       as Invalid
import qualified Luna.Syntax.Text.Lexer.Symbol  as Symbol
import qualified Text.Parser.State.Indent       as Indent

import Control.Monad.State.Layered      (StatesT)
import Data.Map.Strict                  (Map)
import Data.Parser.Instances.Attoparsec ()
import Data.Text.Position               (Delta)
import Data.Vector                      (Vector)
import Luna.Syntax.Text.Lexer.Symbol    (Symbol, symbol)
import Luna.Syntax.Text.Lexer.Token     (Token (Token), TokenInfo (TokenInfo))
import Text.Parser.State.Indent         (Indent)

import Data.Parser hiding (Result, Token, endOfInput)

type Txt = Txt.Text32


endOfInput = peekToken >>= \case
    '\ETX' -> pure ()
    _      -> fail "Not ETX."

loopUnless :: (Monad m, Alternative m) => m a -> m b -> m b
loopUnless f g = go where
    go = (f >> go) <|> g
{-# INLINE loopUnless #-}

----------------------
-- === Location === --
----------------------

-- === Definition === --

data Location = Location
    { _offset     :: Delta
    , _column     :: Delta
    , _row        :: Delta
    , _isTopLevel :: Bool
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''Location


-- === Utils === --

getOffset, getColumn, getRow :: State.Monad Location m => m Delta
getOffset = view offset <$> State.get @Location
getColumn = view column <$> State.get @Location
getRow    = view row    <$> State.get @Location
{-# INLINE getOffset #-}
{-# INLINE getColumn #-}
{-# INLINE getRow    #-}

putOffset, putColumn, putRow :: State.Monad Location m => Delta -> m ()
putOffset = \a -> State.modify_ @Location $ offset .~ a
putColumn = \a -> State.modify_ @Location $ column .~ a
putRow    = \a -> State.modify_ @Location $ row    .~ a
{-# INLINE putOffset #-}
{-# INLINE putColumn #-}
{-# INLINE putRow    #-}

getIsTopLevel :: State.Monad Location m => m Bool
putIsTopLevel :: State.Monad Location m => Bool -> m ()
getIsTopLevel = view isTopLevel <$> State.get @Location
putIsTopLevel = \a -> State.modify_ @Location $ isTopLevel .~ a
{-# INLINE getIsTopLevel #-}
{-# INLINE putIsTopLevel #-}


-- === Instances === --

instance NFData Location
instance Mempty Location where
    mempty = Location 0 0 0 True
    {-# INLINE mempty #-}



-------------------------
-- === Lexer types === --
-------------------------

type Parser = StatesT '[Result, Indent, Location] Parsec.Parser
type Lexer  = Parser ()

getParsecPos :: AttoParsec.Parser t Delta
getParsecPos = AttoParsec.Parser $ \t pos more _ succ ->
    succ t pos more (convert $! AttoParsec.fromPos pos)
{-# INLINE getParsecPos #-}

getParsecPos' :: Parser Delta
getParsecPos' = lift $ lift $ lift getParsecPos
{-# INLINE getParsecPos' #-}


newtype Result = Result [Token] deriving (Mempty, Show)
makeLenses ''Result

addSymbol :: Symbol -> Parser ()
addSymbol = \symbol -> do
    col      <- getColumn
    row      <- getRow
    off      <- getParsecPos'
    oldOff   <- getOffset
    spacing  <- lexeme
    topLevel <- getIsTopLevel
    let span      = off - oldOff
        totalSpan = span + spacing
        token     = Token (TokenInfo span spacing col row topLevel) symbol
        (!col', !row') = if symbol == Symbol.EOL
            then (spacing, row + 1)
            else (col + totalSpan, row)
    putOffset     (off + spacing)
    putColumn     col'
    putRow        row'
    putIsTopLevel False
    State.modify_ @Result $ wrapped %~ (token :)
{-# INLINE addSymbol #-}

getLastSymbol :: Parser Symbol
getLastSymbol = (unwrap <$> State.get @Result) >>= \case
    (t : _) -> pure $ t ^. symbol
    _       -> error "Panic. Unable to get last lexer symbol."
{-# INLINE getLastSymbol #-}

lexeme :: Parser Delta
lexeme = convert . Txt.length <$> whiteSpace
{-# INLINE lexeme #-}

whiteSpace :: Parser Txt
whiteSpace = takeMany ' '
{-# INLINE whiteSpace #-}



-----------------------
-- === Layouting === --
-----------------------

newlineStartChar :: Char -> Bool
newlineStartChar = \c -> c == '\n' || c == '\r'
{-# INLINE newlineStartChar #-}

newlineStartChars :: [Char]
newlineStartChars = ['\n', '\r']
{-# INLINE newlineStartChars #-}



-------------------------
-- === Identifiers === --
-------------------------

-- === Char by char checking === --

isIdentBodyChar, isVarHead, isConsHead :: Char -> Bool
isIdentBodyChar = Char.isAlphaNum
isVarHead       = Char.isLower
isConsHead      = Char.isUpper
{-# INLINE isIdentBodyChar  #-}
{-# INLINE isVarHead        #-}
{-# INLINE isConsHead       #-}

tokenBreakingChars :: [Char]
tokenBreakingChars = "`!@#$%^&*()-=+[]{}\\|;:<>,./ \t\n\ETX"
{-# INLINE tokenBreakingChars #-}


-- === Names === --

invalidSuffix :: Parser Symbol
invalidSuffix = Symbol.Invalid . Invalid.UnexpectedSuffix . Txt.length
            <$> takeWhile1 (`notElem` tokenBreakingChars)
{-# INLINE invalidSuffix #-}

checkInvalidSuffix :: Parser Symbol -> Parser Symbol
checkInvalidSuffix =  (<**> option id (const <$> invalidSuffix))
{-# INLINE checkInvalidSuffix #-}

lexWildcard :: Lexer
lexWildcard = do
    sym <- checkInvalidSuffix $ Symbol.Wildcard <$ token '_'
    addSymbol sym
{-# INLINE lexWildcard #-}

lexVariable :: Lexer
lexVariable = addSymbol =<< checkInvalidSuffix validVar where
    validVar   = Symbol.checkSpecialVar <$> validName
    validName  = takeWhile isIdentBodyChar
          <**> option id (flip Txt.snoc <$> token '?')
          <**> option id (flip (<>)     <$> takeMany1 '\'')
{-# INLINE lexVariable #-}

lexConstructor :: Lexer
lexConstructor = do
    sym <- checkInvalidSuffix $ Symbol.Cons <$> takeWhile isIdentBodyChar
    addSymbol sym
{-# INLINE lexConstructor #-}


-- === Invalid names === --

-- | WARNING! We assume that we have already checked for valid headers before
--   using this check!
isInvalidVarHead :: Char -> Bool
isInvalidVarHead = Char.isAlpha ; {-# INLINE isInvalidVarHead #-}

-- | WARNING! We assume that we have already checked for valid headers before
--   using this check!
lexInvalidVariable :: Lexer
lexInvalidVariable = do
    _ <- takeWhile isIdentBodyChar
    addSymbol $ Symbol.Invalid Invalid.CaselessNameHead
{-# INLINE lexInvalidVariable #-}



---------------------
-- === Numbers === --
---------------------

isDigitCharAtBase :: Word8 -> Char -> Bool
isDigitCharAtBase = \base char -> case charToDigit char of
    Just n  -> n < base
    Nothing -> False
{-# INLINE isDigitCharAtBase #-}

isDecDigitChar :: Char -> Bool
isDecDigitChar = isDigitCharAtBase 10 ; {-# INLINE isDecDigitChar #-}

charToDigit :: Char -> Maybe Word8
charToDigit = \char -> let
    n = Char.ord char
    in unsafeConvert <$> if
        | n >= 48 && n <= 57  -> Just $ n - 48      -- 0 to 9
        | n >= 65 && n <= 90  -> Just $ n - 65 + 10 -- A to Z
        | n >= 97 && n <= 122 -> Just $ n - 97 + 10 -- a to z
        | otherwise           -> Nothing
{-# INLINE charToDigit #-}

unsafeCharToDigit :: Char -> Word8
unsafeCharToDigit = \c -> case charToDigit c of
    Just t  -> t
    Nothing -> error $ "Cannot convert char " <> [c] <> " to digit."
{-# INLINE unsafeCharToDigit #-}

lexNumber :: Lexer
lexNumber = addSymbol =<< checkInvalidSuffix number where
    number  = Symbol.Number <$> (special <|> dec)
    special = token '0' *> choice [p0 'x' 16, p0 'o' 8, p0 'b' 2]
    dec     = Symbol.NumRep 10            <$> body 10 <*> frac 10
    p0 s n  = Symbol.NumRep n  <$ token s <*> body n  <*> frac n
    body n  = Txt.toList . Txt.map unsafeCharToDigit
          <$> takeWhile1 (isDigitCharAtBase n)
    frac  n = option mempty $ token '.' *> body n
{-# INLINE lexNumber #-}



-----------------------------
-- === String literals === --
-----------------------------

-- === String parsing utils === --

beginStr :: Symbol.StrType -> Char -> (Int -> Lexer) -> Lexer
beginStr = \t c p -> do
    quotNum <- beginQuotes c
    addSymbol $ Symbol.Quote t Symbol.Begin
    p quotNum
{-# INLINE beginStr #-}

beginQuotes :: Char -> Parser Int
beginQuotes = \c -> beginMultiQuotes c <|> (1 <$ token c)
{-# INLINE beginQuotes #-}

beginMultiQuotes :: Char -> Parser Int
beginMultiQuotes = \c -> do
    len <- Txt.length <$> takeMany1 c
    when_ (len == 2) $ fail "Empty string"
    pure len
{-# INLINE beginMultiQuotes #-}


-- === Top level string parsers === --

natStrQuote, rawStrQuote, fmtStrQuote, escapeChar, etxChar :: Char
natStrQuote = '`'
rawStrQuote = '"'
fmtStrQuote = '\''
escapeChar  = '\\'
etxChar     = '\ETX'
{-# INLINE natStrQuote #-}
{-# INLINE rawStrQuote #-}
{-# INLINE fmtStrQuote #-}
{-# INLINE escapeChar  #-}
{-# INLINE etxChar     #-}

natStr, rawStr, fmtStr :: Lexer
natStr = undefined -- beginStr Symbol.NatStr natStrQuote
rawStr = beginStr Symbol.RawStr rawStrQuote rawStrBody
fmtStr = beginStr Symbol.FmtStr fmtStrQuote fmtStrBody
{-# INLINE natStr #-}
{-# INLINE rawStr #-}
{-# INLINE fmtStr #-}


-- === Native String === --

-- natStrBody :: Int -> Lexer
-- natStrBody = \hlen -> let
--     code   = Symbol.Str <$> takeWhile1 (/= natStrQuote)
--     quotes = do
--         qs <- takeMany1 natStrQuote
--         if Txt.length qs == hlen
--             then pure $ Symbol.Quote Symbol.NatStr Symbol.End -- <$ unliftEntry
--             else pure $ Symbol.Str qs
--     in addSymbol =<< (code <|> quotes)
-- {-# INLINE natStrBody #-}


-- === Raw String === --

-- rawStrBody :: Int -> Lexer
-- rawStrBody = \hlen -> let
--     bodyChar c = c /= rawStrQuote
--               && c /= escapeChar
--               && c /= etxChar
--               && not (newlineStartChar c)
--     body       = Symbol.Str <$> takeWhile1 bodyChar
--     linebr     = Symbol.EOL <$  newline
--     escape     = token escapeChar <**> option (Symbol.Str . convert)
--                                       (const . Symbol.StrEsc <$> esct)
--     esct       = (Symbol.SlashEsc <$ token escapeChar)
--              <|> (Symbol.QuoteEscape Symbol.RawStr <$ token rawStrQuote)
--     quotes     = do
--         qs <- takeMany1 rawStrQuote
--         if Txt.length qs == hlen
--             then pure $ Symbol.Quote Symbol.RawStr Symbol.End -- unliftEntry
--             else pure $ Symbol.Str qs
--     in addSymbol =<< choice [body, escape, quotes, linebr]
-- {-# INLINE rawStrBody #-}


-- === Fmt String === --

data Action = Continue | Break deriving (Show, Eq)

continue, break :: Parser Symbol -> Parser Action
continue f = Continue <$ (addSymbol =<< f)
break    f = Break    <$ (addSymbol =<< f)
{-# INLINE continue #-}
{-# INLINE break    #-}

fmtStrBody :: Int -> Lexer
fmtStrBody = strBodyBuilder Symbol.FmtStr fmtStrQuote bodyExcl [strInterpolation] escs where
    escs     = [slashEsc, rawEsc, fmtEsc, lexEscSeq]
    bodyExcl = fmtStrQuote : natStrQuote : escapeChar : etxChar : newlineStartChars
{-# INLINE fmtStrBody #-}

rawStrBody :: Int -> Lexer
rawStrBody = strBodyBuilder Symbol.RawStr rawStrQuote bodyExcl [strInterpolation] escs where
    escs     = [slashEsc, rawEsc, noEsc]
    noEsc    = pure . Symbol.Str $ convert escapeChar
    bodyExcl = rawStrQuote : escapeChar : etxChar : newlineStartChars
{-# INLINE rawStrBody #-}

escBuilder :: Char -> Symbol.StrEscType -> Parser Symbol
escBuilder t f = Symbol.StrEsc f <$ token t
{-# INLINE escBuilder #-}

slashEsc, rawEsc, fmtEsc :: Parser Symbol
slashEsc = escBuilder escapeChar  $ Symbol.SlashEsc
rawEsc   = escBuilder rawStrQuote $ Symbol.QuoteEscape Symbol.RawStr
fmtEsc   = escBuilder fmtStrQuote $ Symbol.QuoteEscape Symbol.FmtStr
{-# INLINE slashEsc #-}
{-# INLINE rawEsc   #-}
{-# INLINE fmtEsc   #-}

strBodyBuilder :: Symbol.StrType -> Char -> [Char] -> [Lexer] -> [Parser Symbol] -> Int -> Lexer
strBodyBuilder = \tp quote bodyExcl bodyExts escs hlen -> let
    bodyElem   = not . (`elem` bodyExcl)
    body       = continue $ Symbol.Str <$> takeWhile1 bodyElem
    linebr     = continue $ Symbol.EOL <$ newline
    escape     = continue $ token escapeChar >> choice escs
    bodyParser = choice $ body : escape : linebr : quotes : etx
                        : ((Continue <$) <$> bodyExts)
    quotes      = do
        qs <- takeMany1 quote
        if Txt.length qs == hlen
            then break    $ pure (Symbol.Quote tp Symbol.End)
            else continue $ Symbol.Str . (qs <>) <$> whiteSpace
    etx    = break $ Symbol.Invalid
       (Invalid.Literal $ Invalid.String Invalid.NoClosingMark) <$ endOfInput
    go = bodyParser >>= \case
        Continue -> go
        Break    -> pure ()
    in go
{-# INLINE strBodyBuilder #-}


strInterpolation :: Lexer
strInterpolation = let
    ending = token natStrQuote >> addSymbol (Symbol.Block Symbol.End)
    in do
        token natStrQuote
        addSymbol (Symbol.Block Symbol.Begin)
        topExpr `loopUnless` (ending <|> endOfInput)
{-# INLINE strInterpolation #-}


-- fmtStrCode :: Int -> Lexer
-- fmtStrCode = \hlen -> let
--     ending = do
--         qs <- takeMany1 natStrQuote
--         when_ (Txt.length qs /= hlen) $ fail "Not an ending"
--         -- unliftEntry
--         addSymbol $ Symbol.Block Symbol.End
--     in ending <|> topEntryPoint
-- {-# INLINE fmtStrCode #-}


-- === Escape maps === --

esc1Map :: Map String Int
esc1Map = Char.ord <$> fromList
    [ ("a", '\a'), ("b", '\b'), ("f", '\f'), ("n", '\n'), ("r", '\r')
    , ("t", '\t'), ("v", '\v')
    ]
{-# INLINE esc1Map #-}

esc2Map :: Map String Int
esc2Map = Char.ord <$> fromList
    [ ("BS", '\BS'), ("HT", '\HT'), ("LF", '\LF'), ("VT", '\VT'), ("FF", '\FF')
    , ("CR", '\CR'), ("SO", '\SO'), ("SI", '\SI'), ("EM", '\EM'), ("FS", '\FS')
    , ("GS", '\GS'), ("RS", '\RS'), ("US", '\US'), ("SP", '\SP')
    ]
{-# INLINE esc2Map #-}

esc3Map :: Map String Int
esc3Map = Char.ord <$> fromList
    [ ("NUL", '\NUL'), ("SOH", '\SOH'), ("STX", '\STX'), ("ETX", '\ETX')
    , ("EOT", '\EOT'), ("ENQ", '\ENQ'), ("ACK", '\ACK'), ("BEL", '\BEL')
    , ("DLE", '\DLE'), ("DC1", '\DC1'), ("DC2", '\DC2'), ("DC3", '\DC3')
    , ("DC4", '\DC4'), ("NAK", '\NAK'), ("SYN", '\SYN'), ("ETB", '\ETB')
    , ("CAN", '\CAN'), ("SUB", '\SUB'), ("ESC", '\ESC'), ("DEL", '\DEL')
    ]
{-# INLINE esc3Map #-}

lexEscSeq :: Parser Symbol
lexEscSeq = numEsc <|> chrEcs <|> badEsc where
    numEsc = Symbol.StrEsc . Symbol.NumStrEsc . unsafeRead . convert
         <$> takeWhile1 isDecDigitChar
    chrEcs = choice $ uncurry parseEsc <$> zip [1..] [esc1Map, esc2Map, esc3Map]
    badEsc = Symbol.Invalid
             (Invalid.Literal $ Invalid.String Invalid.EscapeCode) <$ anyToken
{-# INLINE lexEscSeq #-}

parseEsc :: Int -> Map String Int -> Parser Symbol
parseEsc n m = do
    s <- count n anyToken
    case Map.lookup s m of
        Just i  -> pure . Symbol.StrEsc $ Symbol.CharStrEsc i
        Nothing -> fail "Escape not matched"
{-# INLINE parseEsc #-}



--------------------
-- === Config === --
--------------------

markerBegin, markerEnd :: Char
markerBegin = '«'
markerEnd   = '»'
{-# INLINE markerBegin #-}
{-# INLINE markerEnd   #-}

metadataHeader :: IsString s => s
metadataHeader = "META"
{-# INLINE metadataHeader #-}

mkMetadata :: (IsString s, Semigroup s) => s -> s
mkMetadata s = "### " <> metadataHeader <> s
{-# INLINE mkMetadata #-}

lexConfig :: Parser Symbol
lexConfig = takeMany ' ' *> lexMetadata
{-# INLINE lexConfig #-}

lexMetadata :: Parser Symbol
lexMetadata = Symbol.Metadata
    <$  tokens metadataHeader
    <*  takeMany1 ' '
    <*> takeLine
{-# INLINE lexMetadata #-}

lexComment :: Parser Symbol
lexComment = Symbol.Doc <$> takeLine
{-# INLINE lexComment #-}

lexMarker :: Lexer
lexMarker = addSymbol =<< sym
    where incorrect = Symbol.Incorrect . ("Marker " <>)
                  <$> takeTill (== markerEnd)
          correct   = Symbol.Marker . unsafeRead . convert
                  <$> takeWhile1 isDecDigitChar
          sym = token markerBegin *> (correct <|> incorrect) <* token markerEnd
{-# INLINE lexMarker #-}



-----------------------
-- === Operators === --
-----------------------

operatorChars :: [Char]
operatorChars = "!$%&*+-/<>?^~\\"
{-# INLINE operatorChars #-}

isOperatorChar :: Char -> Bool
isOperatorChar = (`elem` operatorChars)
{-# INLINE isOperatorChar #-}

isOperator :: Convertible' s String => s -> Bool
isOperator = test . convertTo' @String where
    test s = all isOperatorChar s
          || s `elem` [",", "..", "...", "=="]
          || (last s == Just '=') && isOperator (unsafeInit s)
{-# INLINE isOperator #-}



---------------------------
-- === Top level map === --
---------------------------

-- === Symbol head char map === --

-- | The size of `symmap` - Vector-based map from head Char to related parser.
symmapSize :: Int
symmapSize = 200
{-# INLINE symmapSize #-}

symmap :: Vector Lexer
symmap = Vector.generate symmapSize $ \i -> let c = Char.chr i in if

    -- Layouting
    | c == ';'          -> dropToken >> addSymbol Symbol.Terminator
    | c == '{'          -> dropToken >> addSymbol (Symbol.Block Symbol.Begin)
    | c == '}'          -> dropToken >> addSymbol (Symbol.Block Symbol.End)
    | c == '('          -> dropToken >> addSymbol (Symbol.Group Symbol.Begin)
    | c == ')'          -> dropToken >> addSymbol (Symbol.Group Symbol.End)
    | c == '\n'         -> dropToken >> addSymbol Symbol.EOL
    | c == '\r'         -> dropToken >> option_ (token '\n')
                        >> addSymbol Symbol.EOL
    | c == ':'          -> addSymbol =<< handleColons =<< takeMany ':'
    | c == markerBegin  -> lexMarker

    -- Identifiers & Keywords
    | c == '_'          -> lexWildcard
    | isVarHead  c      -> lexVariable
    | isConsHead c      -> lexConstructor

    -- Operators
    | c == '@'          -> dropToken >> addSymbol Symbol.TypeApp
    | c == '|'          -> dropToken >> addSymbol Symbol.Merge
    | c == '.'          -> addSymbol =<< handleDots =<< takeMany '.'
    | c == '='          -> addSymbol =<< handleEqs  =<< takeMany '='
    | isOperatorChar c  -> addSymbol =<< (handleOp <$> takeWhile1 isOperatorChar
                                     <*> takeMany '=')

    -- Literals
    | c == '['          -> dropToken >> addSymbol (Symbol.List Symbol.Begin)
    | c == ']'          -> dropToken >> addSymbol (Symbol.List Symbol.End)
    | c == ','          -> dropToken >> addSymbol (Symbol.Operator ",")
    | c == rawStrQuote  -> rawStr
    | c == fmtStrQuote  -> fmtStr
    -- | c == natStrQuote  -> natStr
    | isDecDigitChar c  -> lexNumber

    -- Meta
    | c == '#'          -> addSymbol =<< handleHash =<< takeMany '#'

    -- Utils
    | otherwise         -> fail $ "Unknown " <> show c -- unknownCharSym c

    where
    handleColons = handleReps  [ Symbol.BlockStart, Symbol.Typed]
    handleDots   = handleReps  [ Symbol.Accessor, Symbol.Range, Symbol.Anything]
    handleEqs    = handleReps  [ Symbol.Assignment, Symbol.Operator "=="]
    handleHash   = handleRepsM [ lexComment, pure Symbol.Disable, lexConfig]
    handleReps   = handleRepsM . fmap pure
    handleRepsM  = \ts s -> fromJust (pure $ Symbol.Unknown s)
                 $ ts ^? ix (Txt.length s - 1)
    handleOp p s = if (p == "<" || p == ">") && s == "="
                       then Symbol.Operator (p <> s)
                       else case s of
                           "=" -> Symbol.Modifier p
                           ""  -> Symbol.Operator p
                           s   -> Symbol.Unknown (p <> s)
{-# NOINLINE symmap #-}


-- -- === Utils === --

unknownCharSym :: Char -> Lexer
unknownCharSym c = dropToken >> addSymbol (Symbol.Unknown $ convert c)
{-# INLINE unknownCharSym #-}



-----------------------------------
-- === Top level combinators === --
-----------------------------------

lexer :: Parser ()
lexer = topEntryPoint `loopUnless` (() <$ token '\ETX')
{-# INLINE lexer #-}

topEntryPoint :: Lexer
topEntryPoint = putIsTopLevel True >> (topExpr <|> skipUnknown) where
    skipUnknown = unknownCharSym =<< anyToken
{-# INLINE topEntryPoint #-}

topExpr :: Lexer
topExpr = lexSymChar =<< peekToken
{-# INLINE topExpr #-}

-- | (1): fetch  lexers for ASCII from precomputed cache
--   (2): create lexers for unicode names on the fly
lexSymChar :: Char -> Lexer
lexSymChar = \c -> let chord = Char.ord c in if
    | chord < symmapSize -> Vector.unsafeIndex symmap chord -- (1)
    | isVarHead        c -> lexVariable                     -- (2)
    | isConsHead       c -> lexConstructor
    | isInvalidVarHead c -> lexInvalidVariable
    | otherwise          -> unknownCharSym c
{-# INLINE lexSymChar #-}

-- | (1): Do not include whitespaces as offset inside text.
--   (2): Do not include whitespaces as offset after inline text code.
-- lexeme :: Parser Int
-- lexeme = getLastSymbol >>= \case
--     Symbol.Quote _ Symbol.Begin -> pure 0 -- (1)
--     Symbol.Block Symbol.End     -> pure 0 -- (2)
--     _                           -> spacing
-- {-# INLINE lexeme #-}


-- lexEntryPoint :: Lexer
-- lexEntryPoint = getEntryPoint >>= \case
--     TopLevelEntry  -> topEntryPoint
--     StrCodeEntry i -> fmtStrCode i
--     StrEntry   t i -> case t of
--         Symbol.RawStr -> rawStrBody i
--         Symbol.FmtStr -> fmtStrBody i
--         Symbol.NatStr -> natStrBody i
-- {-# INLINE lexEntryPoint #-}


