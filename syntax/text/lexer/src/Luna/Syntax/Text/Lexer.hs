{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE Strict                    #-}

module Luna.Syntax.Text.Lexer where

import Prologue hiding (List, Type, Symbol, cons, span, range, catch, takeWhile)
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
import           Data.VectorText              (VectorText)
import qualified Data.VectorText              as VectorText
import           Luna.Syntax.Text.Lexer.Name  (isRegularOperatorChar, markerBegin, markerEnd, metadataHeader)
import qualified Data.Text                    as Text
import           Luna.Syntax.Text.Lexer.Stream (Token, ParseError, conduitParserEither)
import           Conduit
import qualified Data.Attoparsec.Text as Parsec

import Data.Parser hiding (Token)
import Data.Parser.Instances.Attoparsec ()


type Parser = StateT EntryStack Parsec.Parser


-- FIXME[WD]: TO REFACTOR
class ShowCons a where
    showCons :: a -> Text


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




------------------
-- === Tags === --
------------------

-- === Definition === --

type Tags = [Text]

class IsTagged a where
    getTags :: a -> Tags
    default getTags :: Show a => a -> Tags
    getTags = tagFromShow ; {-# INLINE getTags #-}


-- === Utils === --

singleTag :: Text -> Tags
singleTag = pure ; {-# INLINE singleTag #-}

tagFromShow :: Show a => a -> Tags
tagFromShow = singleTag . convert . show ; {-# INLINE tagFromShow #-}


-- === Instances === --

data Tagged a = Tagged Tags a deriving (Show, Functor, Foldable, Traversable)



--------------------
-- === Tokens === --
--------------------

-- === Definitions === --

type Lexer = Parser Symbol

data Symbol -- Layout
            = BOF
            | EOF
            | EOL
            | Terminator
            | BlockStart
            | Block       !Bound
            | Group       !Bound
            | Marker      !Word64

            -- Ident
            | Var         !Text
            | Cons        !Text
            | Wildcard

            -- Keyword
            | KwAll
            | KwCase
            | KwClass
            | KwDef
            | KwImport
            | KwOf

            -- Operator
            | Operator    !Text
            | Modifier    !Text
            | Accessor
            -- | Arrow
            | Assignment
            | Typed
            | TypeApp
            | Merge
            | Range
            | Anything

            -- Literal
            | Number      !Number
            | Quote       !StrType  !Bound
            | Str         !Text
            | StrEsc      !StrEscType
            | List        !Bound
            | StrWrongEsc !Int

            -- Comment
            | Disable
            | Doc         !Text

            -- Config
            | Metadata    !Text
            -- | Pragma ...

            -- Other
            | Unknown     !Text
            | Incorrect   !Text
            deriving (Generic, NFData, Show, Eq, Ord)

data StrEscType = CharStrEsc  !Int
                | NumStrEsc   !Int
                | QuoteEscape !StrType !Int
                | SlashEsc
                deriving (Generic, NFData, Show, Eq, Ord)


data Bound   = Begin | End              deriving (Generic, NFData, Show, Eq, Ord)
data StrType = RawStr | FmtStr | NatStr deriving (Generic, NFData, Show, Eq, Ord)
data Numbase = Dec | Bin | Oct | Hex    deriving (Generic, NFData, Show, Eq, Ord)
data Number  = NumRep { _base     :: Numbase
                      , _intPart  :: Text
                      , _fracPart :: Text
                      , _expPart  :: Text
                      } deriving (Generic, NFData, Show, Eq, Ord)
makeLenses ''Number



-- === Utils === --

checkSpecialVar :: Text -> Symbol
checkSpecialVar = \case
    "all"    -> KwAll
    "case"   -> KwCase
    "class"  -> KwClass
    "def"    -> KwDef
    "import" -> KwImport
    "of"     -> KwOf
    "_"      -> Wildcard
    name     -> Var name
{-# INLINE checkSpecialVar #-}

isDecDigitChar, isOctDigitChar, isBinDigitChar, isHexDigitChar, isIndentBodyChar :: Char -> Bool
isDecDigitChar   c = (c >= '0' && c <= '9')                                                           ; {-# INLINE isDecDigitChar   #-}
isOctDigitChar   c = (c >= '0' && c <= '7')                                                           ; {-# INLINE isOctDigitChar   #-}
isBinDigitChar   c = (c == '0' || c == '1')                                                           ; {-# INLINE isBinDigitChar   #-}
isHexDigitChar   c = isDecDigitChar c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')             ; {-# INLINE isHexDigitChar   #-}
isIndentBodyChar c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || isDecDigitChar c || c == '_' ; {-# INLINE isIndentBodyChar #-}

opChars :: [Char]
opChars = "!$%&*+-/<>?^~\\" ; {-# INLINE opChars #-}

matchVar, matchCons, matchOperator, matchModifier, matchStr, matchMetadata :: Symbol -> Maybe Text
matchNumber   :: Symbol -> Maybe Number
matchMarker   :: Symbol -> Maybe Word64
matchVar      = \case { Var      a -> Just a ; _ -> Nothing } ; {-# INLINE matchVar      #-}
matchCons     = \case { Cons     a -> Just a ; _ -> Nothing } ; {-# INLINE matchCons     #-}
matchOperator = \case { Operator a -> Just a ; _ -> Nothing } ; {-# INLINE matchOperator #-}
matchModifier = \case { Modifier a -> Just a ; _ -> Nothing } ; {-# INLINE matchModifier #-}
matchStr      = \case { Str      a -> Just a ; _ -> Nothing } ; {-# INLINE matchStr      #-}
matchNumber   = \case { Number   a -> Just a ; _ -> Nothing } ; {-# INLINE matchNumber   #-}
matchMarker   = \case { Marker   a -> Just a ; _ -> Nothing } ; {-# INLINE matchMarker   #-}
matchMetadata = \case { Metadata a -> Just a ; _ -> Nothing } ; {-# INLINE matchMetadata #-}

intNum :: Text -> Number
intNum  i = NumRep Dec i mempty mempty ; {-# INLINE intNum #-}

pretty :: Symbol -> Text
pretty = \case
    BOF         {} -> "Begin of file"
    EOF         {} -> "End of file"
    EOL         {} -> "End of line"
    Terminator  {} -> "Expression terminator"
    BlockStart  {} -> "Expression block start"
    Block       {} -> "Block"
    Group       {} -> "Expression Group"
    Marker      {} -> "Internal position marker"
    Var         {} -> "Variable"
    Cons        {} -> "Constructor"
    Wildcard    {} -> "Wildcard"
    KwAll       {} -> "Keyword `All`"
    KwCase      {} -> "Keyword `Case`"
    KwClass     {} -> "Keyword `Class`"
    KwDef       {} -> "Keyword `Def`"
    KwImport    {} -> "Keyword `Import`"
    KwOf        {} -> "Keyword `Of`"
    Operator    {} -> "Operator"
    Modifier    {} -> "Modifier"
    Accessor    {} -> "Accessor"
    Assignment  {} -> "Assignment"
    TypeApp     {} -> "Type application"
    Merge       {} -> "Merge operator"
    Range       {} -> "Range operator"
    Anything    {} -> "Anything operator"
    Number      {} -> "Number"
    Quote       {} -> "Quote"
    Str         {} -> "String literal"
    StrEsc      {} -> "String escape sequence"
    List        {} -> "List"
    StrWrongEsc {} -> "Wrong string escape sequence"
    Disable     {} -> "Disable block"
    Doc         {} -> "Documentation"
    Metadata    {} -> "Metadata"
    Unknown     s  -> "Unknown symbol " <> s
    Incorrect   s  -> "Incorrect " <> s
{-# INLINE pretty #-}


-- === Instances === --

-- FIXME[WD]: Templatehaskellize vvv
instance ShowCons Symbol where
    showCons = \case
        BOF         {} -> "BOF"
        EOF         {} -> "EOF"
        EOL         {} -> "EOL"
        Terminator  {} -> "Terminator"
        BlockStart  {} -> "BlockStart"
        Block       {} -> "Block"
        Group       {} -> "Group"
        Marker      {} -> "Marker"
        Var         {} -> "Var"
        Cons        {} -> "Cons"
        Wildcard    {} -> "Wildcard"
        KwAll       {} -> "KwAll"
        KwCase      {} -> "KwCase"
        KwClass     {} -> "KwClass"
        KwDef       {} -> "KwDef"
        KwImport    {} -> "KwImport"
        KwOf        {} -> "KwOf"
        Operator    {} -> "Operator"
        Modifier    {} -> "Modifier"
        Accessor    {} -> "Accessor"
        Assignment  {} -> "Assignment"
        TypeApp     {} -> "TypeApp"
        Merge       {} -> "Merge"
        Range       {} -> "Range"
        Anything    {} -> "Anything"
        Number      {} -> "Number"
        Quote       {} -> "Quote"
        Str         {} -> "Str"
        StrEsc      {} -> "StrEsc"
        List        {} -> "List"
        StrWrongEsc {} -> "StrWrongEsc"
        Disable     {} -> "Disable"
        Doc         {} -> "Doc"
        Metadata    {} -> "Metadata"
        Unknown     {} -> "Unknown"
        Incorrect   {} -> "Incorrect"
    {-# INLINE showCons #-}

-- Tags
instance IsTagged Symbol where
    getTags a = (<> [showCons a]) $ case a of
        BOF         {} -> ["Layout"]
        EOF         {} -> ["Layout"]
        EOL         {} -> ["Layout"]
        Terminator  {} -> ["Layout"]
        BlockStart  {} -> ["Layout"]
        Block       {} -> ["Layout"]
        Group       {} -> ["Layout"]
        Marker      {} -> ["Layout"]
        Var         {} -> ["Ident"]
        Cons        {} -> ["Ident"]
        Wildcard    {} -> ["Ident"]
        KwAll       {} -> ["Keyword"]
        KwCase      {} -> ["Keyword"]
        KwClass     {} -> ["Keyword"]
        KwDef       {} -> ["Keyword"]
        KwImport    {} -> ["Keyword"]
        KwOf        {} -> ["Keyword"]
        Operator    {} -> ["Operator"]
        Modifier    {} -> ["Operator"]
        Accessor    {} -> ["Operator"]
        Assignment  {} -> ["Operator"]
        TypeApp     {} -> ["Operator"]
        Merge       {} -> ["Operator"]
        Range       {} -> ["Operator"]
        Anything    {} -> ["Operator"]
        Number      {} -> ["Literal"]
        Quote       {} -> ["Literal"]
        Str         {} -> ["Literal"]
        StrEsc      {} -> ["Literal"]
        List        {} -> ["Literal"]
        StrWrongEsc {} -> ["Literal"]
        Disable     {} -> ["Control"]
        Doc         {} -> ["Comment"]
        Metadata    {} -> ["Config"]
        Unknown     {} -> ["Error"]
        Incorrect   {} -> ["Error"]
    {-# INLINE getTags #-}


----------------------
-- === Literals === --
----------------------

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


-- -- === Native String === --

natStr :: Lexer
natStr = beginStr NatStr '`' ; {-# INLINE natStr #-}

natStrBody :: Int -> Lexer
natStrBody hlen = code <|> quotes where
    code   = Str <$> takeWhile1 (/= '`')
    quotes = do
        qs <- takeMany1 '`'
        if Text.length qs == hlen
            then Quote NatStr End <$ unliftEntry
            else return $ Str qs
{-# INLINE natStrBody #-}


-- === Raw String === --

rawStr :: Lexer
rawStr = beginStr RawStr '"' ; {-# INLINE rawStr #-}

rawStrBody :: Int -> Lexer
rawStrBody hlen = choice [body, escape, quotes, linebr] where
    body   = Str <$> takeWhile1 (\c -> c /= '"' && c /= '\n' && c /= '\r' && c /= '\\')
    linebr = EOL <$  newline
    escape = StrEsc <$ token '\\' <*> esct
    esct   = (SlashEsc <$ token '\\')
         <|> (QuoteEscape RawStr . Text.length <$> takeMany1 '\"')
         <|> (QuoteEscape FmtStr . Text.length <$> takeMany1 '\'')
    quotes = do
        qs <- takeMany1 '"'
        if Text.length qs == hlen
            then Quote RawStr End <$ unliftEntry
            else return $ Str qs
{-# INLINE rawStrBody #-}


-- === Fmt String === --

fmtStr :: Lexer
fmtStr = beginStr FmtStr '\'' ; {-# INLINE fmtStr #-}

fmtStrBody :: Int -> Lexer
fmtStrBody hlen = choice [body, escape, quotes, linebr, code] where
    body   = Str <$> takeWhile1 (\c -> c /= '\'' && c /= '`' && c /= '\n' && c /= '\r' && c /= '\\')
    linebr = EOL <$  newline
    escape = token '\\' *> esct
    esct   = (StrEsc   SlashEsc <$ token '\\')
         <|> (StrEsc . QuoteEscape RawStr . Text.length <$> takeMany1 '\"')
         <|> (StrEsc . QuoteEscape FmtStr . Text.length <$> takeMany1 '\'')
         <|> lexEscSeq
    quotes = do
        qs <- takeMany1 '\''
        if Text.length qs == hlen
            then Quote FmtStr End <$ unliftEntry
            else return $ Str qs
    code   = Block Begin <$ (liftEntry . StrCodeEntry =<< beginQuotes '`')
{-# INLINE fmtStrBody #-}

fmtStrCode :: Int -> Lexer
fmtStrCode hlen = ending <|> topEntryPoint where
    ending = do
        qs <- takeMany1 '`'
        when (Text.length qs /= hlen) $ fail "Not an ending"
        Block End <$ unliftEntry
{-# INLINE fmtStrCode #-}


-- Escape maps

esc1Map, esc2Map, esc3Map :: Map String Int
esc1Map = Char.ord <$> fromList [ ("a", '\a'), ("b", '\b'), ("f", '\f'), ("n", '\n'), ("r", '\r'), ("t", '\t'), ("v", '\v'), ("'", '\''), ("\"" , '"') ]
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

lexConfig :: Lexer
lexConfig = takeMany ' ' *> (lexMetadata {- <|> lexPragma -}) ; {-# INLINE lexConfig #-}

lexMetadata :: Lexer
lexMetadata = Metadata <$ tokens metadataHeader <* takeMany1 ' ' <*> takeLine ; {-# INLINE lexMetadata #-}

lexComment :: Lexer
lexComment = Doc <$> takeLine ; {-# INLINE lexComment #-}

lexMarker :: Lexer
lexMarker = token markerBegin *> (correct <|> incorrect) <* token markerEnd where
    incorrect = Incorrect . ("Marker " <>) <$> takeTill (== markerEnd)
    correct   = Marker . read . convert <$> takeWhile1 isDecDigitChar
{-# INLINE lexMarker #-}



-------------------
-- === Lexer === --
-------------------

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
    | c == markerBegin  -> lexMarker

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
    | c == '"'          -> rawStr
    | c == '\''         -> fmtStr
    | c == '`'          -> natStr
    | decHead c         -> lexNumber

    -- Meta
    | c == '#'          -> handleHash . Text.cons c =<< takeMany '#'

    -- Utils
    | otherwise         -> unknownCharSym c

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


----------------------------------
----------------------------------


lexSymChar :: Char -> Lexer
lexSymChar c = if chord < symmapSize then Vector.unsafeIndex symmap chord else unknownCharSym c
    where chord = Char.ord c
{-# INLINE lexSymChar #-}

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

lexer     :: Parser (Symbol, Int)
lexerCont :: Parser ((Symbol, EntryStack), Int)
lexer     = lexeme =<< lexEntryPoint ; {-# INLINE lexer #-}
lexerCont = do
    s         <- lexEntryPoint
    (s', off) <- lexeme s
    es        <- get @EntryStack
    return ((s', es), off)

lexeme :: Symbol -> Parser (Symbol, Int)
lexeme s = case s of
    Quote _ Begin -> return (s, 0) -- Do not include whitespaces as offset inside text.
    Block End     -> return (s, 0) -- Do not include whitespaces as offset after inline text code.
    _             -> (s,) <$> spacing
{-# INLINE lexeme #-}

spacing :: Parser Int
spacing = sum <$> many (spaces <|> tabs) where
    spaces = Text.length        <$> takeMany1 ' '
    tabs   = (4*) . Text.length <$> takeMany1 '\t'
{-# INLINE spacing #-}



---------------------
-- === Running === --
---------------------

fromLexerResult :: Either ParseError a -> a
fromLexerResult = either (error . ("Impossible happened: lexer error: " <>) . show) id ; {-# INLINE fromLexerResult #-}

parseBase :: Monad m => Parser (t, Int) -> EntryStack -> ConduitM a Text m () -> ConduitM a c0 m [Either ParseError (Token t)]
parseBase p s f = f .| conduitParserEither s (runStateT @EntryStack p) .| sinkList ; {-# INLINE parseBase #-}

parse        ::              Parser (a, Int) -> EntryStack -> Text     ->   [Token a]
tryParse     ::              Parser (a, Int) -> EntryStack -> Text     ->   Either ParseError [Token a]
parseFile    :: MonadIO m => Parser (a, Int) -> EntryStack -> FilePath -> m [Token a]
tryParseFile :: MonadIO m => Parser (a, Int) -> EntryStack -> FilePath -> m (Either ParseError [Token a])
parse              = fromLexerResult .:.   tryParse                                                       ; {-# INLINE parse        #-}
parseFile          = fromLexerResult <∘∘∘> tryParseFile                                                   ; {-# INLINE parseFile    #-}
tryParse     p s t = sequence . runConduitPure $ parseBase p s (yield t)                                  ; {-# INLINE tryParse     #-}
tryParseFile p s t = liftIO . fmap sequence . runConduitRes $ parseBase p s (sourceFile t .| decodeUtf8C) ; {-# INLINE tryParseFile #-}

runLexer     :: EntryStack -> Text -> [Token (Symbol, EntryStack)]
evalLexer    :: EntryStack -> Text -> [Token Symbol]
evalDefLexer ::               Text -> [Token Symbol]
runLexer     = parse lexerCont ; {-# INLINE runLexer     #-}
evalLexer    = parse lexer     ; {-# INLINE evalLexer    #-}
evalDefLexer = evalLexer def   ; {-# INLINE evalDefLexer #-}



 -- {-# SPECIALIZE conduitParserEither
 --                    :: Monad m
 --                    => A.Parser T.Text b
 --                    -> Conduit T.Text m (Either ParseError (PositionRange, b)) #-}

--
--
-- -----------------------------------------------------------
--
-- -------------------
-- --- TODO[WD]: We keep it here unless module Position uses depreciated dependent state
--
-- getLine :: MonadGetter Position m => m Delta
-- getLine = view Position.line <$> get @Position
--
-- getColumn :: MonadGetter Position m => m Delta
-- getColumn = view Position.column <$> get @Position
--
-- modColumn :: MonadState Position m => (Delta -> Delta) -> m ()
-- modColumn f = modify_ @Position $ Position.column %~ f
--
-- incColumn :: MonadState Position m => Delta -> m ()
-- incColumn = modColumn . (+)
--
-- succColumn :: MonadState Position m => m ()
-- succColumn = modColumn succ
--
-- succLine :: MonadState Position m => m ()
-- succLine = modify_ @Position $ (Position.column .~ 0) . (Position.line %~ succ)
--
-- ---------------------
--
-- -- === Helper states === --
--
-- newtype LastOffset     = LastOffset     Delta       deriving (Generic, NFData, Show, Default)
-- newtype DisabledBlocks = DisabledBlocks (Set Delta) deriving (Generic, NFData, Show, Default)
-- makeLenses ''LastOffset
-- makeLenses ''DisabledBlocks
--
--
--
--
--
-- instance (MonadGetter Offset m, Show t, Monad m) => MonadErrorBuilder t m LexerError where
--     buildError t = LexerError <$> get @Offset <*> pure (show t)
--
-- instance IsString LexerError where fromString = LexerError 0
--
--
---------------------------
-- === LexerToken === --
---------------------------




-- === Definition === --

-- data LexerToken s = LexerToken
--     { _span    :: !Delta
--     , _offset  :: !Delta
--     , _element :: !s
--     } deriving (Generic, Show, Eq, Ord, Functor, Foldable, Traversable) -- FIXME[WD]: Ord is needed by some weird Megaparsec fundep. Remove it when possible.
-- makeLenses ''LexerToken


-- -- === Running === --
--
-- absToRelToks :: [LexerToken s] -> [LexerToken s]
-- absToRelToks = go 0 where
--     go d = \case
--         []     -> []
--         (t:ts) -> t' : go (t ^. offset) ts where
--             t' = t & span   %~ (subtract d)
--                    & offset %~ (subtract $ t ^. span)
--
-- -- runLexer :: forall s . _ => String -> [LexerToken s]
-- -- runLexer s = fromRight (error "Lexer error")
-- --            . runIdentity
-- --            . inferT @ResultSource @Symbol
-- --            . evalBacktracker
-- --            . runFailParser @String
-- --            . evalStreamProvider s
-- --            . evalOffsetRegister
-- --            . execResultRegister @(LexerToken s)
-- --            $ tokenizer
--
-- textStream :: VectorText -> Stream VectorText
-- textStream = Stream VectorText.splitHead
--
-- runLexer :: forall s. _ => VectorText -> [LexerToken Symbol]
-- runLexer s = fromRight (error "Lexer error")
--            . runIdentity
--            . inferT @ResultSource @Symbol
--            . evalBacktracker
--            . runFailParser @LexerError
--            . evalDefStateT @Position
--            . evalDefStateT @LastOffset
--            . evalDefStateT @DisabledBlocks
--            . evalStreamProvider (textStream s)
--         --    . evalHistoryRegister
--            . evalOffsetRegister
--            . execResultRegister @(LexerToken Symbol)
--            $ tokenizer
--
--
-- -- === Instances === --
--
-- -- instance (Token m ~ Char, s ~ s', Eq s, MonadGetter Offset m, MonadTokenParser m, MonadProgressParser m, MonadErrorParser SatisfyError m, MonadErrorParser EmptyStreamError m, Alternative m)
-- --       => MonadResultBuilder Symbol m (LexerToken s') where
-- --     buildResult sym = do
-- --         (span, off) <- (,) <$> get @Offset <* many (token ' ') <*> get @Offset
-- --         return $ LexerToken (convert span) (convert off) sym
--
-- instance NFData s => NFData (LexerToken s)
--
-- instance ( MonadGetter Offset m, Token m ~ Char, MonadTokenParser m, MonadProgressParser m, Alternative m, MonadErrorParser SatisfyError m
--          , MonadErrorParser EmptyStreamError m, MonadState LastOffset m
--          , MonadState Position m, MonadState DisabledBlocks m, s ~ s', Eq s)
--       => MonadResultBuilder Symbol m (LexerToken (Symbol')) where
--     buildResult sym = do
--         -- Lexeme & offsets computing
--         lastOff <- unwrap  <$> get @LastOffset
--         textOff <- convert <$> get @Offset
--         many $ token ' '
--         spacOff <- convert <$> get @Offset
--         put @LastOffset (wrap spacOff)
--         let textDiff = textOff - lastOff
--             spacDiff = spacOff - textOff
--
--         -- Computing real parsed text
--         -- history <- unwrap  <$> get @History
--         -- let src = reverse $ take (convert textDiff)
--         --                   $ drop (convert spacDiff)
--         --                   $ history
--
--
--         -- Position (row/col) update
--         pos <- get @Position
--         incColumn textDiff
--         when (sym == EOL) succLine
--         incColumn spacDiff
--
--         -- DisabledBlocks management and discovery of disabled tokens
--         dblocks <- unwrap <$> get @DisabledBlocks
--         let (lblocks, rblocks) = Set.split (pos ^. Position.column) dblocks
--         put @DisabledBlocks (wrap lblocks)
--         when (sym == Disable) $ modify_ @DisabledBlocks (wrapped %~ (Set.insert $ pos ^. Position.column))
--         let tags = if Set.null lblocks then mempty else ["Disabled"]
--
--         -- Finitialization
--         return $ LexerToken textDiff spacDiff sym
--     -- {-# INLINE buildResult #-}
--
--
--
-- ---------------------------
-- -- === LexerGUIToken === --
-- ---------------------------
--
-- -- === Definition === --
--
-- data LexerGUIToken s = LexerGUIToken
--     { _guiSpan    :: !Delta
--     , _guiOffset  :: !Delta
--     , _guiSource  :: !String
--     , _guiTags    :: ![String]
--     , _guiSymbol  :: !Symbol
--     } deriving (Show, Eq, Ord) -- FIXME[WD]: Ord is needed by some weird Megaparsec fundep. Remove it when possible.
-- makeLenses ''LexerGUIToken
--
--
-- -- === Running === --
--
-- runGUILexer :: String -> [LexerGUIToken String]
-- runGUILexer s = fromRight (error "Lexer error")
--               . runIdentity
--               . inferT @ResultSource @(Symbol String)
--               . evalBacktracker
--               . runFailParser @LexerError
--               . evalDefStateT @Position
--               . evalDefStateT @LastOffset
--               . evalDefStateT @DisabledBlocks
--               . evalStreamProvider (listStream s)
--               . evalHistoryRegister
--               . evalOffsetRegister
--               . execResultRegister @(LexerGUIToken String)
--               $ tokenizer
--
--
-- -- === Instances === --
--
-- instance IsTagged (LexerGUIToken s) where
--     getTags tok = tok ^. guiTags <> getTags (tok ^. guiSymbol)
--
-- instance ( MonadGetter Offset m, Token m ~ Char, MonadTokenParser m, MonadProgressParser m, Alternative m, MonadErrorParser SatisfyError m
--          , MonadErrorParser EmptyStreamError m, MonadState LastOffset m, MonadGetter History m, StateData History m ~ History (Token m)
--          , MonadState Position m, MonadState DisabledBlocks m, s ~ s', Eq s)
--       => MonadResultBuilder Symbol m (LexerGUIToken s') where
--     buildResult sym = do
--         -- Lexeme & offsets computing
--         lastOff <- unwrap  <$> get @LastOffset
--         textOff <- convert <$> get @Offset
--         many $ token ' '
--         spacOff <- convert <$> get @Offset
--         put @LastOffset (wrap spacOff)
--         let textDiff = textOff - lastOff
--             spacDiff = spacOff - textOff
--
--         -- Computing real parsed text
--         history <- unwrap  <$> get @History
--         let src = reverse $ take (convert textDiff)
--                           $ drop (convert spacDiff)
--                           $ history
--
--         -- Position (row/col) update
--         pos <- get @Position
--         incColumn textDiff
--         when (sym == EOL) succLine
--         incColumn spacDiff
--
--         -- DisabledBlocks management and discovery of disabled tokens
--         dblocks <- unwrap <$> get @DisabledBlocks
--         let (lblocks, rblocks) = Set.split (pos ^. Position.column) dblocks
--         put @DisabledBlocks (wrap lblocks)
--         when (sym == Disable) $ modify_ @DisabledBlocks (wrapped %~ (Set.insert $ pos ^. Position.column))
--         let tags = if Set.null lblocks then mempty else ["Disabled"]
--
--         -- Finitialization
--         return $ LexerGUIToken textDiff spacDiff src tags sym
--
--
--
--
--
--
--
--
--
--
-- instance Convertible Offset Delta where
--     convert = convert . unwrap
--
-- -- main :: IO ()
-- -- main = do
-- --     print "LEXER:"
-- --     pprint $ runGUILexer "1"
-- --     -- pprint $ getTags <$> runGUILexer "foo bar 1.3 baz"
-- --     print "---"
-- --     -- Parsert.main
