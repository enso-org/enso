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
import qualified Data.Map                     as Map
import           Data.Map                     (Map)
import qualified Data.Text.Span               as Span
import           Data.Text.Position           (Delta, Position, Offset)
import qualified Data.Text.Position           as Position
import           Data.Vector                  (Vector)
import qualified Data.Vector                  as Vector
import           Data.VectorText              (VectorText)
import qualified Data.VectorText              as VectorText
import           Luna.Syntax.Text.Lexer.Name  (isRegularOperatorChar, markerBegin, markerEnd, metadataHeader)
import qualified Data.Text                    as Text
import           Data.Set (Set)
import qualified Data.Set as Set

-- import Text.Parsert as Parsert
import Type.Inference

import qualified Data.Attoparsec.Text as Parsec
-- import           Data.Attoparsec.Text (satisfy, takeWhile, takeWhile1, option, choice, char, anyChar)

import Data.Parser
import Data.Parser.Instances.Attoparsec ()

type Parser = StateT EntryPoint Parsec.Parser
-- type Parser = Parsec.Parser

-- FIXME[WD]: TO REFACTOR
class ShowCons a where
    showCons :: a -> String



data EntryPoint = GlobalEntry
                | Something
                deriving (Show)

instance Default EntryPoint where def = GlobalEntry ; {-# INLINE def #-}


------------------
-- === Tags === --
------------------

-- === Definition === --

type Tags = [String]

class IsTagged a where
    getTags :: a -> Tags
    default getTags :: Show a => a -> Tags
    getTags = tagFromShow


-- === Utils === --

singleTag :: String -> Tags
singleTag = pure

tagFromShow :: Show a => a -> Tags
tagFromShow = singleTag . show


-- === Instances === --

data Tagged a = Tagged Tags a deriving (Show, Functor, Foldable, Traversable)



--------------------
-- === Tokens === --
--------------------

-- === Definitions === --

data Symbol -- Layout
            = BOF
            | EOF
            | EOL
            | Terminator
            | BlockStart
            | Block  !Bound
            | Group  !Bound
            | Marker {-# UNPACK #-} !Word64

            -- Ident
            | Var    {-# UNPACK #-} !Text
            | Cons   {-# UNPACK #-} !Text
            | Wildcard

            -- Keyword
            | KwAll
            | KwCase
            | KwClass
            | KwDef
            | KwImport
            | KwOf

            -- Operator
            | Operator {-# UNPACK #-} !Text
            | Modifier {-# UNPACK #-} !Text
            | Accessor
            -- | Arrow
            | Assignment
            | Typed
            | TypeApp
            | Merge
            | Range
            | Anything

            -- Literal
            | Number {-# UNPACK #-} !Number
            | Quote  !QuoteType !Bound
            | Str    {-# UNPACK #-} !Text
            | StrEsc !StrEscType {-# UNPACK #-} !Int
            | List   !Bound
            | StrWrongEsc {-# UNPACK #-} !Int

            -- Comment
            | Disable
            | Doc      {-# UNPACK #-} !Text

            -- Config
            | Metadata {-# UNPACK #-} !Text
            -- | Pragma ...

            -- Other
            | Unknown   {-# UNPACK #-} !Text
            | Incorrect {-# UNPACK #-} !Text
            deriving (Generic, NFData, Show, Eq, Ord)

data Bound      = Begin | End              deriving (Generic, NFData, Show, Eq, Ord)
data QuoteType  = RawStr | FmtStr | Native deriving (Generic, NFData, Show, Eq, Ord)
data StrEscType = CharStrEsc | NumStrEsc   deriving (Generic, NFData, Show, Eq, Ord)
data Numbase    = Dec | Bin | Oct | Hex    deriving (Generic, NFData, Show, Eq, Ord)
data Number     = NumRep { _base     :: Numbase
                         , _intPart  :: Text
                         , _fracPart :: Text
                         , _expPart  :: Text
                         } deriving (Generic, NFData, Show, Eq, Ord)
makeLenses ''Number



-- === Utils === --

checkSpecialOp :: Text -> Either Text Symbol
checkSpecialOp = \case
    name -> Left name
{-# INLINE checkSpecialOp #-}

checkSpecialVar :: Text -> Either Text Symbol
checkSpecialVar = \case
    "all"    -> Right KwAll
    "case"   -> Right KwCase
    "class"  -> Right KwClass
    "def"    -> Right KwDef
    "import" -> Right KwImport
    "of"     -> Right KwOf
    "_"      -> Right Wildcard
    name     -> Left  name
{-# INLINE checkSpecialVar #-}

isDecDigitChar   :: Char -> Bool
isOctDigitChar   :: Char -> Bool
isBinDigitChar   :: Char -> Bool
isHexDigitChar   :: Char -> Bool
isIndentBodyChar :: Char -> Bool
isDecDigitChar   c = (c >= '0' && c <= '9')                                                           ; {-# INLINE isDecDigitChar   #-}
isOctDigitChar   c = (c >= '0' && c <= '7')                                                           ; {-# INLINE isOctDigitChar   #-}
isBinDigitChar   c = (c == '0' || c == '1')                                                           ; {-# INLINE isBinDigitChar   #-}
isHexDigitChar   c = isDecDigitChar c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')             ; {-# INLINE isHexDigitChar   #-}
isIndentBodyChar c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || isDecDigitChar c || c == '_' ; {-# INLINE isIndentBodyChar #-}

opChars :: [Char]
opChars = "!$%&*+-/<>?^~\\" ; {-# INLINE opChars #-}

matchVar, matchCons, matchOperator, matchModifier :: Symbol -> Maybe Text
matchStr      :: Symbol -> Maybe Text
matchNumber   :: Symbol -> Maybe Number
matchMarker   :: Symbol -> Maybe Word64
matchMetadata :: Symbol -> Maybe Text
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
    Unknown     s  -> "Unknown symbol " <> convert (show s)
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


--
-- -----------------------------
-- -- === Errors handling === --
-- -----------------------------
--
-- data LexerError = LexerError { _errOffset :: Offset
--                              , _errDesc   :: String
--                              } deriving (Show)
-- makeLenses ''LexerError
--
--
-- dropRecover :: (Error m ~ LexerError, MonadCatchParser m, MonadTokenParser m, MonadErrorParser SatisfyError m, MonadProgressParser m, MonadErrorParser EmptyStreamError m, MonadGetter Offset m)
--             => (NonEmpty (Error m) -> m a) -> m a -> m a
-- dropRecover onerr = recover go where
--     go es = do
--         let e = head $ convert es
--         off <- get @Offset
--         sequence_ (replicate (e ^. errOffset - off - 1) anyToken)
--         onerr es
--
-- dropRecover' :: (Error m ~ LexerError, MonadCatchParser m, MonadTokenParser m, MonadErrorParser SatisfyError m, MonadProgressParser m, MonadErrorParser EmptyStreamError m, MonadGetter Offset m)
--              => m a -> m a -> m a
-- dropRecover' = dropRecover . const
--
--
--
----------------------
-- === Literals === --
----------------------

-- === Numbers === --

lexNumber :: Char -> Parser Symbol
lexNumber digit = Number <$> topLvl digit <* checkNoSfx
    where hexNum      = satisfy (\s -> s == 'x' || s == 'X') *> lexHexNum
          octNum      = satisfy (\s -> s == 'o' || s == 'O') *> lexOctNum
          binNum      = satisfy (\s -> s == 'b' || s == 'B') *> lexBinNum
          lexDecNum c = NumRep Dec . (Text.cons c) <$> lexDec' <*> pure mempty <*> pure mempty -- lexFrac <*> lexExp
          lexHexNum   = NumRep Hex <$> lexHex <*> pure mempty <*> pure mempty
          lexOctNum   = NumRep Oct <$> lexOct <*> pure mempty <*> pure mempty
          lexBinNum   = NumRep Bin <$> lexBin <*> pure mempty <*> pure mempty
          lexDecM     = maybe id Text.cons <$> optional (satisfy (\s -> s == '-' || s == '+')) <*> lexDec
          lexDec'     = takeWhile  isDecDigitChar
          lexDec      = takeWhile1 isDecDigitChar
          lexHex      = takeWhile1 isHexDigitChar
          lexOct      = takeWhile1 isOctDigitChar
          lexBin      = takeWhile1 isBinDigitChar
          lexFrac     = option mempty $ token '.' *> lexDec
          lexExp      = option mempty $ token 'e' *> lexDecM
          checkNoSfx  = pure () -- option'_ $ (\s -> descibedError' $ "Unexpected characters '" <> s <> "' found on the end of number literal") =<< some (satisfy Char.isAlphaNum)
          topLvl      = \case '0' -> choice [hexNum, octNum, binNum, lexDecNum digit]
                              c   -> lexDecNum c
{-# INLINE lexNumber #-}

-- option'_ p = void p <|> pure ()

--
-- -- === Native String === --
--
-- lexNatStr :: SymbolLexing s m => m ()
-- lexNatStr = body *> ending where
--     body   = addResult =<< Str <$> many (satisfy (/= '`'))
--     ending = option_ $ token '`' *> addResult (Quote Native End)
--
--
-- -- === Raw String === --
--
-- lexRawStr :: SymbolLexing s m => Int -> m ()
-- lexRawStr hlen = if hlen == 2
--     then addResult $ Quote RawStr End
--     else many (choice [seg, linebreak]) >> option_ ending
--     where segQuotes = try $ do
--               qs <- many (token '"')
--               qs <$ when (length qs == hlen) (raise SatisfyError)
--           seg  = addResult =<< Str . concat <$> some seg'
--           seg' = do
--               s <- lexRawStrSeg
--               q <- option mempty segQuotes
--               let s' = s <> q
--               when (null s') $ raise SatisfyError
--               return s'
--           ending = some (token '"') >> addResult (Quote RawStr End)
--           linebreak = newline >> addResult EOL
--
--
-- lexRawStrSeg :: SymbolLexing s m => m String
-- lexRawStrSeg = many $ choice [escape, strChar] where
--     escape  = token ('\\') *> option '\\' (satisfy (`elem` ['\\', '"', '\'']))
--     strChar = satisfy (not . (`elem` ['\n', '"']))
--
--
-- -- === Raw String === --
--
-- lexFmtStr :: SymbolLexing s m => Int -> m ()
-- lexFmtStr hlen = if hlen == 2
--     then addResult $ Quote FmtStr End
--     else many (choice [seg, linebreak, escape, embeded]) >> option_ ending
--     where segQuotes = try $ do
--               qs <- many (token '\'')
--               qs <$ when (length qs == hlen) (raise SatisfyError)
--           seg  = addResult =<< Str . concat <$> some seg'
--           seg' = do
--               s <- lexFmtStrSeg
--               q <- option mempty segQuotes
--               let s' = s <> q
--               when (null s') $ raise SatisfyError
--               return s'
--           ending    = some (token '\'') >> addResult (Quote FmtStr End)
--           linebreak = newline >> addResult EOL
--           escape    = token '\\' *> lexEscSeq
--           embeded   = token '{'  *> addResult (Block Begin) *> parseStrBlock hlen 1
--
--
-- lexFmtStrSeg :: SymbolLexing s m => m String
-- lexFmtStrSeg = many strChar where
--     strChar = satisfy (not . (`elem` ['\n', '\'', '\\', '{']))
--
-- parseStrBlock :: SymbolLexing s m => Int -> Int -> m ()
-- parseStrBlock i = \case
--     0 -> lexFmtStr i
--     n -> choice [blockEnd, subBlock, other] where
--              blockEnd = token '}' *> addResult (Block End)   *> parseStrBlock i (pred n)
--              subBlock = token '{' *> addResult (Block Begin) *> parseStrBlock i (succ n)
--              other    = lexEntryPoint *> parseStrBlock i n
--
--
-- -- Escape maps
--
-- esc1Map, esc2Map, esc3Map :: Map String Int
-- esc1Map = Char.ord <$> fromList [ ("a", '\a'), ("b", '\b'), ("f", '\f'), ("n", '\n'), ("r", '\r'), ("t", '\t'), ("v", '\v'), ("'", '\''), ("\"" , '"') ]
-- esc2Map = Char.ord <$> fromList [ ("BS", '\BS'), ("HT", '\HT'), ("LF", '\LF'), ("VT", '\VT'), ("FF", '\FF'), ("CR", '\CR'), ("SO", '\SO'), ("SI", '\SI'), ("EM", '\EM'), ("FS", '\FS'), ("GS", '\GS'), ("RS", '\RS'), ("US", '\US'), ("SP", '\SP') ]
-- esc3Map = Char.ord <$> fromList [ ("NUL", '\NUL'), ("SOH", '\SOH'), ("STX", '\STX'), ("ETX", '\ETX'), ("EOT", '\EOT'), ("ENQ", '\ENQ'), ("ACK", '\ACK'), ("BEL", '\BEL'), ("DLE", '\DLE'), ("DC1", '\DC1'), ("DC2", '\DC2'), ("DC3", '\DC3'), ("DC4", '\DC4'), ("NAK", '\NAK'), ("SYN", '\SYN'), ("ETB", '\ETB'), ("CAN", '\CAN'), ("SUB", '\SUB'), ("ESC", '\ESC'), ("DEL", '\DEL') ]
--
-- lexEscSeq :: SymbolLexing s m => m ()
-- lexEscSeq = numEsc <|> chrEcs <|> wrongEsc where
--     numEsc   = addResult =<< StrEsc NumStrEsc . read <$> some (satisfy isDecDigitChar)
--     chrEcs   = choice $ uncurry parseEsc <$> zip [1..] [esc1Map, esc2Map, esc3Map]
--     wrongEsc = addResult =<< StrWrongEsc . Char.ord <$> anyToken
--
-- parseEsc :: SymbolLexing s m => Int -> Map String Int -> m ()
-- parseEsc n m = do
--     s <- count n anyToken
--     case Map.lookup s m of
--         Just i  -> addResult $ StrEsc CharStrEsc i
--         Nothing -> raise SatisfyError
--
--
-- --------------------
-- -- === Config === --
-- --------------------
--
-- lexConfig :: SymbolLexing s m => m Symbol
-- lexConfig = many (token ' ') *> (lexMetadata {- <|> lexPragma -})
--
-- lexMetadata :: SymbolLexing s m => m Symbol
-- lexMetadata = Metadata . convert <$ tokens metadataHeader <* many (token ' ') <*> many (satisfy (/= '\n'))
--
-- lexComment :: SymbolLexing s m => m Symbol
-- lexComment = Doc <$> many (satisfy (/= '\n'))
--

-------------------
-- === Lexer === --
-------------------

-- === Symbol head char map === --

symmapSize :: Int
symmapSize = 200

symmap :: Vector (Parser Symbol)
symmap = Vector.generate symmapSize $ \i -> let c = Char.chr i in if

    -- -- Layouting
    | c == ';'          -> pure Terminator
    -- | c == ':'          -> addResult =<< handleColons . (c:) =<< many (token ':')
    -- | c == '{'          -> addResult $ Block Begin
    -- | c == '}'          -> addResult $ Block End
    -- | c == '('          -> addResult $ Group Begin
    -- | c == ')'          -> addResult $ Group End
    -- | c == markerBegin  -> addResult =<< (dropRecover' (pure $ Incorrect "Marker") $ Marker . read <$> some (satisfy isDecDigitChar) <* token markerEnd)
    -- | c == '\n'         -> addResult EOL
    -- | c == '\r'         -> addResult =<< EOL <$ option_ (token '\n')
    --
    -- Identifiers & Keywords
    | varHead  c        -> prepareVar  . Text.cons c <$> indentBody
    -- | consHead c        -> addResult =<< prepareCons . (c:) <$> indentBody
    --
    -- -- Operators
    -- | c == '@'          -> addResult TypeApp
    -- | c == '|'          -> addResult Merge
    -- | c == '.'          -> addResult =<< handleDots . (c:) =<< many (token '.')
    -- | c == '='          -> addResult =<< handleEqs  . (c:) =<< many (token '=')
    -- | c `elem` opChars  -> addResult =<< handleOp   . (c:) <$> many (satisfy isRegularOperatorChar) <*> many (token '=')
    --
    -- -- Literals
    -- | c == '['          -> addResult $ List Begin
    -- | c == ']'          -> addResult $ List End
    -- | c == ','          -> addResult $ Operator ","
    -- | c == '"'          -> lexRawStr =<< succ <$> counted_ (many (token '"')  <* addResult (Quote RawStr Begin))
    -- | c == '\''         -> lexFmtStr =<< succ <$> counted_ (many (token '\'') <* addResult (Quote FmtStr Begin))
    -- | c == '`'          -> addResult (Quote Native Begin) *> lexNatStr
    | decHead c         -> lexNumber c
    --
    -- -- Meta
    | c == '#'          -> Terminator <$ put @EntryPoint Something --addResult =<< handleHash . (c:) =<< many (token '#')
    --
    -- -- Utils
    | otherwise         -> unknownCharSym c
    --
    where between  a l r    = a >= l && a <= r
        --   decChars          = ['0' .. '9']
          decHead  c        = between c '0' '9'
          varHead  c        = between c 'a' 'z' || c == '_'
          consHead c        = between c 'A' 'Z'
          indentBody        = (<>) <$> takeWhile isIndentBodyChar <*> takeWhile (== '\'')
    --       prepareCons       = Cons . convert'
          prepareVar        = either Var              id . checkSpecialVar
    --       handleOp   op eqs = either (checkModOp eqs) id . checkSpecialOp  $ convert' op
    --       handleColons      = handleReps  [BlockStart, Typed]
    --       handleDots        = handleReps  [Accessor  , Range, Anything]
    --       handleEqs         = handleReps  [Assignment, Operator "=="]
    --       handleHash        = handleRepsM [pure Disable, lexComment, lexConfig]
    --       handleReps        = handleRepsM . fmap return
    --       handleRepsM ts s  = fromMaybe (return $ Unknown s) $ ts ^? ix (length s - 1)
    --       checkModOp eqs op = case eqs of
    --                               "="  -> Modifier op
    --                               []   -> Operator op
    --                               s    -> Unknown (convert' op <> s)
{-# NOINLINE symmap #-}
--
--
-- -- === Utils === --
--
--
unknownCharSym :: Char -> Parser Symbol
unknownCharSym = pure . Unknown . convert ; {-# INLINE unknownCharSym #-}


-- ----------------------------------
-- ----------------------------------
--
--
lexSymChar :: Char -> Parser Symbol
lexSymChar c = if chord < symmapSize then Vector.unsafeIndex symmap chord else unknownCharSym c
    where chord = Char.ord c
{-# INLINE lexSymChar #-}

lexEntryPoint :: Parser Symbol
lexEntryPoint = get @EntryPoint >>= \case
    GlobalEntry -> anyToken >>= lexSymChar
{-# INLINE lexEntryPoint #-}

lexer :: Parser (Symbol, Int)
lexer = lexeme lexEntryPoint ; {-# INLINE lexer #-}

lexeme :: Parser a -> Parser (a, Int)
lexeme p = (,) <$> p <*> spacing ; {-# INLINE lexeme #-}

spacing :: Parser Int
spacing = sum <$> many (spaces <|> tabs) where
    spaces = Text.length        <$> takeWhile1 (== ' ')
    tabs   = (4*) . Text.length <$> takeWhile1 (== '\t')
{-# INLINE spacing #-}

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

data LexerToken s = LexerToken
    { _span    :: !Delta
    , _offset  :: !Delta
    , _element :: !s
    } deriving (Generic, Show, Eq, Ord, Functor, Foldable, Traversable) -- FIXME[WD]: Ord is needed by some weird Megaparsec fundep. Remove it when possible.
makeLenses ''LexerToken


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
