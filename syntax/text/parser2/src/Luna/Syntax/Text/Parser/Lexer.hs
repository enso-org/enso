{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE UndecidableInstances      #-}

module Luna.Syntax.Text.Parser.Lexer where

import qualified Prelude  as P
import           Prologue hiding (Text, imp, seq, some, takeWhile)
import qualified Prologue

import qualified Control.Monad.State.Layered               as State
import qualified Data.Attoparsec.Internal.Types            as AttoParsec
import qualified Data.Attoparsec.Text32                    as Parsec
import qualified Data.Char                                 as Char
import qualified Data.Graph.Data.Layer.Layout              as Layout
import qualified Data.Text.Position                        as Position
import qualified Data.Text.Span                            as Span
import qualified Data.Text32                               as Text
import qualified Data.Vector                               as Vector
import qualified Luna.IR.Layer                             as Layer
import qualified Luna.IR.Term.Ast.Invalid                  as Invalid
import qualified Luna.Syntax.Text.Lexer                    as Lexer
import qualified Luna.Syntax.Text.Lexer.Symbol             as Lexer
import qualified Luna.Syntax.Text.Parser.Data.Ast          as Ast
import qualified Luna.Syntax.Text.Parser.Data.Ast.Class    as Atom
import qualified Luna.Syntax.Text.Parser.Data.CodeSpan     as CodeSpan
import qualified Luna.Syntax.Text.Parser.State.Marker      as Marker
import qualified Luna.Syntax.Text.Parser.State.TokenStream as TokenStream
import qualified Luna.Syntax.Text.Scope                    as Scope
import qualified Text.Parser.State.Indent                  as Indent

import Control.Monad.State.Layered               (StatesT)
import Data.Parser                               hiding (Result, Token,
                                                  endOfInput, newline)
import Data.Text.Position                        (FileOffset (..))
import Data.Text.Position                        (Delta)
import Data.Text.Position                        (Position)
import Data.Vector                               (Vector)
import Luna.Syntax.Text.Parser.Data.Ast.Spanned  (Spanned (Spanned))
import Luna.Syntax.Text.Parser.Data.CodeSpan     (CodeSpan (CodeSpan))
import Luna.Syntax.Text.Parser.State.LastOffset  (LastOffset (LastOffset))
import Luna.Syntax.Text.Parser.State.TokenStream (TokenStream)
import OCI.Data.Name                             (Name)
import Text.Parser.Combinators                   (some)
import Text.Parser.State.Indent                  (Indent)

import Data.Parser.Instances.Attoparsec ()





type Text = Text.Text32

type Token = Ast.Spanned Ast.Ast
type UnspannedAst = Ast.Ast





-- type Text = Text.Text32
data SyntaxVersion = Syntax1 | Syntax2 deriving (Show)






type Lexer = StatesT
   '[ TokenStream
    , SyntaxVersion
    , Indent
    , Position
    , LastOffset
    , Marker.State
    , FileOffset
    , Scope.Scope
    ] Parsec.Parser



class Monad m => KnownParserOffset m where
    getParserOffset :: m Delta

instance KnownParserOffset Parsec.Parser where
    getParserOffset = AttoParsec.Parser $ \t pos more _ succ ->
        succ t pos more (convert $! AttoParsec.fromPos pos)
    {-# INLINE getParserOffset #-}

instance (MonadTrans t, Monad (t m), KnownParserOffset m)
      => KnownParserOffset (t m) where
    getParserOffset = lift getParserOffset
    {-# INLINE getParserOffset #-}










-------------------
-- === Spans === --
-------------------

getLastOffset :: State.Getter LastOffset m => m Delta
getLastOffset = unwrap <$> State.get @LastOffset
{-# INLINE getLastOffset #-}

putLastOffset :: State.Setter LastOffset m => Delta -> m ()
putLastOffset = State.put @LastOffset . wrap
{-# INLINE putLastOffset #-}

spanned :: Lexer a -> Lexer (CodeSpan, a)
spanned = \parser -> do
    lastEndOff <- getLastOffset
    startOff   <- getParserOffset
    out        <- parser
    endOff     <- getParserOffset
    nextOff    <- whiteSpace
    putLastOffset endOff

    let spacing   = startOff - lastEndOff
        bodyLen   = endOff - startOff
        totalSpan = bodyLen + nextOff
        realSpan  = Span.leftSpacedSpan spacing bodyLen

    Position.incColumn totalSpan
    pure (CodeSpan realSpan realSpan, out)
{-# INLINE spanned #-}

newline :: Lexer ()
newline = do
    len <- eol
    off <- whiteSpace
    Position.succLine
    Position.incColumn off
{-# INLINE newline #-}

lexeme :: Lexer a -> Lexer a
lexeme = \p -> p <* whiteSpace
{-# INLINE lexeme #-}

whiteSpace :: Lexer Delta
whiteSpace = convert . Text.length <$> takeMany ' '
{-# INLINE whiteSpace #-}

computeSpan :: Lexer a -> Lexer (Spanned a)
computeSpan = \p -> uncurry Spanned <$> spanned p
{-# INLINE computeSpan #-}

eol :: Lexer Delta
eol = (1 <$ n) <|> (2 <$ rn) <|> (1 <$ r) where
    n  = token '\n'
    r  = token '\r'
    rn = r >> n
{-# INLINE eol #-}

isEolBeginChar :: Char -> Bool
isEolBeginChar = (`elem` eolStartChars)
{-# INLINE isEolBeginChar #-}

eolStartChars :: [Char]
eolStartChars = ['\n', '\r', '\ETX']
{-# INLINE eolStartChars #-}






-- TODO: Can we do better?
instance Convertible Text.Text32 Name where
    convert = convertVia @String ; {-# INLINE convert #-}





trimIndent :: [Text] -> Text
trimIndent = \texts -> let
    indents = Text.length . Text.takeWhile (== ' ') <$> texts
    indent  = if null indents then 0 else unsafeMinimum indents
    texts'  = Text.drop indent <$> texts
    in intercalate "\n" texts'
{-# NOINLINE trimIndent #-}








-------------------------------
-- === Syntax management === --
-------------------------------

syntax1Only :: Lexer a -> Lexer a
syntax1Only = \p -> State.get @SyntaxVersion >>= \case
    Syntax1 -> p
    Syntax2 -> fail "syntax 1 only"
{-# INLINE syntax1Only #-}



---------------------
-- === Helpers === --
---------------------

symbol :: Text -> Lexer Text
symbol = lexeme . tokens
{-# INLINE symbol #-}

invalid :: Lexer Invalid.Symbol -> Lexer Token
invalid = computeSpan . invalid'
{-# INLINE invalid #-}

invalid' :: Lexer Invalid.Symbol -> Lexer UnspannedAst
invalid' = fmap Ast.Invalid
{-# INLINE invalid' #-}

unknown :: Lexer Token
unknown = invalid $ do
    txt <- unknownChunk
    -- Ast.checkBlacklistedUnknown $ Text.head txt
    pure $ Invalid.Unknown
{-# INLINE unknown #-}

gatherWith1 :: (Alternative m, Mempty a) => (b -> a) -> m (a -> b) -> m b
gatherWith1 = \f p -> let go = p <*> option mempty (f <$> go) in go
{-# INLINE gatherWith1 #-}

gatherMany1 :: (Alternative m) => m ([a] -> NonEmpty a) -> m (NonEmpty a)
gatherMany1 = gatherWith1 convert
{-# INLINE gatherMany1 #-}

anyLine :: Lexer Text
anyLine = takeWhile $ not . isEolBeginChar
{-# INLINE anyLine #-}

unknownChunk :: Lexer Text
unknownChunk = Text.cons <$> anyToken <*> takeWhile (not . isSeparatorChar)
{-# INLINE unknownChunk #-}

chunk :: Lexer Text
chunk = takeWhile1 (not . isSeparatorChar)
{-# INLINE chunk #-}

-- chunkOfNot :: [Char] -> Lexer Int
-- chunkOfNot s = Text.length <$> takeWhile1 (not . (`elem` break)) where
--     break = ' ' : (s <> Ast.eolStartChars)
-- {-# INLINE chunkOfNot #-}



-------------------------
-- === Identifiers === --
-------------------------

-- === Getting values === --

var, cons :: Lexer ()
var  = TokenStream.add =<< computeSpan var'
cons = TokenStream.add =<< computeSpan cons'
{-# INLINE var  #-}
{-# INLINE cons #-}

wildcard :: Lexer ()
wildcard = TokenStream.add =<< computeSpan wildcard'
{-# INLINE wildcard #-}

var', cons' :: Lexer UnspannedAst
var'  = (Ast.Var  <$> varName)  <**> option id (const <$> invalidIdentSuffix)
cons' = (Ast.Cons <$> consName) <**> option id (const <$> invalidIdentSuffix)
{-# INLINE var'  #-}
{-# INLINE cons' #-}

wildcard' :: Lexer UnspannedAst
wildcard' = correct <**> option id (const <$> invalidIdentSuffix) where
    correct = Ast.Wildcard <$ token '_'
{-# INLINE wildcard' #-}

isIdentBodyChar, isVarHead, isConsHead :: Char -> Bool
isIdentBodyChar = \c -> Char.isAlphaNum c
isVarHead       = \c -> Char.isLower    c || c == '_'
isConsHead      = Char.isUpper
{-# INLINE isIdentBodyChar  #-}
{-# INLINE isVarHead        #-}
{-# INLINE isConsHead       #-}


-- === Expecting values === --

expectVar :: Name -> Lexer ()
expectVar = \name -> let err = fail "no match" in var' >>= \case
    Ast.Var name' -> when_ (name /= name') err
    _             -> err
{-# INLINE expectVar #-}

expectCons :: Name -> Lexer ()
expectCons = \name -> let err = fail "no match" in cons' >>= \case
    Ast.Cons name' -> when_ (name /= name') err
    _              -> err
{-# INLINE expectCons #-}

expectOperator :: Name -> Lexer ()
expectOperator = \name -> let err = fail "no match" in operator' >>= \case
    Ast.Operator name' -> when_ (name /= name') err
    _                  -> err
{-# INLINE expectOperator #-}


-- === Names === --

varName :: Lexer Name
varName = convert . (<>) <$> header <*> identBody where
    header = Text.snoc <$> pfx <*> head
    pfx    = takeMany '_'
    head   = satisfy Char.isLower
{-# INLINE varName #-}

consName :: Lexer Name
consName = convert . Text.cons <$> header <*> identBody where
    header = satisfy Char.isUpper
{-# INLINE consName #-}

identBody :: Lexer Text
identBody = body <**> sfx where
    body  = takeWhile isIdentBodyChar
    sfx   = option id (flip (<>) <$> takeMany1 '\'')
{-# INLINE identBody #-}


-- === Validation === --

-- | We are not using full (33-126 / ['"`]) range here, because we need to check
--   such invalid suffixes as `foo'a`.
invalidIdentSuffix :: Lexer UnspannedAst
invalidIdentSuffix = Ast.Invalid . Invalid.UnexpectedSuffix <$> base where
    base = Text.length <$> takeWhile1 (not . isSeparatorChar)
{-# INLINE invalidIdentSuffix #-}

isSeparatorChar :: Char -> Bool
isSeparatorChar = \c ->
    let ord = Char.ord c
        cat = Char.generalCategory c
    in (ord >= 7 && ord <= 47 && ord /= 34 && ord /= 39) -- !\#$%&()*+,-./
                                                         -- 0 - 9
    || (ord >= 58 && ord <= 64)                          -- :;<=>?@
                                                         -- A - Z
    || (ord >= 91 && ord <= 94)                          -- [\\]^
                                                         -- _`a-z
    || (ord >= 123 && ord <= 126)                        -- {|}~
    || (cat == Char.Space)
    || (cat == Char.Control)
    || (cat == Char.MathSymbol)
{-# INLINE isSeparatorChar #-}



-----------------------
-- === Operators === --
-----------------------

-- === API === --

operator :: Lexer ()
operator = TokenStream.add =<< computeSpan operator'
{-# NOINLINE operator #-}

operator' :: Lexer UnspannedAst
operator' = let
    base       = convert <$> takeWhile1 isOperatorBodyChar
    specialOps = tokens <$> ["<=", ">=", "==", "=", ".."]
    dots       = Ast.Operator . convert <$> choice (tokens <$> ["."])
    comma      = Ast.Operator . convert <$> token ','
    special    = Ast.Operator . convert <$> choice specialOps
    normal     = base <**> option Ast.Operator (Ast.Modifier <$ token eqChar)
    correct    = special <|> normal
    in (correct <**> option id (const <$> invalidOperatorSuffix))
       <|> dots <|> comma
{-# NOINLINE operator' #-}

unsafeAnyTokenOperator :: Lexer ()
unsafeAnyTokenOperator = TokenStream.add
    =<< computeSpan (Ast.Operator . convert <$> anyToken)
{-# INLINE unsafeAnyTokenOperator #-}

eqChar :: Char
eqChar = '='
{-# INLINE eqChar #-}

isOpenCloseChar :: Char -> Bool
isOpenCloseChar = (`elem` ("(){}[]" :: [Char]))


-- === Helpers === --

isOperatorBodyChar :: Char -> Bool
isOperatorBodyChar = \c
    -> c /= eqChar
    && c /= '.'
    && c /= ','
    && isOperatorBeginChar c
{-# INLINE isOperatorBodyChar #-}

isOperatorBeginChar :: Char -> Bool
isOperatorBeginChar = \c -> let
    ord   = Char.ord c
    check = (ord == 33)              -- !
         || (ord >= 36 && ord <= 38) -- $%&
         || (ord >= 42 && ord <= 47) -- *+,-./
         || (ord >= 58 && ord <= 63) -- :;<>=?
         || (ord == 92)              -- \
         || (ord == 94)              -- ^
         || (ord == 126)             -- ~
         || Char.generalCategory c == Char.MathSymbol
    in check
{-# INLINE isOperatorBeginChar #-}


-- === Validation === --

invalidOperatorSuffix :: Lexer UnspannedAst
invalidOperatorSuffix = let
    inv = Ast.Invalid . Invalid.UnexpectedSuffix <$> sfx
    sfx = Text.length <$> takeWhile1 isOperatorBeginChar
    in inv
{-# INLINE invalidOperatorSuffix #-}



----------------------
-- === Comments === --
----------------------

-- === API === --

comment :: Lexer ()
comment = TokenStream.add =<< computeSpan (commentChar >> parser) where
    commentChar = token commentStartChar
    body        = trimIndent <$> (multiLine <|> singleLine)
    multiLine   = commentChar *> flexBlock rawLine
    singleLine  = pure <$> rawLine
    rawLine     = takeWhile (not . isEolBeginChar)
    parser      = (Ast.Comment <$> body)
{-# INLINE comment #-}

-- comment :: Lexer ()
-- comment = parser where
--     commentChar    = token commentStartChar

--     multilineChars = (\c1 c2 -> [c1,c2]) <$> commentChar <*> commentChar
--     multiLineOp    = computeSpan (Ast.Operator . convert <$> multilineChars)
--     multiLineStart = TokenStream.add =<< multiLineOp

--     singleLineChars = (:[]) <$> commentChar
--     singleLineOp    = computeSpan (Ast.Operator . convert <$> singleLineChars)
--     singleLineStart = TokenStream.add =<< singleLineOp

--     rawLine     = takeWhile (not . isEolBeginChar)
--     multiLine   = multiLineStart *> flexBlock rawLine
--     singleLine  = singleLineStart *> (pure <$> rawLine)
--     body        = trimIndent <$> (multiLine <|> singleLine)
--     parser      = TokenStream.add =<< computeSpan (Ast.Comment <$> body)
-- {-# INLINE comment #-}

commentStartChar :: Char
commentStartChar = '#'
{-# INLINE commentStartChar #-}

isCommentStartChar :: Char -> Bool
isCommentStartChar = (== commentStartChar)
{-# INLINE isCommentStartChar #-}



---------------------
-- === Markers === --
---------------------

markerBegin, markerEnd :: Char
markerBegin = '«'
markerEnd   = '»'
{-# INLINE markerBegin #-}
{-# INLINE markerEnd   #-}

isMarkerBeginChar :: Char -> Bool
isMarkerBeginChar = (== markerBegin)
{-# INLINE isMarkerBeginChar #-}

marker :: Lexer ()
marker = TokenStream.add . (Ast.span %~ CodeSpan.asPhantom) =<< computeSpan parser where
    correct   = Ast.Marker <$> decimal
    incorrect = Ast.Invalid Invalid.InvalidMarker <$ takeTill (== markerEnd)
    parser    = token markerBegin *> (correct <|> incorrect) <* token markerEnd
{-# INLINE marker #-}



-------------------------
-- === Expressions === --
-------------------------

-- === API === --

expr :: Lexer ()
expr = fastExprByChar =<< peekToken
{-# INLINE expr #-}

exprs :: Lexer ()
exprs = let
    parser = do
        whiteSpace
        many expr
        token '\ETX'
        pure ()
        -- Ast.tokens' . unwrap <$> State.get @Ast.Result
    -- in State.put @Ast.Result . wrap . pure =<< computeSpan parser
    in parser
{-# NOINLINE exprs #-}


unknownExpr :: Lexer ()
unknownExpr = TokenStream.add =<< unknown
{-# INLINE unknownExpr #-}

lineBreak :: Lexer ()
lineBreak = TokenStream.add =<< ast where
    ast = computeSpan $ do
        some newline
        Ast.LineBreak <$> Position.getColumn
{-# INLINE lineBreak #-}


-- === Fast ASCII Lookup Table === --

-- | The size of `symmap` - Vector-based map from head Char to related parser.
lookupTableSize :: Int
lookupTableSize = 200
{-# INLINE lookupTableSize #-}

exprLookupTable :: Vector (Lexer ())
exprLookupTable = Vector.generate lookupTableSize $ exprByChar . Char.chr

exprByChar :: Char -> Lexer ()
exprByChar = \c -> if
    | isOperatorBeginChar c -> operator
    | isEolBeginChar      c -> lineBreak
    | isOpenCloseChar     c -> unsafeAnyTokenOperator
    | isVarHead           c -> var <|> wildcard
    | isConsHead          c -> cons
    | isCommentStartChar  c -> comment
    | isMarkerBeginChar   c -> marker

    -- Literals
    | isRawStrQuote       c -> strBuilder '"'
    | isFmtStrQuote       c -> strBuilder '\''
    | isDigitChar         c -> number

    -- Utils
    | otherwise         -> unknownExpr


-- | (1): fetch  parser for ASCII from precomputed cache
--   (2): choose parser for unicode characters on the fly
fastExprByChar :: Char -> Lexer ()
fastExprByChar = \c -> let ord = Char.ord c in if
    | ord < lookupTableSize -> Vector.unsafeIndex exprLookupTable ord -- (1)
    | otherwise             -> exprByChar c                           -- (2)
{-# INLINE fastExprByChar #-}



number :: Lexer ()
number = TokenStream.add =<< computeSpan parser where
    parser  = correct <**> option id (const <$> invalidIdentSuffix)
    correct = Ast.Number <$> digits
{-# INLINE number #-}

isDigitChar :: Char -> Bool
isDigitChar = \c -> c >= '0' && c <= '9'
{-# INLINE isDigitChar #-}

digitChars :: Lexer (NonEmpty Char)
digitChars = unsafeConv <$> takeWhile1 isDigitChar where
    unsafeConv :: Text -> NonEmpty Char
    unsafeConv = \txt -> case convertTo @[Char] txt of
        []     -> impossible
        (a:as) -> a :| as
{-# INLINE digitChars #-}

intDigits :: Lexer (NonEmpty Int)
intDigits = toDigit <<$>> digitChars where
    toDigit = subtract 48 . Char.ord
{-# INLINE intDigits #-}

digits :: Lexer (NonEmpty Word8)
digits = fromIntegral <<$>> intDigits
{-# INLINE digits #-}

decimal :: Lexer Int
decimal = unsafeRead . convert <$> digitChars
{-# INLINE decimal #-}


isRawStrQuote :: Char -> Bool
isRawStrQuote = (== '"')
{-# INLINE isRawStrQuote #-}

isFmtStrQuote :: Char -> Bool
isFmtStrQuote = (== '\'')
{-# INLINE isFmtStrQuote #-}


failIf :: Bool -> Lexer ()
failIf = \b -> if b then fail "Failed check" else pure ()
{-# INLINE failIf #-}

strBuilder :: Char -> Lexer ()
strBuilder quote = TokenStream.add =<< computeSpan parser where
    parser = do
        let isQuote      = (== quote)
            isBodyChar c = c == quote || isEolBeginChar c

        quoteLen <- Text.length <$> takeWhile1 isQuote

        let mkChunk       = \p -> TokenStream.add =<< computeSpan p
            chunkPlain    = Atom.StrPlain <$> takeWhile1 (not . isBodyChar)
            chunkQuote    = bodyQuotes =<< takeWhile1 isQuote
            bodyQuotes qs = Atom.StrPlain qs <$ failIf (Text.length qs == quoteLen)
            chunk         = computeSpan $ choice [chunkPlain, chunkQuote] -- lineBreak
            ending        = Text.length <$> takeWhile1 isQuote
            body          = Ast.Str <$> many chunk
        body <* ending




--------------------
-- === Layout === --
--------------------

flexBlock1 :: Lexer a -> Lexer (NonEmpty a)
flexBlock1 = \p -> let
    line = some newline >> Indent.indented >> p
    in some line
{-# INLINE flexBlock1 #-}

flexBlock :: Lexer a -> Lexer [a]
flexBlock = \p -> option mempty $ convert <$> flexBlock1 p
{-# INLINE flexBlock #-}
