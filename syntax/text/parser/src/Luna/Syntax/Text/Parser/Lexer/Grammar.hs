{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE UndecidableInstances      #-}

module Luna.Syntax.Text.Parser.Lexer.Grammar where

import qualified Prelude  as P
import           Prologue hiding (Text, imp, seq, takeWhile)
import qualified Prologue

import qualified Control.Monad.State.Layered               as State
import qualified Data.Attoparsec.Internal.Types            as Parsec
import qualified Data.Attoparsec.Text32                    as Parsec32
import qualified Data.Char                                 as Char
import qualified Data.Graph.Data.Layer.Layout              as Layout
import qualified Data.Parser                               as Parsec
import qualified Data.Text.Position                        as Position
import qualified Data.Text.Span                            as Span
import qualified Data.Text32                               as Source
import qualified Data.Vector                               as Vector
import qualified Luna.IR.Layer                             as Layer
import qualified Luna.IR.Term.Ast.Invalid                  as Invalid
import qualified Luna.Syntax.Text.Lexer                    as Lexer
import qualified Luna.Syntax.Text.Lexer.Symbol             as Lexer
import qualified Luna.Syntax.Text.Parser.Ast               as Ast
import qualified Luna.Syntax.Text.Parser.Ast.Class         as Atom
import qualified Luna.Syntax.Text.Parser.Ast.CodeSpan      as CodeSpan
import qualified Luna.Syntax.Text.Parser.Lexer.Names       as Names
import qualified Luna.Syntax.Text.Parser.State.Marker      as Marker
import qualified Luna.Syntax.Text.Parser.State.TokenStream as TokenStream
import qualified Luna.Syntax.Text.Parser.State.Version     as Syntax
import qualified Luna.Syntax.Text.Scope                    as Scope
import qualified Text.Parser.State.Indent                  as State.Indent
import qualified Text.Parser.State.Indent                  as Indent

import Control.Monad.State.Layered               (StatesT)
import Data.Parser                               (choice, option, takeMany,
                                                  takeMany1, takeTill,
                                                  takeWhile, takeWhile1)
import Data.Text.Position                        (FileOffset (..))
import Data.Text.Position                        (Delta)
import Data.Text.Position                        (Position)
import Data.Text32                               (Text32)
import Data.Vector                               (Vector)
import Luna.Syntax.Text.Parser.Ast.CodeSpan      (CodeSpan (CodeSpan))
import Luna.Syntax.Text.Parser.Ast.Spanned       (Ast, Spanned (Spanned))
import Luna.Syntax.Text.Parser.State.LastOffset  (LastOffset (LastOffset))
import Luna.Syntax.Text.Parser.State.TokenStream (TokenStream)
import Luna.Syntax.Text.Scope                    (Scope)
import OCI.Data.Name                             (Name)
import Text.Parser.Combinators                   (many1, many1')
import Text.Parser.State.Indent                  (Indent)

import Data.Parser.Instances.Attoparsec ()





-------------------
-- === Types === --
-------------------

type Source = Text32
type Token  = Ast.Spanned Ast



-----------------------------
-- === Attoparsec hack === --
-----------------------------

-- | For more info see: https://github.com/bos/attoparsec/issues/101

class Monad m => KnownParserOffset m where
    getParserOffset :: m Delta

instance KnownParserOffset Parsec32.Parser where
    getParserOffset = Parsec.Parser $ \t pos more _ succ ->
        succ t pos more (convert $! Parsec.fromPos pos)
    {-# INLINE getParserOffset #-}

instance (MonadTrans t, Monad (t m), KnownParserOffset m)
      => KnownParserOffset (t m) where
    getParserOffset = lift getParserOffset
    {-# INLINE getParserOffset #-}



-------------------
-- === Utils === --
-------------------

-- TODO: Can we do better?
instance Convertible Source.Text32 Name where
    convert = convertVia @String
    {-# INLINE convert #-}

trimLinesLeft :: [Source] -> Source
trimLinesLeft = \texts -> let
    indents = Source.length . Source.takeWhile (== ' ') <$> texts
    indent  = if null indents then 0 else unsafeMinimum indents
    texts'  = Source.drop indent <$> texts
    in intercalate "\n" texts'
{-# NOINLINE trimLinesLeft #-}



-------------------
-- === Lexer === --
-------------------

-- === Definition === --

type Lexer_ = Lexer ()
type Lexer  = StatesT
   '[ TokenStream
    , Syntax.Version
    , Indent
    , Position
    , LastOffset
    , FileOffset
    , Scope.Scope
    ] Parsec32.Parser


-- === Evaluation === --

evalVersion1 :: Source -> [Token]
evalVersion1 = eval Syntax.Version1
{-# INLINE evalVersion1 #-}

eval :: Syntax.Version -> Source -> [Token]
eval = flip evalWith exprs
{-# INLINE eval #-}

evalVersion1With :: Lexer a -> Source -> [Token]
evalVersion1With = evalWith Syntax.Version1
{-# INLINE evalVersion1With #-}

evalWith :: Syntax.Version -> Lexer a -> Source -> [Token]
evalWith = \sv p src_ -> let
    src        = (src_ <> "\ETX")
    panic  s   = error $ "Panic. Not all input consumed by lexer: " <> show s
    finish s r = if (Source.length s /= 0) then panic s else r
    parser     = p <* Parsec.token '\ETX'
    in case evalStepWith sv parser src of
        Parsec.Fail _ _ e  -> error e
        Parsec.Done src' r -> finish src' r
        Parsec.Partial g   -> case g mempty of
            Parsec.Done src' r -> finish src' r
            Parsec.Fail _ _ e  -> error e
            Parsec.Partial {}  -> impossible
{-# INLINE evalWith #-}

evalStepWith
    :: Syntax.Version -> Lexer a -> Source -> Parsec.IResult Source [Token]
evalStepWith = \sv p txt
   -> flip Parsec.parsePartial txt
    . State.evalDefT @Scope
    . State.evalDefT @FileOffset
    . State.evalDefT @LastOffset
    . State.evalDefT @Position
    . State.Indent.eval
    . flip (State.evalT @Syntax.Version) sv
    . TokenStream.eval
    $ p
{-# INLINE evalStepWith #-}


-- === Syntax management === --

syntax1Only :: Lexer a -> Lexer a
syntax1Only = \p -> State.get @Syntax.Version >>= \case
    Syntax.Version1 -> p
    Syntax.Version2 -> fail "syntax 1 only"
{-# INLINE syntax1Only #-}


-- === Sanity checking === --

-- | Luna Backend uses this functionality

isCorrectOutput :: [Token] -> Bool
isCorrectOutput toks = all (== True) $ checkToken <$> toks where
    checkToken t = case Ast.unspan t of
        Ast.Invalid {} -> False
        _              -> True

isCorrectPattern :: [Token] -> Bool
isCorrectPattern toks = all (== True) $ checkToken <$> toks where
    checkToken t = case Ast.unspan t of
        Ast.Operator "=" -> False
        _                -> True

isSingleVar :: [Token] -> Bool
isSingleVar ts = case Ast.unspan <$> ts of
    [Ast.Var {}] -> True
    _            -> False



-------------------------------
-- === Primitive parsers === --
-------------------------------

-- === Spans === --

spanned :: Lexer a -> Lexer (Spanned a)
spanned = \p -> uncurry Spanned <$> computeSpan p
{-# INLINE spanned #-}

computeSpan :: Lexer a -> Lexer (CodeSpan, a)
computeSpan = \parser -> do
    lastEndOff <- getLastOffset
    startOff   <- getParserOffset
    putLastOffset startOff -- | For nested span computations
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
{-# INLINE computeSpan #-}

getLastOffset :: State.Getter LastOffset m => m Delta
getLastOffset = unwrap <$> State.get @LastOffset
{-# INLINE getLastOffset #-}

putLastOffset :: State.Setter LastOffset m => Delta -> m ()
putLastOffset = State.put @LastOffset . wrap
{-# INLINE putLastOffset #-}


-- === Layouting === --

newline :: Lexer_
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
whiteSpace = convert . Source.length <$> takeMany ' '
{-# INLINE whiteSpace #-}

eol :: Lexer_
eol = n <|> rn where
    n  = void $ Parsec.token '\n'
    r  = void $ Parsec.token '\r'
    rn = r <* option () n
{-# INLINE eol #-}

isEolBeginChar :: Char -> Bool
isEolBeginChar = (`elem` eolStartChars)
{-# INLINE isEolBeginChar #-}

eolStartChars :: [Char]
eolStartChars = ['\n', '\r', '\ETX']
{-# INLINE eolStartChars #-}



---------------------
-- === Helpers === --
---------------------

invalid :: Lexer Invalid.Symbol -> Lexer Token
invalid = spanned . invalid'
{-# INLINE invalid #-}

invalid' :: Lexer Invalid.Symbol -> Lexer Ast
invalid' = fmap Ast.Invalid
{-# INLINE invalid' #-}

unknown :: Lexer Token
unknown = invalid $ Invalid.Unknown <$ unknownChunk
{-# INLINE unknown #-}

unknownChunk :: Lexer Source
unknownChunk
    = Source.cons <$> Parsec.anyToken <*> takeWhile (not . isSeparatorChar)
{-# INLINE unknownChunk #-}

chunk :: Lexer Source
chunk = takeWhile1 (not . isSeparatorChar)
{-# INLINE chunk #-}



-------------------------
-- === Identifiers === --
-------------------------

-- === API === --

var, cons :: Lexer_
var  = TokenStream.add =<< spanned var'
cons = TokenStream.add =<< spanned cons'
{-# INLINE var  #-}
{-# INLINE cons #-}

wildcard :: Lexer_
wildcard = TokenStream.add =<< spanned wildcard'
{-# INLINE wildcard #-}

var', cons' :: Lexer Ast
var'  = (Ast.Var  <$> varName)  <**> option id (const <$> invalidIdentSuffix)
cons' = (Ast.Cons <$> consName) <**> option id (const <$> invalidIdentSuffix)
{-# INLINE var'  #-}
{-# INLINE cons' #-}

wildcard' :: Lexer Ast
wildcard' = correct <**> option id (const <$> invalidIdentSuffix) where
    correct = Ast.Wildcard <$ Parsec.token Names.wildcard
{-# INLINE wildcard' #-}



-- === Expecting values === --

noMatchFail :: Lexer_
noMatchFail = fail "no match"
{-# INLINE noMatchFail #-}

expectVar :: Name -> Lexer_
expectVar = \name -> let err = noMatchFail in var' >>= \case
    Ast.Var name' -> when_ (name /= name') err
    _             -> err
{-# INLINE expectVar #-}

expectCons :: Name -> Lexer_
expectCons = \name -> let err = noMatchFail in cons' >>= \case
    Ast.Cons name' -> when_ (name /= name') err
    _              -> err
{-# INLINE expectCons #-}

expectOperator :: Name -> Lexer_
expectOperator = \name -> let err = noMatchFail in operator' >>= \case
    Ast.Operator name' -> when_ (name /= name') err
    _                  -> err
{-# INLINE expectOperator #-}


-- === Names === --

varName :: Lexer Name
varName = convert . (<>) <$> header <*> identBody where
    header = Source.snoc <$> pfx <*> head
    pfx    = takeMany Names.varNamePfxChar
    head   = Parsec.satisfy Char.isLower
{-# INLINE varName #-}

consName :: Lexer Name
consName = convert . Source.cons <$> header <*> identBody where
    header = Parsec.satisfy Char.isUpper
{-# INLINE consName #-}

identBody :: Lexer Source
identBody = body <**> sfx where
    body  = takeWhile Names.isIdentBodyChar
    sfx   = option id (flip (<>) <$> takeMany1 Names.identBodySfxChar)
{-# INLINE identBody #-}


-- === Validation === --

-- | We are not using full (33-126 / ['"`]) range here, because we need to check
--   such invalid suffixes as `foo'a`.
invalidIdentSuffix :: Lexer Ast
invalidIdentSuffix = Ast.Invalid . Invalid.UnexpectedSuffix <$> base where
    base = Source.length <$> takeWhile1 (not . isSeparatorChar)
{-# INLINE invalidIdentSuffix #-}

isSeparatorChar :: Char -> Bool
isSeparatorChar = \c ->
    let ord = Char.ord c
        cat = Char.generalCategory c
    in (ord >= 7 && ord <= 47 && ord /= 34 && ord /= 39) -- (+) !\#$%&()*+,-./
                                                         -- (-) 0 - 9
    || (ord >= 58 && ord <= 64)                          -- (+) :;<=>?@
                                                         -- (-) A - Z
    || (ord >= 91 && ord <= 94)                          -- (+) [\\]^
                                                         -- (-) _`a-z
    || (ord >= 123 && ord <= 126)                        -- (+) {|}~
    || (cat == Char.Space)
    || (cat == Char.Control)
    || (cat == Char.MathSymbol)
{-# INLINE isSeparatorChar #-}



-----------------------
-- === Operators === --
-----------------------

-- === API === --

operator :: Lexer_
operator = TokenStream.add =<< spanned operator'
{-# NOINLINE operator #-}

operator' :: Lexer Ast
operator' = let
    base       = convert <$> takeWhile1 isOperatorBodyChar
    specialOps = Parsec.tokens <$> ["<=", ">=", "==", "=", ".."]
    dots       = Ast.Operator . convert <$> choice (Parsec.tokens <$> ["."])
    comma      = Ast.Operator . convert <$> Parsec.token ','
    special    = Ast.Operator . convert <$> choice specialOps
    correct    = special <|> normal
    normal     = base <**> option Ast.Operator
                 (Ast.Modifier <$ Parsec.token eqChar)
    in (correct <**> option id (const <$> invalidOperatorSuffix))
       <|> dots <|> comma
{-# NOINLINE operator' #-}

unsafeAnyTokenOperator :: Lexer_
unsafeAnyTokenOperator = TokenStream.add
    =<< spanned (Ast.Operator . convert <$> Parsec.anyToken)
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

invalidOperatorSuffix :: Lexer Ast
invalidOperatorSuffix = let
    inv = Ast.Invalid . Invalid.UnexpectedSuffix <$> sfx
    sfx = Source.length <$> takeWhile1 isOperatorBeginChar
    in inv
{-# INLINE invalidOperatorSuffix #-}



----------------------
-- === Comments === --
----------------------

-- === API === --

comment :: Lexer_
comment = TokenStream.add =<< spanned (commentChar >> parser) where
    commentChar = Parsec.token commentStartChar
    body        = trimLinesLeft <$> (multiLine <|> singleLine)
    multiLine   = commentChar *> flexBlock rawLine
    singleLine  = pure <$> rawLine
    rawLine     = takeWhile (not . isEolBeginChar)
    parser      = (Ast.Comment <$> body)
{-# INLINE comment #-}

metadata :: Lexer_
metadata = TokenStream.add =<< spanned parser where
    parser = Ast.Metadata <$ header <*> body
    header = Parsec.tokens "### " *> Parsec.tokens "META"
    body   = takeWhile (not . isEolBeginChar)
{-# INLINE metadata #-}

-- comment :: Lexer_
-- comment = parser where
--     commentChar    = Parsec.token commentStartChar

--     multilineChars = (\c1 c2 -> [c1,c2]) <$> commentChar <*> commentChar
--     multiLineOp    = spanned (Ast.Operator . convert <$> multilineChars)
--     multiLineStart = TokenStream.add =<< multiLineOp

--     singleLineChars = (:[]) <$> commentChar
--     singleLineOp    = spanned (Ast.Operator . convert <$> singleLineChars)
--     singleLineStart = TokenStream.add =<< singleLineOp

--     rawLine     = takeWhile (not . isEolBeginChar)
--     multiLine   = multiLineStart *> flexBlock rawLine
--     singleLine  = singleLineStart *> (pure <$> rawLine)
--     body        = trimLinesLeft <$> (multiLine <|> singleLine)
--     parser      = TokenStream.add =<< spanned (Ast.Comment <$> body)
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

isMarkerBeginChar :: Char -> Bool
isMarkerBeginChar = (== Names.markerBegin)
{-# INLINE isMarkerBeginChar #-}

marker :: Lexer_
marker = TokenStream.add . (Ast.span %~ CodeSpan.asPhantom) =<< spanned parser where
    correct   = Ast.Marker <$> decimal
    incorrect = Ast.Invalid Invalid.InvalidMarker <$ takeTill (== Names.markerEnd)
    parser    = Parsec.token Names.markerBegin
             *> (correct <|> incorrect) <* Parsec.token Names.markerEnd
{-# INLINE marker #-}



-------------------------
-- === Expressions === --
-------------------------

-- === API === --

expr :: Lexer_
expr = fastExprByChar =<< Parsec.peekToken
{-# INLINE expr #-}

exprs :: Lexer_
exprs = do
    whiteSpace
    many expr
    pure ()
{-# NOINLINE exprs #-}



unknownExpr :: Lexer_
unknownExpr = TokenStream.add =<< unknown
{-# INLINE unknownExpr #-}

lineBreak :: Lexer_
lineBreak = TokenStream.add =<< ast where
    ast = spanned $ do
        many1 newline
        Ast.LineBreak <$> Position.getColumn
{-# INLINE lineBreak #-}


-- === Fast ASCII Lookup Table === --

-- | The size of `symmap` - Vector-based map from head Char to related parser.
lookupTableSize :: Int
lookupTableSize = 200
{-# INLINE lookupTableSize #-}

exprLookupTable :: Vector (Lexer_)
exprLookupTable = Vector.generate lookupTableSize $ exprByChar . Char.chr

exprByChar :: Char -> Lexer_
exprByChar = \c -> if
    | isOperatorBeginChar  c -> operator
    | isEolBeginChar       c -> lineBreak
    | isOpenCloseChar      c -> unsafeAnyTokenOperator
    | Names.isVarHeadChar  c -> var <|> wildcard
    | Names.isConsHeadChar c -> cons
    | isCommentStartChar   c -> metadata <|> comment
    | isMarkerBeginChar    c -> marker

    -- Literals
    | isRawStrQuote       c -> strBuilder '"'
    | isFmtStrQuote       c -> strBuilder '\''
    | isDigitChar         c -> number

    -- Utils
    | otherwise         -> unknownExpr


-- | (1): fetch  parser for ASCII from precomputed cache
--   (2): choose parser for unicode characters on the fly
fastExprByChar :: Char -> Lexer_
fastExprByChar = \c -> let ord = Char.ord c in if
    | ord < lookupTableSize -> Vector.unsafeIndex exprLookupTable ord -- (1)
    | otherwise             -> exprByChar c                           -- (2)
{-# INLINE fastExprByChar #-}



number :: Lexer_
number = TokenStream.add =<< spanned parser where
    parser  = correct <**> option id (const <$> invalidIdentSuffix)
    correct = Ast.Number <$> digits
{-# INLINE number #-}

isDigitChar :: Char -> Bool
isDigitChar = \c -> c >= '0' && c <= '9'
{-# INLINE isDigitChar #-}

digitChars :: Lexer (NonEmpty Char)
digitChars = unsafeConv <$> takeWhile1 isDigitChar where
    unsafeConv :: Source -> NonEmpty Char
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


failIf :: Bool -> Lexer_
failIf = \b -> if b then fail "Failed check" else pure ()
{-# INLINE failIf #-}

strBuilder :: Char -> Lexer_
strBuilder quote = TokenStream.add =<< spanned parser where
    parser = do
        let isQuote      = (== quote)
            isBodyChar c = not $ isQuote c || isEolBeginChar c

        quoteLen <- Source.length <$> takeWhile1 isQuote

        let chunkBody    = takeWhile1 isBodyChar
            chunkQuote   = bodyQuotes =<< takeWhile1 isQuote
            bodyQuotes a = a <$ failIf (quoteLen == Source.length a)
            chunkPlain   = spanned $ Atom.StrPlain . mconcat <$> many1' (choice [chunkBody, chunkQuote])

            quotes       = void $ takeWhile1 isQuote
            end          = quotes <|> noEnd
            chunks       = many1' chunkPlain <|> (mergeStrChunks <$> strBlock chunkPlain)
            noEnd        = (TokenStream.add =<< invalid (pure $ Invalid.Literal $ Invalid.String Invalid.NoClosingMark))
            body         = Ast.Str <$> chunks
        if quoteLen == 2 then pure (Ast.Str mempty) else (body <* end) <|> (Ast.Str mempty <$ noEnd)


--------------------
-- === Layout === --
--------------------

strBlock1 :: forall t. Lexer (Spanned (Atom.StrChunk t)) -> Lexer [Spanned (Atom.StrChunk t)]
strBlock1 = \p -> let
    line :: Lexer (Spanned Delta, [Spanned (Atom.StrChunk t)])
    line = do
        off <- spanned $ newline *> Indent.indented
        (off,) <$> many p

    lines = do
        ls <- many1' line
        let sinds  = fst <$> ls
            conts  = snd <$> ls
            spans  = view Ast.span    <$> sinds
            inds   = Ast.unsafeUnspan <$> sinds
            (is' : inds')  = subtract minInd  <$> inds
            minInd = unsafeMinimum inds
            astLs  = (Atom.StrPlain . convert .            flip replicate ' ' $   is')
                   : (Atom.StrPlain . convert . ('\n' :) . flip replicate ' ' <$> inds')
            astLs' = zipWith Ast.Spanned spans astLs
            conts' = zipWith (:) astLs' conts
        pure $ concat conts'
    in lines
{-# INLINE strBlock1 #-}

strBlock :: Lexer (Spanned (Atom.StrChunk t)) -> Lexer [Spanned (Atom.StrChunk t)]
strBlock = option mempty . strBlock1
{-# INLINE strBlock #-}


mergeStrChunks :: [Spanned (Atom.StrChunk t)] -> [Spanned (Atom.StrChunk t)]
mergeStrChunks = \case
    []     -> []
    (a:as) -> let
        span       = view Ast.span a
        spans      = view Ast.span <$> as
        getTxt     = \(Atom.StrPlain t) -> t
        getSpanTxt = getTxt . Ast.unsafeUnspan
        txt        = getSpanTxt  $  a
        txts       = getSpanTxt <$> as
        in [Ast.Spanned (foldl (<>) span spans) (Atom.StrPlain $ foldl (<>) txt txts)]



flexBlock1 :: Lexer a -> Lexer [a]
flexBlock1 = \p -> let
    line = do
        many1' newline
        Indent.indented
        pure <$> p
    in concat <$> many1' line
{-# INLINE flexBlock1 #-}

flexBlock :: Lexer a -> Lexer [a]
flexBlock = option mempty . flexBlock1
{-# INLINE flexBlock #-}
