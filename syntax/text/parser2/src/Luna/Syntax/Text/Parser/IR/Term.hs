{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE UndecidableInstances      #-}

module Luna.Syntax.Text.Parser.IR.Term where

import qualified Prelude  as P
import           Prologue hiding (Text, imp, seq, some, takeWhile)
import qualified Prologue

import qualified Control.Monad.State.Layered            as State
import qualified Data.Attoparsec.Text32                 as Parsec
import qualified Data.Char                              as Char
import qualified Data.Graph.Data.Layer.Layout           as Layout
import qualified Data.Text.Position                     as Position
import qualified Data.Text.Span                         as Span
import qualified Data.Text32                            as Text
import qualified Data.Vector                            as Vector
import qualified Luna.IR                                as IR
import qualified Luna.IR.Layer                          as Layer
import qualified Luna.IR.Term.Ast.Invalid               as Invalid
import qualified Luna.Syntax.Text.Lexer                 as Lexer
import qualified Luna.Syntax.Text.Lexer.Symbol          as Lexer
import qualified Luna.Syntax.Text.Parser.Data.Ast.Class as Atom
import qualified Luna.Syntax.Text.Parser.Data.CodeSpan  as CodeSpan
import qualified Luna.Syntax.Text.Parser.IR.Ast         as Ast
import qualified Luna.Syntax.Text.Parser.State.Marker   as Marker
import qualified Luna.Syntax.Text.Scope                 as Scope
import qualified Text.Parser.State.Indent               as Indent


import Control.Monad.State.Layered           (StatesT)
import Data.Parser                           hiding (Result, Token, endOfInput)
import Data.Text.Position                    (FileOffset (..))
import Data.Text.Position                    (Delta)
import Data.Vector                           (Vector)
import Luna.IR                               (SomeTerm, Term)
import Luna.Syntax.Text.Parser.Data.CodeSpan (CodeSpan (CodeSpan),
                                              CodeSpanRange (..))
import Luna.Syntax.Text.Parser.IR.Ast        (isEolBeginChar)
import Luna.Syntax.Text.Parser.IR.Ast        (Parser)
-- import Luna.Syntax.Text.Parser.Pass.Class       (IRB (IRB, fromIRB),
--                                                  IRBS (IRBS), fromIRBS, irb0,
--                                                  irb1, irb2, irb3, irb4, irb5,
--                                                  liftIRBS1, liftIRBS2,
--                                                  liftIRBS3, withIRB)
import Luna.Syntax.Text.Parser.State.LastOffset (LastOffset (LastOffset))
import OCI.Data.Name                            (Name)
import Text.Parser.Combinators                  (some)
import Text.Parser.State.Indent                 (Indent)

import Data.Parser.Instances.Attoparsec ()





type Text = Text.Text32

type Ast = Ast.Spanned Ast.Ast
type UnspannedAst = Ast.Ast



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





getLastOffset   :: State.Getter LastOffset m => m Delta
getLastOffset   = unwrap <$> State.get @LastOffset




-------------------------------
-- === Syntax management === --
-------------------------------

syntax1Only :: Parser a -> Parser a
syntax1Only = \p -> State.get @Ast.SyntaxVersion >>= \case
    Ast.Syntax1 -> p
    Ast.Syntax2 -> fail "syntax 1 only"
{-# INLINE syntax1Only #-}



---------------------
-- === Helpers === --
---------------------

symbol :: Text -> Parser Text
symbol = Ast.lexeme . tokens
{-# INLINE symbol #-}

invalid :: Parser Invalid.Symbol -> Parser Ast
invalid = Ast.computeSpan . invalid'
{-# INLINE invalid #-}

invalid' :: Parser Invalid.Symbol -> Parser UnspannedAst
invalid' = fmap Ast.Invalid
{-# INLINE invalid' #-}

unknown :: Parser Ast
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

anyLine :: Parser Text
anyLine = takeWhile $ not . isEolBeginChar
{-# INLINE anyLine #-}

unknownChunk :: Parser Text
unknownChunk = Text.cons <$> anyToken <*> takeWhile (not . isSeparatorChar)
{-# INLINE unknownChunk #-}

chunk :: Parser Text
chunk = takeWhile1 (not . isSeparatorChar)
{-# INLINE chunk #-}

-- chunkOfNot :: [Char] -> Parser Int
-- chunkOfNot s = Text.length <$> takeWhile1 (not . (`elem` break)) where
--     break = ' ' : (s <> Ast.eolStartChars)
-- {-# INLINE chunkOfNot #-}



-------------------------
-- === Identifiers === --
-------------------------

-- === Getting values === --

var, cons :: Parser ()
var  = Ast.register =<< Ast.computeSpan var'
cons = Ast.register =<< Ast.computeSpan cons'
{-# INLINE var  #-}
{-# INLINE cons #-}

wildcard :: Parser ()
wildcard = Ast.register =<< Ast.computeSpan wildcard'
{-# INLINE wildcard #-}

var', cons' :: Parser UnspannedAst
var'  = (Ast.Var  <$> varName)  <**> option id (const <$> invalidIdentSuffix)
cons' = (Ast.Cons <$> consName) <**> option id (const <$> invalidIdentSuffix)
{-# INLINE var'  #-}
{-# INLINE cons' #-}

wildcard' :: Parser UnspannedAst
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

expectVar :: Name -> Parser ()
expectVar = \name -> let err = fail "no match" in var' >>= \case
    Ast.Var name' -> when_ (name /= name') err
    _             -> err
{-# INLINE expectVar #-}

expectCons :: Name -> Parser ()
expectCons = \name -> let err = fail "no match" in cons' >>= \case
    Ast.Cons name' -> when_ (name /= name') err
    _              -> err
{-# INLINE expectCons #-}

expectOperator :: Name -> Parser ()
expectOperator = \name -> let err = fail "no match" in operator' >>= \case
    Ast.Operator name' -> when_ (name /= name') err
    _                  -> err
{-# INLINE expectOperator #-}


-- === Names === --

varName :: Parser Name
varName = convert . (<>) <$> header <*> identBody where
    header = Text.snoc <$> pfx <*> head
    pfx    = takeMany '_'
    head   = satisfy Char.isLower
{-# INLINE varName #-}

consName :: Parser Name
consName = convert . Text.cons <$> header <*> identBody where
    header = satisfy Char.isUpper
{-# INLINE consName #-}

identBody :: Parser Text
identBody = body <**> sfx where
    body  = takeWhile isIdentBodyChar
    sfx   = option id (flip (<>) <$> takeMany1 '\'')
{-# INLINE identBody #-}


-- === Validation === --

-- | We are not using full (33-126 / ['"`]) range here, because we need to check
--   such invalid suffixes as `foo'a`.
invalidIdentSuffix :: Parser UnspannedAst
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

operator :: Parser ()
operator = Ast.register =<< Ast.computeSpan operator'
{-# NOINLINE operator #-}

operator' :: Parser UnspannedAst
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

unsafeAnyTokenOperator :: Parser ()
unsafeAnyTokenOperator = Ast.register
    =<< Ast.computeSpan (Ast.Operator . convert <$> anyToken)
{-# INLINE unsafeAnyTokenOperator #-}

eqChar :: Char
eqChar = '='
{-# INLINE eqChar #-}

isOpenCloseChar :: Char -> Bool
isOpenCloseChar = (`elem` ("(){}[]" :: [Char]))


-- === Helpers === --

isOperatorBodyChar :: Char -> Bool
isOperatorBodyChar = \c -> c /= eqChar
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

invalidOperatorSuffix :: Parser UnspannedAst
invalidOperatorSuffix = let
    inv = Ast.Invalid . Invalid.UnexpectedSuffix <$> sfx
    sfx = Text.length <$> takeWhile1 isOperatorBeginChar
    in inv
{-# INLINE invalidOperatorSuffix #-}



----------------------
-- === Comments === --
----------------------

-- === API === --

comment :: Parser ()
comment = Ast.register =<< Ast.computeSpan (commentChar >> parser) where
    commentChar = token commentStartChar
    body        = trimIndent <$> (multiLine <|> singleLine)
    multiLine   = commentChar *> flexBlock rawLine
    singleLine  = pure <$> rawLine
    rawLine     = takeWhile (not . isEolBeginChar)
    parser      = (Ast.Comment <$> body)
{-# INLINE comment #-}

-- comment :: Parser ()
-- comment = parser where
--     commentChar    = token commentStartChar

--     multilineChars = (\c1 c2 -> [c1,c2]) <$> commentChar <*> commentChar
--     multiLineOp    = Ast.computeSpan (Ast.Operator . convert <$> multilineChars)
--     multiLineStart = Ast.register =<< multiLineOp

--     singleLineChars = (:[]) <$> commentChar
--     singleLineOp    = Ast.computeSpan (Ast.Operator . convert <$> singleLineChars)
--     singleLineStart = Ast.register =<< singleLineOp

--     rawLine     = takeWhile (not . isEolBeginChar)
--     multiLine   = multiLineStart *> flexBlock rawLine
--     singleLine  = singleLineStart *> (pure <$> rawLine)
--     body        = trimIndent <$> (multiLine <|> singleLine)
--     parser      = Ast.register =<< Ast.computeSpan (Ast.Comment <$> body)
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

marker :: Parser ()
marker = Ast.register . (Ast.span %~ CodeSpan.asPhantom) =<< Ast.computeSpan parser where
    correct   = Ast.Marker <$> decimal
    incorrect = Ast.Invalid Invalid.InvalidMarker <$ takeTill (== markerEnd)
    parser    = token markerBegin *> (correct <|> incorrect) <* token markerEnd
{-# INLINE marker #-}



-------------------------
-- === Expressions === --
-------------------------

-- === API === --

expr :: Parser ()
expr = fastExprByChar =<< peekToken
{-# INLINE expr #-}

exprs :: Parser ()
exprs = let
    parser = do
        Ast.whiteSpace
        many expr
        token '\ETX'
        pure ()
        -- Ast.tokens' . unwrap <$> State.get @Ast.Result
    -- in State.put @Ast.Result . wrap . pure =<< Ast.computeSpan parser
    in parser
{-# NOINLINE exprs #-}


unknownExpr :: Parser ()
unknownExpr = Ast.register =<< unknown
{-# INLINE unknownExpr #-}

lineBreak :: Parser ()
lineBreak = Ast.register =<< ast where
    ast = Ast.computeSpan $ do
        some Ast.newline
        Ast.LineBreak <$> Position.getColumn
{-# INLINE lineBreak #-}


-- === Fast ASCII Lookup Table === --

-- | The size of `symmap` - Vector-based map from head Char to related parser.
lookupTableSize :: Int
lookupTableSize = 200
{-# INLINE lookupTableSize #-}

exprLookupTable :: Vector (Parser ())
exprLookupTable = Vector.generate lookupTableSize $ exprByChar . Char.chr

exprByChar :: Char -> Parser ()
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
fastExprByChar :: Char -> Parser ()
fastExprByChar = \c -> let ord = Char.ord c in if
    | ord < lookupTableSize -> Vector.unsafeIndex exprLookupTable ord -- (1)
    | otherwise             -> exprByChar c                           -- (2)
{-# INLINE fastExprByChar #-}



number :: Parser ()
number = Ast.register =<< Ast.computeSpan parser where
    parser  = correct <**> option id (const <$> invalidIdentSuffix)
    correct = Ast.Number <$> digits
{-# INLINE number #-}

isDigitChar :: Char -> Bool
isDigitChar = \c -> c >= '0' && c <= '9'
{-# INLINE isDigitChar #-}

digitChars :: Parser (NonEmpty Char)
digitChars = unsafeConv <$> takeWhile1 isDigitChar where
    unsafeConv :: Text -> NonEmpty Char
    unsafeConv = \txt -> case convertTo @[Char] txt of
        []     -> impossible
        (a:as) -> a :| as
{-# INLINE digitChars #-}

intDigits :: Parser (NonEmpty Int)
intDigits = toDigit <<$>> digitChars where
    toDigit = subtract 48 . Char.ord
{-# INLINE intDigits #-}

digits :: Parser (NonEmpty Word8)
digits = fromIntegral <<$>> intDigits
{-# INLINE digits #-}

decimal :: Parser Int
decimal = unsafeRead . convert <$> digitChars
{-# INLINE decimal #-}


isRawStrQuote :: Char -> Bool
isRawStrQuote = (== '"')
{-# INLINE isRawStrQuote #-}

isFmtStrQuote :: Char -> Bool
isFmtStrQuote = (== '\'')
{-# INLINE isFmtStrQuote #-}


failIf :: Bool -> Parser ()
failIf = \b -> if b then fail "Failed check" else pure ()
{-# INLINE failIf #-}

strBuilder :: Char -> Parser ()
strBuilder quote = Ast.register =<< Ast.computeSpan parser where
    parser = do
        let isQuote      = (== quote)
            isBodyChar c = c == quote || isEolBeginChar c

        quoteLen <- Text.length <$> takeWhile1 isQuote

        let mkChunk       = \p -> Ast.register =<< Ast.computeSpan p
            chunkPlain    = Atom.StrPlain <$> takeWhile1 (not . isBodyChar)
            chunkQuote    = bodyQuotes =<< takeWhile1 isQuote
            bodyQuotes qs = Atom.StrPlain qs <$ failIf (Text.length qs == quoteLen)
            chunk         = Ast.computeSpan $ choice [chunkPlain, chunkQuote] -- lineBreak
            ending        = Text.length <$> takeWhile1 isQuote
            body          = Ast.Str <$> many chunk
        body <* ending




--------------------
-- === Layout === --
--------------------

flexBlock1 :: Parser a -> Parser (NonEmpty a)
flexBlock1 = \p -> let
    line = some Ast.newline >> Indent.indented >> p
    in some line
{-# INLINE flexBlock1 #-}

flexBlock :: Parser a -> Parser [a]
flexBlock = \p -> option mempty $ convert <$> flexBlock1 p
{-# INLINE flexBlock #-}

-- discoverBlock1 :: Parser a -> Parser (NonEmpty a)
-- discoverBlock1 = \p -> let
--     line   = some Ast.newline >> Indent.indentedEq >> p
--     block  = (:|) <$> p <*> many line
--     header = some Ast.newline >> Indent.indented
--     in header >> Indent.withCurrent block

-- multiLineBlock1 :: Parser a -> Parser (NonEmpty a)
-- multiLineBlock1 = \p -> let
--     line = some Ast.newline >> Indent.indented >> p
--     in (:|) <$> p <*> many line
-- {-# INLINE multiLineBlock1 #-}







-------------------------
-- === Expressions === --
-------------------------

-- === Parsers === --


-- varExpr :: Parser ()
-- varExpr = Ast.register =<< var
-- {-# INLINE varExpr #-}

-- consExpr :: Parser ()
-- consExpr = Ast.register =<< cons
-- {-# INLINE consExpr #-}

-- wildcardExpr :: Parser ()
-- wildcardExpr = Ast.register =<< wildcard
-- {-# INLINE wildcardExpr #-}

-- operatorExpr :: Parser ()
-- operatorExpr = Ast.register =<< operator
-- {-# INLINE operatorExpr #-}

-- lamExpr :: Parser ()
-- lamExpr = arrow >> body where
--     arrow = Ast.register =<< Ast.computeSpan (Ast.Operator . convert <$> tokP)
--     body  = void $ discoverBlock1 expr
--     tokP  = State.get @Ast.SyntaxVersion >>= \case
--         Ast.Syntax1 -> tokens ":"
--         Ast.Syntax2 -> tokens "->"
-- {-# INLINE lamExpr #-}



-- groupExpr :: Parser ()
-- groupExpr = Ast.computeSpan $ Ast.grouped' <$> parensed expr where
-- {-# INLINE group2 #-}



-- === Elements === --

-- old_funcDef :: Parser Ast
-- old_funcDef = syntax1Only parser where

--     -- Utils
--     blockChar  = ':'
--     header     = symbol "def"

--     -- Name
--     name       = var <|> opName <|> invName
--     opName     = operatorBy (\c -> isOperatorBodyChar c && c /= blockChar)
--     invName    = invalid $ Invalid.InvalidFunctionName <$ chunk

--     -- Args
--     invArg     = invalid $ Invalid.InvalidFunctionArg <$ chunkOfNot [blockChar]
--     args       = many (old_nonSpacedExpr <|> invArg)

--     blockStart = Ast.lexeme $ token blockChar

--     -- Body
--     body       = multiLine <|> lineExpr <|> pure Ast.missing
--     multiLine  = (Ast.block <$> discoverBlock1 lineExpr)
--     singleLine = lineExpr
--     impl       = (blockStart *> body) <|> invBody
--     invBody    = Ast.computeSpan $ Ast.Invalid Invalid.MissingColonBlock
--               <$ anyLine <* flexBlock anyLine

--     -- Def
--     invDef     = invalid' $ Invalid.InvalidFunctionDefinition
--               <$ anyLine <* flexBlock anyLine
--     valDef     = Ast.function' <$> name <*> args <*> impl
--     parser     = Ast.computeSpan $ header >> (valDef <|> invDef)


-- a co jesli [a,b,c] potraktujemy jako mixfix?

-- list :: Parser Ast
-- list = Ast.computeSpan $ braced $ Ast.list' <$> elems where
--     missing :: Parser (IRBS SomeTerm)
--     missing   = irbs . pure $ irb0 IR.missing'
--     elem      = Reserved.withLocal sep p
--     optElem   = elem <|> missing
--     bodyH     = (:) <$> elem    <*> many       (separator *> optElem)
--     bodyT     = (:) <$> missing <*> someAsList (separator *> optElem)
--     elems     = option mempty $ bodyH <|> bodyT
--     separator = symbol ","


-- old_nonSpacedExpr :: Parser Ast
-- old_nonSpacedExpr = choice
--     [ var
--     , cons
--     , group
--     ]
-- {-# INLINE old_nonSpacedExpr #-}


-- group :: Parser Ast
-- group = Ast.computeSpan $ Ast.grouped' <$> parensed expr where
-- {-# INLINE group #-}

-- parensed :: Parser a -> Parser a
-- parensed = \p -> symbol "(" *> Ast.withBlacklistedUnknown ')' p <* symbol ")"
-- {-# INLINE parensed #-}

-- braced :: Parser a -> Parser a
-- braced = \p -> symbol "[" *> Ast.withBlacklistedUnknown ']' p <* symbol "]"
-- {-# INLINE braced #-}













-- foo = a -> b ->
--     c -> d ->
--     e -> f

-- foo = a -> b
--     -> c -> d
--     -> e -> f


-- bar f = f 1

-- bar (foo ...)


-- foo bar baz gaz faz
--   $ daz naz uaz
--   jaz kaz
--   raz laz

-- s = sphere
--     radius   = 10
--     color    = red
--     position = Point 0 0 0



-- x = foo.sum 1
-- skipEOLs :: Parser ()
-- skipEOLs = void $ many eol

-- possibleNonEmptyBlock :: Parser a -> Parser (NonEmpty a)
-- possibleNonEmptyBlock = \p -> (es >> nonEmptyBlock2 p) <|> (pure <$> p) where
--     es = try (eol >> skipEOLs >> Indent.indented)
-- {-# INLINE possibleNonEmptyBlock #-}

-- possibleNonEmptyBlock' :: Parser a -> Parser [a]
-- possibleNonEmptyBlock' = fmap convert . possibleNonEmptyBlock
-- {-# INLINE possibleNonEmptyBlock' #-}

-- discoverIndent :: Parser a -> Parser a
-- discoverIndent = discover . Indent.withCurrent where
--     discover p = skipEOLs >> p

-- nonEmptyBlock', nonEmptyBlockBody' :: Parser a -> Parser [a]
-- nonEmptyBlock'     = uncurry (:) <<$>> nonEmptyBlock
-- nonEmptyBlockBody' = uncurry (:) <<$>> nonEmptyBlockBody

-- nonEmptyBlock2 ::  Parser a -> Parser (NonEmpty a)
-- nonEmptyBlock2 p = Indent.withCurrent (nonEmptyBlockBody2 p)

-- nonEmptyBlockBody2 ::  Parser a -> Parser (NonEmpty a)
-- nonEmptyBlockBody2 p = (:|) <$> p <*> lines where
--     spacing = many eol
--     indent  = spacing <* Indent.indentedEq <* notFollowedBy etx
--     lines   = many $ try indent *> p

-- nonEmptyBlock , nonEmptyBlockBody  :: Parser a -> Parser (a, [a])
-- nonEmptyBlock     p = Indent.indented >> Indent.withCurrent (nonEmptyBlockBody p)
-- nonEmptyBlockTop    = Indent.withRoot    . nonEmptyBlockBody
-- nonEmptyBlockBody p = (,) <$> p <*> lines where
--     spacing = many eol
--     indent  = spacing <* Indent.indentedEq <* notFollowedBy etx
--     lines   = many $ try indent *> p

-- optionalBlock, optionalBlockTop, optionalBlockBody :: Parser a -> Parser [a]
-- optionalBlock     p = option mempty $ uncurry (:) <$> nonEmptyBlock     p
-- optionalBlockTop  p = option mempty $ uncurry (:) <$> nonEmptyBlockTop  p
-- optionalBlockBody p = option mempty $ uncurry (:) <$> nonEmptyBlockBody p

-- breakableNonEmptyBlock', breakableNonEmptyBlockTop', breakableNonEmptyBlockBody' :: Parser a -> Parser     [a]
-- breakableNonEmptyBlock , breakableNonEmptyBlockTop , breakableNonEmptyBlockBody  :: Parser a -> Parser (a, [a])
-- breakableNonEmptyBlock'      = uncurry (:) <<$>> breakableNonEmptyBlock
-- breakableNonEmptyBlockTop'   = uncurry (:) <<$>> breakableNonEmptyBlockTop
-- breakableNonEmptyBlockBody'  = uncurry (:) <<$>> breakableNonEmptyBlockBody
-- breakableNonEmptyBlock       = Indent.withCurrent . breakableNonEmptyBlockBody
-- breakableNonEmptyBlockTop    = Indent.withRoot    . breakableNonEmptyBlockBody
-- breakableNonEmptyBlockBody p = (,) <$> lexedp <*> lines where
--     spacing = many eol
--     lines   = many $ Indent.indentedEq *> lexedp
--     lexedp  = p <* spacing








-- checkInvalidSuffix :: Parser Symbol -> Parser Symbol
-- checkInvalidSuffix =  (<**> option id (const <$> invalidSuffix))
-- {-# INLINE checkInvalidSuffix #-}





-- consName, varName, opName, identName, modifierName :: Parser Name
-- foreignLangName                                    :: Parser Name
-- consName        = convert <$> satisfTest Lexer.matchCons     ; {-# INLINE consName        #-}
-- varName         = convert <$> satisfTest Lexer.matchVar      ; {-# INLINE varName         #-}
-- opName          = convert <$> satisfTest Lexer.matchOperator ; {-# INLINE opName          #-}
-- identName       = varName <|> consName <|> opName            ; {-# INLINE identName       #-}
-- funcName        = varName <|> opName                         ; {-# INLINE funcName        #-}
-- modifierName    = convert <$> satisfTest Lexer.matchModifier ; {-# INLINE modifierName    #-}
-- foreignLangName = consName                                   ; {-# INLINE foreignLangName #-}

-- previewVarName :: Parser Name
-- previewVarName = do
--     s <- previewNextSymbol
--     maybe (unexpected . fromString $ "Expecting variable, got: " <> show s)
--           (pure . convert) $ Lexer.matchVar s
-- {-# INLINE previewVarName #-}

-- matchVar, matchCons, matchOp :: Text32 -> Parser ()
-- matchVar  = symbol . Lexer.Var      ; {-# INLINE matchVar  #-}
-- matchCons = symbol . Lexer.Cons     ; {-# INLINE matchCons #-}
-- matchOp   = symbol . Lexer.Operator ; {-# INLINE matchOp   #-}


-- === Helpers === --




-- -- Qualified names

-- qualVarName, qualConsName :: Parser [Name]
-- qualConsName = qualNameBase consName ; {-# INLINE qualConsName #-}
-- qualVarName  = qualNameBase varName  ; {-# INLINE qualVarName  #-}

-- qualNameBase :: Parser Name -> Parser [Name]
-- qualNameBase p = (\a b -> a <> [b])
--              <$> many (try $ consName <* symbol Lexer.Accessor) <*> p
-- {-# INLINE qualNameBase #-}



-- -- TODO: Review Literals
-- ----------------------
-- -- === Literals === --
-- ----------------------

-- rawQuoteBegin, rawQuoteEnd :: Parser ()
-- fmtQuoteBegin, fmtQuoteEnd :: Parser ()
-- rawQuoteBegin = symbol $ Lexer.Quote Lexer.RawStr Lexer.Begin ; {-# INLINE rawQuoteBegin #-}
-- rawQuoteEnd   = symbol $ Lexer.Quote Lexer.RawStr Lexer.End   ; {-# INLINE rawQuoteEnd   #-}
-- fmtQuoteBegin = symbol $ Lexer.Quote Lexer.FmtStr Lexer.Begin ; {-# INLINE fmtQuoteBegin #-}
-- fmtQuoteEnd   = symbol $ Lexer.Quote Lexer.FmtStr Lexer.End   ; {-# INLINE fmtQuoteEnd   #-}

-- number :: Parser (IRBS SomeTerm)
-- number = irbs $ do
--     Lexer.NumRep base i f <- satisfTest Lexer.matchNumber
--     i' <- Mutable.fromList i
--     f' <- Mutable.fromList f
--     pure $ irb3 IR.number' base i' f'


-- list :: Parser (IRBS SomeTerm) -> Parser (IRBS SomeTerm)
-- list p = irbs $ Reserved.withoutLocal sep $ braced $ (\g -> liftIRBS1 (irb1 IR.list') $ sequence g) <$> elems where
--     missing :: Parser (IRBS SomeTerm)
--     missing   = irbs . pure $ irb0 IR.missing'
--     sep       = Lexer.Operator ","
--     elem      = Reserved.withLocal sep p
--     optElem   = elem <|> missing
--     bodyH     = (:) <$> elem    <*> many       (separator *> optElem)
--     bodyT     = (:) <$> missing <*> someAsList (separator *> optElem)
--     elems     = option mempty $ bodyH <|> bodyT
--     braced p  = braceBegin *> p <* braceEnd
--     separator = symbol sep

-- -- FIXME[WD]: This `try` should be refactored out
-- -- FIXME[WD]: Tuple and List parsers are too similar no to be refactored to common part. However tuples will disappear soon.
-- tuple :: Parser (IRBS SomeTerm) -> Parser (IRBS SomeTerm)
-- tuple p = try $ irbs $ Reserved.withoutLocal sep $ parensed
--         $ (liftIRBS1 (irb1 IR.tuple') . sequence) <$> elems where
--     missing :: Parser (IRBS SomeTerm)
--     missing    = irbs $ pure (irb0 IR.missing')
--     sep        = Lexer.Operator ","
--     elem       = Reserved.withLocal sep p
--     optElem    = elem <|> missing
--     body       = (:) <$> optElem <*> someAsList (separator *> optElem)
--     elems      = option mempty body
--     parensed p = groupBegin *> p <* groupEnd
--     separator  = symbol sep

-- string :: Parser (IRBS SomeTerm)
-- string = rawStr <|> fmtStr

-- rawStr :: Parser (IRBS SomeTerm)
-- rawStr = irbs $ do
--     rawQuoteBegin
--     withRecovery (\e -> invalid Invalid.FunctionHeader <$ Loc.dropSymbolsUntilAndGatherErrors (== (Lexer.Quote Lexer.RawStr Lexer.End)) []) p
--     where body = (strBody <|> pure mempty) <* rawQuoteEnd
--           p    = do
--               b <- Indent.withCurrent body
--               let bodyStr = convertTo @String b -- FIXME[WD]: We're converting Text -> String here.
--               irb1 IR.rawString' <$> Mutable.fromList bodyStr
--     -- withRecovery (\e -> invalid "Invalid string literal" <$ Loc.unregisteredDropSymbolsUntil' (== (Lexer.Quote Lexer.RawStr Lexer.End)))
--     --              $ (IR.rawString' . convertVia @String)
--     --            <$> Indent.withCurrent (strBody rawQuoteEnd) -- FIXME[WD]: We're converting Text -> String here.
--     -- dropSymbolsUntilAndGatherErrors
--     -- :: (MonadParsec e Stream m, MonadLoc m) => (Lexer.Symbol -> Bool) -> [Token] -> m [Token]

-- -- fmtStr :: Parser (IRBS SomeTerm)
-- -- fmtStr = irbs $ do
-- --     fmtQuoteBegin
-- --     (IR.rawString' . convertVia @String) <$> Indent.withCurrent (strBody fmtQuoteEnd) -- FIXME[WD]: We're converting Text -> String here.
-- --     -- withRecovery (\e -> invalid "Invalid string literal" <$ Loc.unregisteredDropSymbolsUntil' (== (Lexer.Quote Lexer.FmtStr Lexer.End)))
-- --     --              $ (IR.rawString' . convertVia @String)
-- --     --            <$> Indent.withCurrent (strBody fmtQuoteEnd) -- FIXME[WD]: We're converting Text -> String here.

-- strBody :: Parser Text32
-- strBody = segStr <|> nl where
--     segStr = (<>) <$> strContent <*> (strBody <|> pure mempty)
--     nl     = Text32.cons '\n' <$ eol <*> (line <|> nl <|> pure mempty)
--     line   = do Indent.indentedOrEq
--                 (<>) . convert . flip replicate ' ' <$> Indent.indentation
--                                                     <*> strBody

-- strContent :: Parser Text32
-- strContent = satisfTest Lexer.matchStr <|> strEsc


-- -- match = irbs $ (\n (p,ps) -> liftIRBS2 IR.match' n (sequence $ p:ps))


-- fmtStr :: Parser (IRBS SomeTerm)
-- fmtStr = irbs $ fmtQuoteBegin *> (withRecovery handler str) where
--     str     = (liftIRBS1 (irb1 IR.fmtString') . sequence) <$> body
--     chunk   = (:) <$> element <*> body
--     body    = chunk  <|> end
--     element = rawStr <|> code
--     end     = mempty <$ fmtQuoteEnd
--     rawStr  = irbs $ (irb1 IR.rawString') <$> (Mutable.fromList =<< (convertTo @String <$> strBody))
--     code    = symbol (Lexer.Block Lexer.Begin) *> lineExpr <* symbol (Lexer.Block Lexer.End)
--     handler e = do
--         out <- Loc.dropSymbolsUntilAndGatherErrors (== (Lexer.Quote Lexer.FmtStr Lexer.End)) []
--         -- FIXME: We might want to do it safe way and report all errors in
--         --        string literal. However, we need to think how to do it right.
--         --        It seems this is the only place where multiple errors can
--         --        occur within a single IR.
--         let err = case out of
--                 Left  {} -> error "TODO: no closing mark" -- FIXME: finish me
--                 Right s  -> fromJust (error "PANIC: no error discovered in wrong string literal") (head s)
--         pure $ invalid err

-- strEsc :: Parser Text32
-- strEsc = satisfTest Lexer.matchStrEsc >>= pure . \case
--     Lexer.CharStrEsc i             -> convert $ Char.chr i
--     Lexer.NumStrEsc i              -> convert $ Char.chr i
--     Lexer.SlashEsc                 -> "\\"
--     Lexer.QuoteEscape Lexer.RawStr -> "\""
--     Lexer.QuoteEscape Lexer.FmtStr -> "'"



-- -------------------------
-- -- === Expressions === --
-- -------------------------

-- -- === Utils === --

-- app, seq, unify, appFlipped, sectionLeft, sectionRight
--     :: IRBS SomeTerm -> IRBS SomeTerm -> IRBS SomeTerm
-- app          = inheritCodeSpan2 (irb2 IR.app')                        ; {-# INLINE app          #-}
-- seq          = inheritCodeSpan2 (irb2 IR.seq')                        ; {-# INLINE seq          #-}
-- unify        = inheritCodeSpan2 (irb2 IR.unify')                      ; {-# INLINE unify        #-}
-- sectionLeft  = inheritCodeSpan2 (irb2 IR.sectionLeft')                ; {-# INLINE sectionLeft  #-}
-- appFlipped   = flip . inheritCodeSpan2 $ flip (irb2 IR.app')          ; {-# INLINE appFlipped   #-}
-- sectionRight = flip . inheritCodeSpan2 $ flip (irb2 IR.sectionRight') ; {-# INLINE sectionRight #-}

-- appSides :: IRBS SomeTerm -> IRBS SomeTerm -> IRBS SomeTerm -> IRBS SomeTerm
-- appSides = app .: appFlipped ; {-# INLINE appSides #-}

-- apps, seqs :: IRBS SomeTerm -> [IRBS SomeTerm] -> IRBS SomeTerm
-- apps = foldl' app ; {-# INLINE apps #-}
-- seqs = foldl' seq ; {-# INLINE seqs #-}

-- seqs2 :: NonEmpty (IRBS SomeTerm) -> IRBS SomeTerm
-- seqs2 = \(a :| as) -> foldl' seq a as ; {-# INLINE seqs2 #-}

-- grouped :: Parser (IRBS SomeTerm) -> Parser (IRBS SomeTerm)
-- grouped = irbs . parensed . fmap (liftIRBS1 (irb1 IR.grouped')) ; {-# INLINE grouped #-}


-- -- === Metadata === --

-- -- FIXME: performance
-- metadata :: Parser (IRBS SomeTerm)
-- metadata = irbs $ irb1 IR.metadata' <$> (Mutable.fromList =<< (convertTo @String <$> metaContent)) ; {-# INLINE metadata #-}

-- metaContent :: Parser Text32
-- metaContent = satisfTest Lexer.matchMetadata ; {-# INLINE metaContent #-}


-- -- === Disabled === --

-- possiblyDisabled :: Parser (IRBS SomeTerm) -> Parser (IRBS SomeTerm)
-- possiblyDisabled p = disabled p <|> p ; {-# INLINE possiblyDisabled #-}

-- disabled :: Parser (IRBS SomeTerm) -> Parser (IRBS SomeTerm)
-- disabled p = irbs $ liftIRBS1 (irb1 IR.disabled') <$ symbol Lexer.Disable <*> p ; {-# INLINE disabled #-}


-- -- === Segments === --

-- type IsFirst = Bool
-- type IsLast  = Bool

-- type    ExprSegment        = ExprToken (IRBS SomeTerm)
-- type    ExprSegments       = NonEmpty ExprSegment
-- type    ExprSegmentBuilder = SegmentBuilder ExprSegment
-- newtype SegmentBuilder a   = SegmentBuilder
--     { runSegmentBuilder :: IsFirst -> IsLast -> NonEmpty a
--     } deriving (Functor)

-- type ExprToken       a = Expr.Token (ExprSymbol      a)
-- type ExprTokenProto  a = Expr.Token (ExprSymbolProto a)
-- type ExprSymbol      a = Labeled SpacedName (SomeSymbol Symbol.Expr    a)
-- type ExprSymbolProto a = Labeled SpacedName (SomeSymbol Symbol.Phantom a)

-- instance Semigroup ExprSegmentBuilder where
--     a <> b = SegmentBuilder $ \l r -> runSegmentBuilder a l False
--                                    <> runSegmentBuilder b False r
--     {-# INLINE (<>) #-}

-- expr :: Parser (IRBS SomeTerm)
-- expr = possiblyDocumented $ func <|> lineExpr ; {-# INLINE expr #-}

-- lineExpr :: Parser (IRBS SomeTerm)
-- lineExpr = marked <*> possiblyDisabled (valExpr <**> option id assignment) where
--     assignment = flip unify <$ symbol Lexer.Assignment <*> valExpr
-- {-# INLINE lineExpr #-}

-- valExpr :: Parser (IRBS SomeTerm)
-- valExpr = buildTokenExpr =<< exprSegments ; {-# INLINE valExpr #-}

-- nonemptyValExpr :: Parser (IRBS SomeTerm)
-- nonemptyValExpr = do
--     es <- exprSegments
--     -- when (null es) $ unexpected "Empty expression" -- FIXME[WD]
--     buildTokenExpr es

-- nonemptyValExprLocal :: Parser (IRBS SomeTerm)
-- nonemptyValExprLocal = do
--     es <- exprSegmentsLocal
--     -- when (null es) $ unexpected "Empty expression" -- FIXME[WD]
--     buildTokenExpr es

-- nonSpacedValExpr :: Parser (IRBS SomeTerm)
-- nonSpacedValExpr = do
--     es <- exprSegmentsNonSpaced
--     -- when (null es) $ unexpected "Empty expression" -- FIXME[WD]
--     buildTokenExpr es

-- nonSpacedPattern :: Parser (IRBS SomeTerm)
-- nonSpacedPattern = nonSpacedValExpr

-- concatExprSegmentBuilders :: NonEmpty ExprSegmentBuilder -> ExprSegmentBuilder
-- concatExprSegmentBuilders bldrs = SegmentBuilder $ \l r -> case bldrs of
--     (s:|[])     -> runSegmentBuilder s l r
--     (s:|(t:ts)) -> runSegmentBuilder s l False
--                 <> runSegmentBuilder (concatExprSegmentBuilders $ t:|ts) False r

-- exprSegments    , exprSegmentsLocal     :: Parser ExprSegments
-- exprFreeSegments, exprFreeSegmentsLocal :: Parser ExprSegmentBuilder
-- exprSegments          = buildExprTok <$> exprFreeSegments
-- exprSegmentsLocal     = buildExprTok <$> exprFreeSegmentsLocal
-- exprFreeSegments      = Reserved.withNewLocal exprFreeSegmentsLocal
-- exprFreeSegmentsLocal = fmap concatExprSegmentBuilders . some
--                       $ choice [ mfixVarSeg, consSeg, wildSeg, numSeg, strSeg
--                                , opSeg, accSeg, tupleSeg, grpSeg, listSeg
--                                , lamSeg, matchseg, typedSeg, funcSeg, invalidSeg
--                                ]

-- exprSegmentsNonSpaced    , exprSegmentsNonSpacedLocal     :: Parser ExprSegments
-- exprFreeSegmentsNonSpaced, exprFreeSegmentsNonSpacedLocal :: Parser ExprSegmentBuilder
-- exprSegmentsNonSpaced          = buildExprTok <$> exprFreeSegmentsNonSpaced
-- exprSegmentsNonSpacedLocal     = buildExprTok <$> exprFreeSegmentsNonSpacedLocal
-- exprFreeSegmentsNonSpaced      = Reserved.withNewLocal exprFreeSegmentsNonSpacedLocal
-- exprFreeSegmentsNonSpacedLocal = choice [ varSeg, consSeg, wildSeg, numSeg
--                                         , strSeg, tupleSeg, grpSeg, listSeg
--                                         , invalidSeg
--                                         ]


-- -- === Components === --

-- -- Utils
-- posIndependent :: a -> SegmentBuilder (Expr.Token a)
-- posIndependent = SegmentBuilder . P.curry . const . pure . Expr.tokenx ; {-# INLINE posIndependent #-}

-- -- FIXME[WD]: change the API to monadic one, so we can register symbols and mixfix monads could gather needed info (like var names)
-- --            without the need to keep the label in the term
-- unlabeledAtom :: a -> Labeled SpacedName (SomeSymbol Symbol.Expr a)
-- unlabeledAtom = labeled (Name.spaced "#unnamed#") . Symbol.atom

-- -- -- Possible tokens
-- consSeg, funcSeg, grpSeg, invalidSeg, lamSeg, matchseg, numSeg, strSeg,
--     tupleSeg, varSeg, wildSeg :: Parser ExprSegmentBuilder
-- consSeg    = posIndependent . unlabeledAtom <$> cons
-- funcSeg    = posIndependent . unlabeledAtom <$> func
-- grpSeg     = posIndependent . unlabeledAtom <$> grouped nonemptyValExpr
-- invalidSeg = posIndependent . unlabeledAtom <$> invalidToken
-- lamSeg     = posIndependent . labeled (Name.unspaced SpecialName.lam) . Symbol.suffix <$> lamBldr
-- listSeg    = posIndependent . unlabeledAtom <$> list  nonemptyValExprLocal
-- matchseg   = posIndependent . unlabeledAtom <$> match
-- numSeg     = posIndependent . unlabeledAtom <$> number
-- strSeg     = posIndependent . unlabeledAtom <$> string
-- tupleSeg   = posIndependent . unlabeledAtom <$> tuple nonemptyValExprLocal
-- varSeg     = posIndependent . unlabeledAtom <$> var
-- wildSeg    = posIndependent . unlabeledAtom <$> wildcard


-- mfixVarSeg :: Parser ExprSegmentBuilder
-- mfixVarSeg  = do
--     (span, name) <- computeSpan varName
--     nameSet      <- Scope.lookupMultipartNames name
--     let cvar = posIndependent $ unlabeledAtom $ irbsFromSpan span (irb1 IR.var' name)
--     if TreeSet.null nameSet
--         then pure cvar
--         else -- catchInvalidWith (span <>) (pure . posIndependent . unlabeledAtom)
--            {-$-} Reserved.withMany (Lexer.Var . convertVia @String <$> TreeSet.keys nameSet) -- FIXME: conversion Name -> Text
--            $ do segmentToks <- exprFreeSegmentsLocal
--                 namedSegs   <- option mempty (parseMixfixSegments nameSet)
--                 if null namedSegs then pure (cvar <> segmentToks) else do
--                     segment <- buildTokenExpr (buildExprTok segmentToks)
--                     let segments  = snd <$> namedSegs
--                         nameParts = fst <$> namedSegs
--                         mfixVar   = irbsFromSpan span
--                                   . irb1 IR.var' . Name.mixfix $ name :| nameParts
--                         mfixExpr  = apps (app mfixVar segment) segments
--                     pure . posIndependent . unlabeledAtom $ mfixExpr

-- opSeg :: Parser ExprSegmentBuilder
-- opSeg   = do
--     (before, after) <- checkOffsets
--     (span, name)    <- computeSpan opName
--     let segment isFirst isLast = pure . Expr.tokenx $ operator & if
--             | isSingle    -> labeled (Name.unspaced            name) . Symbol.atom
--             | isUMinus    -> labeled (Name.unspaced SpecialName.uminus) . Symbol.prefix . app
--             | isFirst     -> labeled (Name.spacedIf after  name) . Symbol.prefix . sectionLeft
--             | isLast      -> labeled (Name.spacedIf before name) . Symbol.suffix . sectionRight
--             | symmetrical -> labeled (Name.spacedIf after  name) . Symbol.infixx . appSides
--             | before      -> labeled (Name.lspaced         name) . Symbol.prefix . sectionLeft
--             | otherwise   -> labeled (Name.rspaced         name) . Symbol.suffix . sectionRight
--             where isMinus     = name == SpecialName.minus
--                   isSingle    = isFirst && isLast
--                   isUMinus    = isMinus && not after && (isFirst || before) && not isLast
--                   operator    = irbsFromSpan span . (irb1 IR.var') $ if isUMinus then SpecialName.uminus else name
--                   symmetrical = before == after
--     pure $ SegmentBuilder segment

-- typedSeg :: Parser ExprSegmentBuilder
-- typedSeg   = do
--     (off, off') <- checkOffsets
--     let typedExpr = if off then valExpr else nonSpacedValExpr
--     tp <- symbol Lexer.Typed *> typedExpr
--     when_ (off /= off') $ error "TODO: before and after offsets have to match"
--     let seg :: IRBS SomeTerm -> IRBS SomeTerm
--         seg = flip (inheritCodeSpan2 (irb2 IR.typed')) tp
--     pure . posIndependent . labeled (Name.spacedIf off SpecialName.typed) $ Symbol.suffix seg

-- accSectNames :: Parser (NonEmpty (CodeSpan, Name))
-- accSectNames = do
--     sname <- computeSpan $ symbol Lexer.Accessor *> identName
--     (beforeDot, afterDot) <- checkOffsets
--     (sname :|) <$> if beforeDot || afterDot then pure   mempty
--                                             else option mempty (convert <$> accSectNames)


-- accSeg :: Parser ExprSegmentBuilder
-- accSeg = fmap2 Expr.tokenx $ do
--     (beforeDot, afterDot) <- checkOffsets
--     snames@(n :| ns) <- accSectNames
--     mupdt <- option Nothing (Just <$ symbol Lexer.Assignment <*> nonemptyValExpr)
--     mmod  <- option Nothing (Just .: (,) <$> modifierName <*> nonemptyValExpr)
--     let (spans, (names :: [Name])) = unzip $ convert snames
--     -- FIXME: vnamesX are the same thing, but SmallVector 16 and SmallVector 0
--     --        for test purposes needed separate instances. Remove when possible.
--     vnames  <- Mutable.fromList names
--     vnames2 <- Mutable.fromList names
--     vnames3 <- Mutable.fromList names
--     let symmetrical    = beforeDot == afterDot
--         accCons s n    = inheritCodeSpan1With (<> s) (flip (irb2 IR.acc') n)
--         accSect        = labeled uname . Symbol.atom
--                        $ irbsFromSpan (mconcat spans)
--                        $ irb1 IR.accSection' vnames
--         uname          = Name.unspaced SpecialName.acc
--         sname          = Name.spacedIf beforeDot SpecialName.acc
--         accConss       =  labeled sname ( Symbol.suffix $ uncurry accCons n )
--                       :| (labeled uname . Symbol.suffix . uncurry accCons <$> ns)
--         Just fupdt           = mupdt -- FIXME[WD]: make it nicer
--         Just (modName, fmod) = mmod  -- FIXME[WD]: make it nicer

--         -- FIXME[WD]: make it nicer vvv
--         updateAtom = labeled sname . Symbol.suffix
--                    $ flip (inheritCodeSpan2With (<>) (flip (irb3 IR.update') vnames2))
--                           (unsafeModifyCodeSpan (CodeSpan.asOffsetSpan (mconcat spans) <>) fupdt)
--         modifyAtom = labeled sname . Symbol.suffix
--                    $ flip (inheritCodeSpan2With (<>) (flip (flip (irb4 IR.modify') vnames3) modName))
--                           (unsafeModifyCodeSpan (CodeSpan.asOffsetSpan (mconcat spans) <>) fmod)

--     let segment isFirst isLast = if
--             | isFirst     -> pure accSect
--              -- FIXME[WD]: make it nicer vvv
--             | symmetrical -> if
--                 | isJust mupdt -> pure updateAtom
--                 | isJust mmod  -> pure modifyAtom
--                 | otherwise    -> accConss
--             | beforeDot   -> pure accSect
--             | otherwise   -> error "unsupported" -- FIXME[WD]: make nice error here
--     pure $ SegmentBuilder segment

-- match :: Parser (IRBS SomeTerm)
-- match = irbs $ (\n (p :| ps) -> liftIRBS2 (irb2 IR.match') n (sequence $ p:ps))
--       <$ symbol Lexer.KwCase <*> nonemptyValExprLocal
--       <* symbol Lexer.KwOf   <*> possibleNonEmptyBlock clause
--       where clause = pat <**> lamBldr

-- pat :: Parser (IRBS SomeTerm)
-- pat = Reserved.withLocal Lexer.BlockStart nonemptyValExprLocal


-- lamBldr :: Parser (IRBS SomeTerm -> IRBS SomeTerm)
-- lamBldr = do
--     body <- symbol Lexer.BlockStart *> possibleNonEmptyBlock lineExpr
--     pure $ flip (inheritCodeSpan2 (irb2 IR.lam')) (seqs2 body)


-- -- === Utils === --

-- buildExprTok :: ExprSegmentBuilder -> ExprSegments
-- buildExprTok bldr = runSegmentBuilder bldr True True

-- parseMixfixSegments :: SparseTreeSet Name -> Parser [(Name, IRBS SomeTerm)]
-- parseMixfixSegments nameSet = do
--     name <- previewVarName
--     let total = TreeSet.check' name nameSet
--     case nameSet ^? ix name of
--         Nothing       -> unexpected . fromString $ "Identifier `" <> convert name <> "` is not part of a mutli-name expression"
--         Just nameSet' -> do
--             let possiblePaths = TreeSet.keys nameSet'
--             anySymbol_
--             segment <- Reserved.withMany (Lexer.Var . convertVia @String <$> possiblePaths) nonemptyValExpr -- FIXME: conversion Name -> Text
--             let restMod = if total then option mempty else (<|> unexpected (fromString $ "Unexpected end of mixfix expression, expecting one of " <> show possiblePaths))
--             ((name,segment):) <$> restMod (parseMixfixSegments nameSet')

-- buildTokenExpr (s:|ss) = buildTokenExpr' (s:ss)

-- buildTokenExpr' :: Expr.Tokens (Labeled SpacedName (SomeSymbol Symbol.Expr (IRBS SomeTerm))) -> Parser (IRBS SomeTerm)
-- buildTokenExpr' = Expr.buildExpr_termApp . Labeled (Name.spaced SpecialName.app) $ Symbol.Symbol app


-- ---------------------------
-- -- === Documentation === --
-- ---------------------------

-- possiblyDocumented :: Parser (IRBS SomeTerm) -> Parser (IRBS SomeTerm)
-- possiblyDocumented p = documented p <|> p

-- -- FIXME: performance
-- documented :: Parser (IRBS SomeTerm) -> Parser (IRBS SomeTerm)
-- documented p = irbs $ do
--     d <- doc
--     t <- p
--     let x = convertTo @String d
--     ir <- irb2 IR.documented' <$> Mutable.fromList x
--     pure $ liftIRBS1 ir t
--     -- (\d t -> liftIRBS1 (IR.documented' $ convert @String d) t) <$> doc <*> p

-- doc :: Parser Text32
-- doc = intercalate "\n" <$> some (satisfTest Lexer.matchDocComment <* eol)


-- --------------------------
-- -- === Declarations === --
-- --------------------------

-- topLvlDecl :: Parser (IRBS SomeTerm)
-- topLvlDecl = possiblyDocumented $ (marked <*> func) <|> record <|> metadata

-- -- === Functions === --

-- func :: Parser (IRBS SomeTerm)
-- func = irbs $ funcHdr <**> (funcSig <|> funcDef) where
--     funcDef, funcSig :: Parser (IRBS SomeTerm -> IRB SomeTerm)
--     funcHdr     = symbol Lexer.KwDef *> withRecovery headerRec (var <|> op)
--     headerRec e = irbs $ invalid Invalid.FunctionHeader
--                <$ Loc.unregisteredDropSymbolsUntil
--                   (`elem` [Lexer.BlockStart, Lexer.EOL, Lexer.ETX])
--     funcSig     = (\tp name -> liftIRBS2 (irb2 IR.functionSig') name tp)
--                <$ symbol Lexer.Typed <*> valExpr
--     funcDef     = (\args body name -> liftIRBS3 (irb3 IR.function') name
--                                       (sequence args) (seqs2 body))
--               <$> many nonSpacedPattern
--               <*> withRecovery blockRec block
--     block       = symbol Lexer.BlockStart
--                *> (possibleNonEmptyBlock lineExpr <|> missing)
--     missing :: Parser (NonEmpty (IRBS SomeTerm))
--     missing = fmap pure . irbs . pure $ irb0 IR.missing'
--     blockRec e  = pure <$> (irbs $ invalid Invalid.FunctionBlock
--                <$ optionalBlockAny)


-- -- -- === Classes == --

-- record :: Parser (IRBS SomeTerm)
-- record = irbs $ (\nat n args (cs,ds) -> liftIRBS3 (irb5 IR.record' nat n) (sequence args) (sequence cs) (sequence ds))
--    <$> try (option False (True <$ symbol Lexer.KwNative) <* symbol Lexer.KwClass) <*> consName <*> many var <*> body
--     where body      = option mempty $ symbol Lexer.BlockStart *> bodyBlock
--           funcBlock = optionalBlockBody (possiblyDocumented func)
--           consBlock = breakableNonEmptyBlockBody' recordCons <|> breakableOptionalBlockBody recordNamedFields
--           bodyBlock = discoverIndent ((,) <$> consBlock <*> funcBlock)

-- recordCons :: Parser (IRBS SomeTerm)
-- recordCons = irbs $ (\n fields -> liftIRBS1 (irb2 IR.recordCons' n) (sequence fields)) <$> consName <*> (blockDecl <|> inlineDecl) where
--     blockDecl  = symbol Lexer.BlockStart *> possibleNonEmptyBlock' recordNamedFields
--     inlineDecl = many unnamedField

-- recordNamedFields :: Parser (IRBS SomeTerm)
-- recordNamedFields = irbs $ do
--     varNames <- Mutable.fromList =<< many varName
--     liftIRBS1 (irb2 IR.recordFields' varNames) <$ symbol Lexer.Typed <*> valExpr

-- unnamedField :: Parser (IRBS SomeTerm)
-- unnamedField = do
--     m <- Mutable.new
--     irbs $ liftIRBS1 (irb2 IR.recordFields' m)
--                       <$> nonSpacedValExpr



-- --------------------
-- -- === Units === --
-- --------------------

-- -- === Imports === --

-- impHub :: Parser (IRBS SomeTerm)
-- impHub = irbs $ (\(imps :: [IRBS SomeTerm])
--                  -> liftIRBS1 (irb1 IR.importHub')
--                     (sequence imps :: IRBS [SomeTerm])) <$> impHeader

-- impHeader :: Parser [IRBS SomeTerm]
-- impHeader = option mempty $ breakableOptionalBlockTop imp -- <* skipEmptyLines

-- imp :: Parser (IRBS SomeTerm)
-- imp = irbs $ (\a tgts -> liftIRBS1 (flip (irb2 IR.imp') tgts) a)
--    <$ symbol Lexer.KwImport <*> importSource <*> impTgt where
--     impTgt  = option Import.Everything $ symbol Lexer.BlockStart *> listTgt
--     listTgt = Import.Listed <$> (Mutable.fromList =<< many (varName <|> consName))

-- importSource :: Parser (IRBS SomeTerm)
-- importSource = irbs . fmap (irb1 IR.importSource') $ choice
--     [ Import.World    <$  matchCons "World"
--     , Import.Relative <$  symbol Lexer.Accessor <*> (convert <$> qualConsName)
--     , Import.Absolute . convert <$> qualConsName
--     ]


-- -- ------------------------------------
-- -- -- === Foreign Import Parsing === --+
-- -- ------------------------------------

-- foreignImportList :: Parser (IRBS SomeTerm)
-- foreignImportList = irbs $
--     (\lang imports ->
--         liftIRBS1 (irb2 IR.foreignImport' lang) (sequence imports))
--     <$  (symbol Lexer.KwForeign *> symbol Lexer.KwImport)
--     <*> foreignLangName
--     <*  symbol Lexer.BlockStart
--     <*> possibleNonEmptyBlock' foreignLocationImportList

-- foreignLocationImportList :: Parser (IRBS SomeTerm)
-- foreignLocationImportList = irbs $
--     (\loc imports ->
--         liftIRBS2 (irb2 IR.foreignImportList') loc (sequence imports))
--     <$> stringOrVarName
--     <*  symbol Lexer.BlockStart
--     <*> possibleNonEmptyBlock' foreignSymbolImport

-- foreignSymbolImport :: Parser (IRBS SomeTerm)
-- foreignSymbolImport = irbs $ withRecovery recover
--     $   try (foreignSymbolImportWithSafety defaultFISafety)
--     <|> foreignSymbolImportWithSafety specifiedFISafety
--     where recover e = invalid Invalid.ForeignImportSafety
--                       <$ Loc.unregisteredDropSymbolsUntil
--                       (`elem` [Lexer.ETX, Lexer.EOL])

-- foreignSymbolImportWithSafety :: Parser (IRBS SomeTerm) -> Parser (IRB SomeTerm)
-- foreignSymbolImportWithSafety safe =
--     (\safety forName localName importType ->
--         liftIRBS3 (foreignSymbolProxy localName) safety forName importType)
--     <$> safe <*> stringOrVarName <*> funcName <*  symbol Lexer.Typed <*> valExpr
--     where foreignSymbolProxy a b c d = irb4 IR.foreignImportSymbol' b c a d

-- defaultFISafety :: Parser (IRBS SomeTerm)
-- defaultFISafety = irbs $
--     (\safety -> id (irb1 IR.foreignImportSafety' safety))
--     <$> ((pure Import.Default) :: Parser IR.ForeignImportType)

-- -- TODO [Ara, WD] Need to have the lexer deal with these as contextual symbols
-- -- using a positive lookahead in the _Lexer_.
-- specifiedFISafety :: Parser (IRBS SomeTerm)
-- specifiedFISafety = irbs $
--     (\(importSafety :: IR.ForeignImportType) ->
--         id (irb1 (IR.foreignImportSafety') importSafety))
--     <$> getImpSafety (optionMaybe varName)
--     where getImpSafety :: Parser (Maybe Name) -> Parser IR.ForeignImportType
--           getImpSafety p = p >>= \case
--               Nothing -> pure Import.Default
--               Just n  -> case n of
--                   "safe"   -> pure Import.Safe
--                   "unsafe" -> pure Import.Unsafe
--                   _        -> fail "Invalid safety specification."

-- stringOrVarName :: Parser (IRBS SomeTerm)
-- stringOrVarName = string <|> (asgNameParser varName)

-- asgNameParser :: Parser Name -> Parser (IRBS SomeTerm)
-- asgNameParser nameParser = irbs $ (\varN -> id (irb1 IR.var' varN))
--      <$> nameParser



-- -- -- === Unit body === --

-- unit' :: Parser (IRBS SomeTerm)
-- unit  :: Parser (IRBS (IR.Term IR.Unit))
-- unit' = Layout.relayout <<$>> unit
-- unit  = irbs $
--     (\imps cls
--         -> Layout.unsafeRelayout <$> liftIRBS2 (flip (irb3 IR.unit) []) imps cls)
--         <$ spacing <*> (foreignImportList <|> impHub) <*> unitCls <* spacing
--     where spacing = many eol

-- unitCls :: Parser (IRBS SomeTerm)
-- unitCls = irbs $ (\ds -> liftIRBS1 (irb5 IR.record' False "" [] [])
--                      (sequence ds)) <$> optionalBlockTop topLvlDecl



-- ----------------------------
-- -- === Layout parsing === --
-- ----------------------------

-- skipEOLs :: Parser ()
-- skipEOLs = void $ many eol

-- possibleNonEmptyBlock :: Parser a -> Parser (NonEmpty a)
-- possibleNonEmptyBlock = \p -> (es >> nonEmptyBlock2 p) <|> (pure <$> p) where
--     es = try (eol >> skipEOLs >> Indent.indented)
-- {-# INLINE possibleNonEmptyBlock #-}

-- possibleNonEmptyBlock' :: Parser a -> Parser [a]
-- possibleNonEmptyBlock' = fmap convert . possibleNonEmptyBlock
-- {-# INLINE possibleNonEmptyBlock' #-}

-- discoverIndent :: Parser a -> Parser a
-- discoverIndent = discover . Indent.withCurrent where
--     discover p = skipEOLs >> p

-- nonEmptyBlock', nonEmptyBlockBody' :: Parser a -> Parser [a]
-- nonEmptyBlock'     = uncurry (:) <<$>> nonEmptyBlock
-- nonEmptyBlockBody' = uncurry (:) <<$>> nonEmptyBlockBody

-- nonEmptyBlock2 ::  Parser a -> Parser (NonEmpty a)
-- nonEmptyBlock2 p = Indent.withCurrent (nonEmptyBlockBody2 p)

-- nonEmptyBlockBody2 ::  Parser a -> Parser (NonEmpty a)
-- nonEmptyBlockBody2 p = (:|) <$> p <*> lines where
--     spacing = many eol
--     indent  = spacing <* Indent.indentedEq <* notFollowedBy etx
--     lines   = many $ try indent *> p

-- nonEmptyBlock , nonEmptyBlockBody  :: Parser a -> Parser (a, [a])
-- nonEmptyBlock     p = Indent.indented >> Indent.withCurrent (nonEmptyBlockBody p)
-- nonEmptyBlockTop    = Indent.withRoot    . nonEmptyBlockBody
-- nonEmptyBlockBody p = (,) <$> p <*> lines where
--     spacing = many eol
--     indent  = spacing <* Indent.indentedEq <* notFollowedBy etx
--     lines   = many $ try indent *> p

-- optionalBlock, optionalBlockTop, optionalBlockBody :: Parser a -> Parser [a]
-- optionalBlock     p = option mempty $ uncurry (:) <$> nonEmptyBlock     p
-- optionalBlockTop  p = option mempty $ uncurry (:) <$> nonEmptyBlockTop  p
-- optionalBlockBody p = option mempty $ uncurry (:) <$> nonEmptyBlockBody p

-- breakableNonEmptyBlock', breakableNonEmptyBlockTop', breakableNonEmptyBlockBody' :: Parser a -> Parser     [a]
-- breakableNonEmptyBlock , breakableNonEmptyBlockTop , breakableNonEmptyBlockBody  :: Parser a -> Parser (a, [a])
-- breakableNonEmptyBlock'      = uncurry (:) <<$>> breakableNonEmptyBlock
-- breakableNonEmptyBlockTop'   = uncurry (:) <<$>> breakableNonEmptyBlockTop
-- breakableNonEmptyBlockBody'  = uncurry (:) <<$>> breakableNonEmptyBlockBody
-- breakableNonEmptyBlock       = Indent.withCurrent . breakableNonEmptyBlockBody
-- breakableNonEmptyBlockTop    = Indent.withRoot    . breakableNonEmptyBlockBody
-- breakableNonEmptyBlockBody p = (,) <$> lexedp <*> lines where
--     spacing = many eol
--     lines   = many $ Indent.indentedEq *> lexedp
--     lexedp  = p <* spacing

-- breakableOptionalBlock, breakableOptionalBlockTop, breakableOptionalBlockBody :: Parser a -> Parser [a]
-- breakableOptionalBlock     p = option mempty $ uncurry (:) <$> breakableNonEmptyBlock     p
-- breakableOptionalBlockTop  p = option mempty $ uncurry (:) <$> breakableNonEmptyBlockTop  p
-- breakableOptionalBlockBody p = option mempty $ uncurry (:) <$> breakableNonEmptyBlockBody p


-- nonEmptyBlockAny :: Parser [Token]
-- nonEmptyBlockAny = concat . convertTo @[[Token]] <$> some line where
--     line = Indent.indented >> (body <* eol)
--     body = Loc.getTokensUntil (\t -> (t == Lexer.EOL) || (t == Lexer.ETX))

-- optionalBlockAny :: Parser [Token]
-- optionalBlockAny = option mempty nonEmptyBlockAny






































-- --------------------
-- -- === Parser === --
-- --------------------

-- type FileName = Literal.String


-- type ParsingPassReq2 m = Req m '[ Editor // Net   // '[AnyExpr, AnyExprLink]
--                                , Editor  // Layer // AnyExpr // CodeSpan
--                                , Reader  // Attr  // Source
--                                , Writer  // Attr  // '[ParsedExpr, Marker.TermMap]
--                                , Editor  // Attr  // Invalids
--                                , Emitter // New   // '[AnyExpr, AnyExprLink]

--                                -- codegen & codespan testing
--                                , Reader // Layer // AnyExprLink // Model
--                                , Reader // Layer // AnyExpr     // Model
--                                , Reader // Layer // AnyExpr     // Succs
--                                ]


-- type ParsingPassReq_2 m = Req m '[ Editor // Net   // '[AnyExpr, AnyExprLink]
--                                  , Editor // Layer // AnyExpr // CodeSpan
--                                  , Editor // Attr  // Invalids
--                                  , Emitter // New   // '[AnyExpr, AnyExprLink]

--                                  -- codegen & codespan testing
--                                  , Reader // Layer // AnyExprLink // Model
--                                  , Reader // Layer // AnyExpr     // Model
--                                  , Reader // Layer // AnyExpr     // Succs
--                                  ]




-- parsingPassM :: (MonadPassManager m, ParsingPassReq2 m) => Parser (IRBS SomeTerm) -> m ()
-- parsingPassM p = do
--     src <- getAttr @Source
--     (ref, gidMap) <- parsingBase p (convert src)
--     putAttr @ParsedExpr    $ wrap ref
--     putAttr @Marker.TermMap $ gidMap






-- parsingBase_ :: ( MonadPassManager m, ParsingPassReq_2 m
--                 , UnsafeGeneralizable a SomeTerm, UnsafeGeneralizable a SomeTerm -- FIXME[WD]: Constraint for testing only
--                 ) => IRBSParser a -> Text32 -> m a
-- parsingBase_ = view _1 .:. parsingBase

-- parserPassX  :: MonadPassManager m => Parser (IRBS SomeTerm) -> Pass Parsing   m
-- parserPassX  = parsingPassM

-- reparserPass :: MonadPassManager m => Parser (IRBS SomeTerm) -> Pass Reparsing m
-- reparserPass p = do
--     -- Reading previous analysis
--     gidMapOld <- getAttr @Marker.TermMap
--     refOld    <- getAttr @ParsedExpr

--     -- parsing new file and updating updated analysis
--     putAttr @Marker.TermMap mempty
--     parsingPassM p
--     gidMap    <- getAttr @Marker.TermMap
--     ref       <- getAttr @ParsedExpr

--     -- Preparing reparsing status
--     rs        <- cmpMarkedExprMaps gidMapOld gidMap
--     putAttr @ReparsingStatus (wrap rs)



-- cmpMarkedExprMaps :: IsomorphicCheckCtx m => Marker.TermMap -> Marker.TermMap -> m [ReparsingChange]
-- cmpMarkedExprMaps (oldMap'@(_unwrap -> oldMap)) (_unwrap -> newMap) = (remExprs <>) <$> mapM (uncurry $ cmpMarkedExpr oldMap') newAssocs where
--     newAssocs = Map.assocs newMap
--     oldAssocs = Map.assocs oldMap
--     remExprs  = fmap RemovedExpr . catMaybes $ (\(k,v) -> if_ (not $ Map.member k newMap) (Just v)) <$> oldAssocs

-- cmpMarkedExpr :: IsomorphicCheckCtx m => Marker.TermMap -> Marker.ID -> SomeTerm -> m ReparsingChange
-- cmpMarkedExpr (_unwrap -> map) mid newExpr = case map ^. at mid of
--     Nothing      -> pure $ AddedExpr newExpr
--     Just oldExpr -> checkIsoExpr (unsafeGeneralize oldExpr) (unsafeGeneralize newExpr) <&> \case -- FIXME [WD]: remove unsafeGeneralize, we should use SomeTerm / SomeTerm everywhere
--         False -> ChangedExpr   oldExpr newExpr
--         True  -> UnchangedExpr oldExpr newExpr













--------------------


-- ---------------------
-- -- === Satisfy === --
-- ---------------------

-- satisfy    :: (Lexer.Symbol -> Bool)    -> Parser Lexer.Symbol
-- satisfy_   :: (Lexer.Symbol -> Bool)    -> Parser ()
-- satisfTest :: (Lexer.Symbol -> Maybe a) -> Parser a
-- satisfy_   f = void $ satisfy f
-- satisfy    f = satisfTest $ \s -> justIf (f s) s
-- satisfTest f = token' test where
--     test r t = if Reserved.lookup r (t ^. Lexer.symbol)
--         then Left (Just $ Tokens (pure t), Set.empty)
--         else satisfyTestSymbol f t
-- {-# INLINE satisfy_   #-}
-- {-# INLINE satisfy    #-}
-- {-# INLINE satisfTest #-}

-- satisfyUnchecked     :: (Lexer.Symbol -> Bool)    -> Parser Lexer.Symbol
-- satisfyUnchecked_    :: (Lexer.Symbol -> Bool)    -> Parser ()
-- satisfyUncheckedTest :: (Lexer.Symbol -> Maybe a) -> Parser a
-- satisfyUnchecked_    f = void $ satisfyUnchecked f
-- satisfyUnchecked     f = satisfyUncheckedTest $ \s -> justIf (f s) s
-- satisfyUncheckedTest f = token' (const $ satisfyTestSymbol f)
-- {-# INLINE satisfyUnchecked_    #-}
-- {-# INLINE satisfyUnchecked     #-}
-- {-# INLINE satisfyUncheckedTest #-}

-- satisfyTestSymbol :: (Lexer.Symbol -> Maybe b) -> Lexer.Token
--                   -> Either (Maybe (ErrorItem Lexer.Token), Set a) b
-- satisfyTestSymbol f t = note (Just $ Tokens (pure t), Set.empty)
--                       $ f (t ^. Lexer.symbol)
-- {-# INLINE satisfyTestSymbol #-}

-- symbol :: Lexer.Symbol -> Parser ()
-- symbol = satisfy_ . (==)
-- {-# INLINE symbol #-}

-- anySymbol  :: Parser Lexer.Symbol
-- anySymbol_ :: Parser ()
-- anySymbol  = satisfyUnchecked $ const True
-- anySymbol_ = void anySymbol
-- {-# INLINE anySymbol #-}
-- {-# INLINE anySymbol_ #-}



-- checkLastOffset :: State.Getter LastOffset m => m Bool
-- checkLastOffset = (>0)   <$> getLastOffset
-- {-# INLINE getLastOffset   #-}
-- {-# INLINE checkLastOffset #-}

-- checkOffsets :: (MonadParsec e Stream m, State.Getter LastOffset m)
--              => m (Bool, Bool)
-- checkOffsets = (,) <$> checkLastOffset <*> checkNextOffset
-- {-# INLINE checkOffsets #-}



----------------------------------
-- -- === Code span management === --
-- ----------------------------------

-- attachCodeSpanLayer :: CodeSpan -> IRB (Term a) -> IRB (Term a)
-- attachCodeSpanLayer s = withIRB (>>~ flip (Layer.write @CodeSpan) s) ; {-# INLINE attachCodeSpanLayer #-}

-- computeSpan :: Parser a -> Parser (CodeSpan, a)
-- computeSpan p = do
--     lastCodeSpan <- unwrap <$> State.get @CodeSpanRange
--     fileOffStart <- unwrap <$> State.get @FileOffset
--     marker       <- Marker.getLast
--     State.put @CodeSpanRange $ wrap fileOffStart
--     out          <- p
--     fileOffEnd   <- unwrap <$> State.get @FileOffset
--     lastOffset   <- getLastOffset
--     let end       = fileOffEnd   - lastOffset
--         off       = fileOffStart - lastCodeSpan
--         emptySpan = Span.leftSpacedSpan mempty mempty
--         realLen   = max 0 (end - fileOffStart)
--         newRange  = max end fileOffStart
--         realSpan  = Span.leftSpacedSpan off realLen
--         viewSpan  = case marker of
--             Nothing -> realSpan
--             Just m  -> Span.leftSpacedSpan
--                        (off - m ^. Lexer.span - m ^. Lexer.offset) realLen
--     State.put @CodeSpanRange $ convert newRange
--     pure (CodeSpan realSpan viewSpan, out)
-- {-# INLINE computeSpan #-}

-- spanOf :: Parser a -> Parser a
-- spanOf = fmap snd . computeSpan ; {-# INLINE spanOf #-}

-- inheritCodeSpan1 :: (     SomeTerm -> IRB  SomeTerm)
--                  -> (IRBS SomeTerm -> IRBS SomeTerm)
-- inheritCodeSpan1 = inheritCodeSpan1With id ; {-# INLINE inheritCodeSpan1 #-}

-- inheritCodeSpan2 :: (     SomeTerm ->      SomeTerm -> IRB  SomeTerm)
--                  -> (IRBS SomeTerm -> IRBS SomeTerm -> IRBS SomeTerm)
-- inheritCodeSpan2 = inheritCodeSpan2With (CodeSpan.concat) ; {-# INLINE inheritCodeSpan2 #-} -- FIXME Is CodeSpan.concat deprecated?

-- inheritCodeSpan1With :: (CodeSpan -> CodeSpan)
--                      -> (     SomeTerm -> IRB  SomeTerm)
--                      -> (IRBS SomeTerm -> IRBS SomeTerm)
-- inheritCodeSpan1With sf f (IRBS (IRB irb1)) = wrap $ IRB $ do
--     t1 <- irb1
--     s1 <- Layer.read @CodeSpan t1
--     let s1' = CodeSpan.dropOffset s1
--     -- The new IR becomes a new parent, so it handles the left offset.
--     Layer.write @CodeSpan t1 s1'
--     fromIRB $ unwrap $ irbsFromSpan (sf s1) $ f t1
-- {-# INLINE inheritCodeSpan1With #-}

-- inheritCodeSpan2With :: (CodeSpan -> CodeSpan -> CodeSpan)
--                      -> (     SomeTerm ->      SomeTerm -> IRB  SomeTerm)
--                      -> (IRBS SomeTerm -> IRBS SomeTerm -> IRBS SomeTerm)
-- inheritCodeSpan2With sf f (IRBS (IRB irb1)) (IRBS (IRB irb2)) = wrap $ IRB $ do
--     t1 <- irb1
--     s1 <- Layer.read @CodeSpan t1
--     -- The new IR becomes a new parent, so it handles the left offset.
--     let s1' = CodeSpan.dropOffset s1
--     Layer.write @CodeSpan t1 s1'
--     t2 <- irb2
--     s2 <- Layer.read @CodeSpan t2
--     fromIRB $ unwrap $ irbsFromSpan (sf s1 s2) $ f t1 t2
-- {-# INLINE inheritCodeSpan2With #-}

-- -- | Magic helper. Use with care only if you really know what you do.
-- unsafeModifyCodeSpan :: (CodeSpan -> CodeSpan) -> IRBS SomeTerm -> IRBS SomeTerm
-- unsafeModifyCodeSpan f (IRBS (IRB irb1)) = wrap $ IRB $ do
--     t1 <- irb1
--     s1 <- Layer.read @CodeSpan t1
--     Layer.write @CodeSpan t1 (f s1)
--     pure t1
-- {-# INLINE unsafeModifyCodeSpan #-}


-- -- === IRBS construction === --

-- irbsFromSpan :: CodeSpan -> IRB (Term a) -> IRBS (Term a)
-- irbsFromSpan = IRBS .: attachCodeSpanLayer ; {-# INLINE irbsFromSpan #-}

-- irbs   ::                    Parser       (IRB (Term a))   -> Parser       (IRBS (Term a))
-- irbsF  :: Functor f       => Parser (f    (IRB (Term a)))  -> Parser (f    (IRBS (Term a)))
-- irbsF2 :: Functors '[f,g] => Parser (f (g (IRB (Term a)))) -> Parser (f (g (IRBS (Term a))))
-- irbs   p = uncurry          irbsFromSpan  <$> computeSpan p ; {-# INLINE irbs   #-}
-- irbsF  p = uncurry (fmap  . irbsFromSpan) <$> computeSpan p ; {-# INLINE irbsF  #-}
-- irbsF2 p = uncurry (fmap2 . irbsFromSpan) <$> computeSpan p ; {-# INLINE irbsF2 #-}


-- -- irbsFromSpan :: CodeSpan -> IRB (Term a) -> IRBS (Term a)
-- -- irbsFromSpan = IRBS .: attachCodeSpanLayer ; {-# INLINE irbsFromSpan #-}

-- spanned :: Parser UnspannedAst -> Parser Ast.Spanned
-- spanned = \p -> uncurry Ast.Spanned <$> computeSpan p
-- {-# INLINE spanned #-}



--------------------
-- === Errors === --
--------------------

-- invalid :: Invalid.Symbol -> IRB SomeTerm
-- invalid t = IRB $ do
--     inv <- IR.invalid' t
--     Attr.registerInvalid inv
--     pure $ Layout.relayout inv
-- {-# INLINE invalid #-}

-- invalidToken :: Parser (IRBS SomeTerm)
-- invalidToken = irbs $ invalid <$> satisfTest Lexer.matchInvalid
-- {-# INLINE invalidToken #-}

-- -- invalidSymbol :: (Lexer.Symbol -> Text32) -> Parser (IRBS SomeTerm)
-- -- invalidSymbol f = irbs $ invalid . f <$> anySymbol

-- -- catchParseErrors :: Parser a -> Parser (Either String a)
-- -- catchParseErrors p = withRecovery2 (pure . Left . parseErrorTextPretty)
-- --                                    (Right <$> p)

-- -- catchInvalidWith :: HasCallStack => (Span.LeftSpacedSpan -> Span.LeftSpacedSpan)
-- --                  -> (IRBS SomeTerm -> a) -> Parser a -> Parser a
-- -- catchInvalidWith spanf f p = undefined -- do
-- --     -- (span, result) <- computeSpan $ catchParseErrors p
-- --     -- pure $ flip fromRight result $ f . irbsFromSpan (spanf span) . invalid . convert



-- ---------------------
-- -- === Markers === --
-- ---------------------

-- markerIRB :: Parser (Marker.ID, IRBS SomeTerm)
-- markerIRB = Marker.getAndClearLast >>= \case
--     Nothing -> expected "marker"
--     Just t  -> do
--         crange <- unwrap <$> State.get @CodeSpanRange
--         foEnd  <- unwrap <$> State.get @FileOffset
--         let markerLen  = t ^. Lexer.span
--             markerOffR = t ^. Lexer.offset
--             markerOffL = foEnd - crange - markerLen - markerOffR
--             markerSpan = Span.leftSpacedSpan markerOffL markerLen
--         State.modify_ @CodeSpanRange $ wrapped .~ foEnd
--         pure $ ( t ^. Marker.markerID
--                , irbsFromSpan (CodeSpan.mkPhantomSpan markerSpan)
--                                   (id $ irb1 IR.marker' $ t ^. Marker.markerID)
--                )

-- marked :: Parser (IRBS SomeTerm -> IRBS SomeTerm)
-- marked = option registerUnmarkedExpr $ uncurry markedExpr <$> markerIRB where
--     markedExpr mid expr = registerMarkedExpr mid
--                         . inheritCodeSpan2 (irb2 IR.marked') expr

-- registerUnmarkedExpr ::              IRBS SomeTerm -> IRBS SomeTerm
-- registerMarkedExpr   :: Marker.ID -> IRBS SomeTerm -> IRBS SomeTerm
-- registerUnmarkedExpr = wrapped %~ withIRB (>>~ Marker.registerOrphan)
-- registerMarkedExpr m = wrapped %~ withIRB (>>~ Marker.register m)
-- {-# INLINE registerUnmarkedExpr #-}
-- {-# INLINE registerMarkedExpr   #-}



-- ---------------------
-- -- === Symbols === --
-- ---------------------

-- stx, etx, eol :: Parser ()
-- stx = symbol Lexer.STX ; {-# INLINE stx #-}
-- etx = symbol Lexer.ETX ; {-# INLINE etx #-}
-- eol = symbol Lexer.EOL ; {-# INLINE eol #-}

-- braceBegin, braceEnd :: Parser ()
-- braceBegin = symbol $ Lexer.List Lexer.Begin ; {-# INLINE braceBegin #-}
-- braceEnd   = symbol $ Lexer.List Lexer.End   ; {-# INLINE braceEnd   #-}

-- groupBegin, groupEnd :: Parser ()
-- groupBegin = symbol $ Lexer.Group Lexer.Begin ; {-# INLINE groupBegin #-}
-- groupEnd   = symbol $ Lexer.Group Lexer.End   ; {-# INLINE groupEnd   #-}

-- parensed :: Parser a -> Parser a
-- parensed p = groupBegin *> p <* groupEnd ; {-# INLINE parensed #-}




-- var :: Parser (IRBS SomeTerm)
-- cons, var, op, wildcard :: Parser (IRBS SomeTerm)
-- cons     = snd <$> namedCons                             ; {-# INLINE cons     #-}
-- var      = snd <$> namedVar                              ; {-# INLINE var      #-}
-- op       = snd <$> namedOp                               ; {-# INLINE op       #-}
-- wildcard = irbs $ irb0 IR.blank' <$ symbol Lexer.Wildcard ; {-# INLINE wildcard #-}

-- namedVar :: Parser (Name, IRBS SomeTerm)
-- namedCons, namedVar, namedOp, namedIdent :: Parser (Name, IRBS SomeTerm)
-- namedCons  = irbsNamed (flip (irb2 IR.cons') []) consName ; {-# INLINE namedCons  #-}
-- namedVar   = undefined -- irbsNamed (irb1 IR.var')            varName  ; {-# INLINE namedVar   #-}
-- namedOp    = irbsNamed (irb1 IR.var')            opName   ; {-# INLINE namedOp    #-}
-- namedIdent = namedVar <|> namedCons <|> namedOp    ; {-# INLINE namedIdent #-}


-- === Helpers === --

-- irbsNamed :: (Name -> IRB (Term a)) -> Parser Name -> Parser (Name, IRBS (Term a))
-- irbsNamed cons = irbsF . fmap (\name -> (name, cons name)) ; {-# INLINE irbsNamed #-}

