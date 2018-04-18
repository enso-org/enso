{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE UndecidableInstances      #-}

module Luna.Syntax.Text.Parser.IR.Term where

import qualified Prelude                 as P
import           Prologue                hiding (seq, some)
import qualified Prologue
import           Text.Parser.Combinators

import qualified Control.Monad.State.Layered            as State
import qualified Data.Char                              as Char
import qualified Data.Set                               as Set
import qualified Data.Text.Span                         as Span
import qualified Data.Text32                            as Text32
import qualified Data.TreeSet                           as TreeSet
import qualified Language.Symbol                        as Symbol
import qualified Luna.Data.Name                         as Name
import qualified Luna.IR                                as IR
import qualified Luna.IR.Layer                          as Layer
import qualified Luna.IR.Term.Ast                       as Import
import qualified Luna.IR.Term.Ast.Invalid               as Invalid
import qualified Luna.Syntax.Text.Lexer                 as Lexer
import qualified Luna.Syntax.Text.Lexer.Symbol          as Lexer
import qualified Luna.Syntax.Text.Parser.Data.CodeSpan  as CodeSpan
import qualified Luna.Syntax.Text.Parser.Data.Invalid   as Attr
import qualified Luna.Syntax.Text.Parser.IR.Expr           as Expr
import qualified Luna.Syntax.Text.Parser.Loc            as Loc
import qualified Luna.Syntax.Text.Parser.Data.Name.Spaced    as Name
import qualified Luna.Syntax.Text.Parser.Data.Name.Special   as SpecialName
import qualified Luna.Syntax.Text.Parser.State.Indent   as Indent
import qualified Luna.Syntax.Text.Parser.State.Marker   as Marker
import qualified Luna.Syntax.Text.Parser.State.Reserved as Reserved
import qualified Luna.Syntax.Text.Scope                 as Scope
import qualified OCI.Data.Name.Multipart                as Name.Multipart
import qualified OCI.IR.Layout                          as Layout


import Data.List.NonEmpty                       ((<|))
import Data.Set                                 (Set)
import Data.Text.Position                       (FileOffset (..))
import Data.Text.Position                       (Delta)
import Data.Text32                              (Text32)
import Data.TreeSet                             (SparseTreeSet)
import Language.Symbol                          (Labeled (Labeled), SomeSymbol,
                                                 labeled)
import Luna.IR                                  (SomeTerm, Term)
import Luna.Pass                                (Pass)
import Luna.Syntax.Text.Parser.IR.Class            (Parser, Stream, Token)
import Luna.Syntax.Text.Parser.Data.CodeSpan    (CodeSpan (CodeSpan),
                                                 CodeSpanRange (..))
import Luna.Syntax.Text.Parser.Loc              (checkNextOffset,
                                                 previewNextSymbol, token')
import Luna.Syntax.Text.Parser.Data.Name.Spaced      (SpacedName)
import Luna.Syntax.Text.Parser.Pass.Class       (IRB, IRBS (IRBS), fromIRBS,
                                                 liftIRBS1, liftIRBS2,
                                                 liftIRBS3)
import Luna.Syntax.Text.Parser.State.LastOffset (LastOffset (LastOffset))
import OCI.Data.Name                            (Name)
import Text.Megaparsec                          (ErrorItem (Tokens),
                                                 MonadParsec, ParseError,
                                                 between, choice, hidden,
                                                 lookAhead, manyTill,
                                                 notFollowedBy, skipMany, try,
                                                 unexpected, withRecovery)
import Text.Megaparsec.Char                     (anyChar, char, digitChar,
                                                 letterChar, lowerChar,
                                                 spaceChar, upperChar)
import Text.Megaparsec.Error                    (parseErrorPretty,
                                                 parseErrorTextPretty)
import Text.Megaparsec.Ext                      (expected)


-- TODO: Can we do better?
instance Convertible Text32.Text32 Name where
    convert = convertVia @String ; {-# INLINE convert #-}



---------------------
-- === Satisfy === --
---------------------

satisfy    :: (Lexer.Symbol -> Bool)    -> Parser Lexer.Symbol
satisfy_   :: (Lexer.Symbol -> Bool)    -> Parser ()
satisfTest :: (Lexer.Symbol -> Maybe a) -> Parser a
satisfy_   f = void $ satisfy f                  ; {-# INLINE satisfy_ #-}
satisfy    f = satisfTest $ \s -> justIf (f s) s ; {-# INLINE satisfy  #-}
satisfTest f = token' test Nothing where
    test r t = case Reserved.lookup r (t ^. Lexer.element) of
        True  -> Left (Just $ Tokens (pure t), Set.empty)
        False -> satisfyTestSymbol f t
{-# INLINE satisfTest #-}

satisfyUnchecked     :: (Lexer.Symbol -> Bool)    -> Parser Lexer.Symbol
satisfyUnchecked_    :: (Lexer.Symbol -> Bool)    -> Parser ()
satisfyUncheckedTest :: (Lexer.Symbol -> Maybe a) -> Parser a
satisfyUnchecked_    f = void $ satisfyUnchecked f                    ; {-# INLINE satisfyUnchecked_    #-}
satisfyUnchecked     f = satisfyUncheckedTest $ \s -> justIf (f s) s  ; {-# INLINE satisfyUnchecked     #-}
satisfyUncheckedTest f = token' (const $ satisfyTestSymbol f) Nothing ; {-# INLINE satisfyUncheckedTest #-}

satisfyTestSymbol :: (t -> Maybe b) -> Lexer.Token t
                  -> Either (Maybe (ErrorItem (Lexer.Token t)), Set a) b
satisfyTestSymbol f t = note (Just $ Tokens (pure t), Set.empty)
                      $ f (t ^. Lexer.element)
{-# INLINE satisfyTestSymbol #-}

symbol :: Lexer.Symbol -> Parser ()
symbol = satisfy_ . (==) ; {-# INLINE symbol #-}

anySymbol  :: Parser Lexer.Symbol
anySymbol_ :: Parser ()
anySymbol  = satisfyUnchecked $ const True ; {-# INLINE anySymbol #-}
anySymbol_ = void anySymbol                ; {-# INLINE anySymbol_ #-}

getLastOffset   :: State.Getter LastOffset m => m Delta
checkLastOffset :: State.Getter LastOffset m => m Bool
getLastOffset   = unwrap <$> State.get @LastOffset ; {-# INLINE getLastOffset   #-}
checkLastOffset = (>0)   <$> getLastOffset          ; {-# INLINE checkLastOffset #-}

checkOffsets :: (MonadParsec e Stream m, State.Getter LastOffset m)
             => m (Bool, Bool)
checkOffsets = (,) <$> checkLastOffset <*> checkNextOffset ; {-# INLINE checkOffsets #-}



----------------------------------
-- === Code span management === --
----------------------------------

attachCodeSpanLayer :: CodeSpan -> IRB (Term a) -> IRB (Term a)
attachCodeSpanLayer s = (>>~ flip (Layer.write @CodeSpan) s) ; {-# INLINE attachCodeSpanLayer #-}

spanned :: Parser a -> Parser (CodeSpan, a)
spanned p = do
    lastCodeSpan <- unwrap <$> State.get @CodeSpanRange
    fileOffStart <- unwrap <$> State.get @FileOffset
    marker       <- Marker.getLast
    State.put @CodeSpanRange $ wrap fileOffStart
    out          <- p
    fileOffEnd   <- unwrap <$> State.get @FileOffset
    lastOffset   <- getLastOffset
    let end       = fileOffEnd   - lastOffset
        off       = fileOffStart - lastCodeSpan
        emptySpan = Span.leftSpacedSpan mempty mempty
        realLen   = max 0 (end - fileOffStart)
        newRange  = max end fileOffStart
        realSpan  = Span.leftSpacedSpan off realLen
        viewSpan  = case marker of
            Nothing -> realSpan
            Just m  -> Span.leftSpacedSpan
                       (off - m ^. Lexer.span - m ^. Lexer.offset) realLen
    State.put @CodeSpanRange $ convert newRange
    pure (CodeSpan realSpan viewSpan, out)
{-# INLINE spanned #-}

spanOf :: Parser a -> Parser a
spanOf = fmap snd . spanned ; {-# INLINE spanOf #-}

inheritCodeSpan1 :: (     SomeTerm -> IRB  SomeTerm)
                 -> (IRBS SomeTerm -> IRBS SomeTerm)
inheritCodeSpan1 = inheritCodeSpan1With id ; {-# INLINE inheritCodeSpan1 #-}

inheritCodeSpan2 :: (     SomeTerm ->      SomeTerm -> IRB  SomeTerm)
                 -> (IRBS SomeTerm -> IRBS SomeTerm -> IRBS SomeTerm)
inheritCodeSpan2 = inheritCodeSpan2With (CodeSpan.concat) ; {-# INLINE inheritCodeSpan2 #-} -- FIXME Is CodeSpan.concat deprecated?

inheritCodeSpan1With :: (CodeSpan -> CodeSpan)
                     -> (     SomeTerm -> IRB  SomeTerm)
                     -> (IRBS SomeTerm -> IRBS SomeTerm)
inheritCodeSpan1With sf f (IRBS irb1) = wrap $ do
    t1 <- irb1
    s1 <- Layer.read @CodeSpan t1
    let s1' = CodeSpan.dropOffset s1
    -- The new IR becomes a new parent, so it handles the left offset.
    Layer.write @CodeSpan t1 s1'
    unwrap . irbsFromSpan (sf s1) $ f t1
{-# INLINE inheritCodeSpan1With #-}

inheritCodeSpan2With :: (CodeSpan -> CodeSpan -> CodeSpan)
                     -> (     SomeTerm ->      SomeTerm -> IRB  SomeTerm)
                     -> (IRBS SomeTerm -> IRBS SomeTerm -> IRBS SomeTerm)
inheritCodeSpan2With sf f (IRBS irb1) (IRBS irb2) = wrap $ do
    t1 <- irb1
    s1 <- Layer.read @CodeSpan t1
    -- The new IR becomes a new parent, so it handles the left offset.
    let s1' = CodeSpan.dropOffset s1
    Layer.write @CodeSpan t1 s1'
    t2 <- irb2
    s2 <- Layer.read @CodeSpan t2
    unwrap . irbsFromSpan (sf s1 s2) $ f t1 t2
{-# INLINE inheritCodeSpan2With #-}

-- | Magic helper. Use with care only if you really know what you do.
unsafeModifyCodeSpan :: (CodeSpan -> CodeSpan) -> IRBS SomeTerm -> IRBS SomeTerm
unsafeModifyCodeSpan f (IRBS irb1) = wrap $ do
    t1 <- irb1
    s1 <- Layer.read @CodeSpan t1
    Layer.write @CodeSpan t1 (f s1)
    pure t1
{-# INLINE unsafeModifyCodeSpan #-}


-- === IRBS construction === --

irbsFromSpan :: CodeSpan -> IRB (Term a) -> IRBS (Term a)
irbsFromSpan = IRBS .: attachCodeSpanLayer ; {-# INLINE irbsFromSpan #-}

irbs   ::                    Parser       (IRB (Term a))   -> Parser       (IRBS (Term a))
irbsF  :: Functor f       => Parser (f    (IRB (Term a)))  -> Parser (f    (IRBS (Term a)))
irbsF2 :: Functors '[f,g] => Parser (f (g (IRB (Term a)))) -> Parser (f (g (IRBS (Term a))))
irbs   p = uncurry          irbsFromSpan  <$> spanned p ; {-# INLINE irbs   #-}
irbsF  p = uncurry (fmap  . irbsFromSpan) <$> spanned p ; {-# INLINE irbsF  #-}
irbsF2 p = uncurry (fmap2 . irbsFromSpan) <$> spanned p ; {-# INLINE irbsF2 #-}



--------------------
-- === Errors === --
--------------------

invalid :: Invalid.Symbol -> IRB SomeTerm
invalid t = do
    inv <- IR.invalid' t
    Attr.registerInvalid inv
    pure $ Layout.relayout inv
{-# INLINE invalid #-}

invalidToken :: Parser (IRBS SomeTerm)
invalidToken = irbs $ invalid <$> satisfTest Lexer.matchInvalidSymbol ; {-# INLINE invalidToken #-}

-- invalidSymbol :: (Lexer.Symbol -> Text32) -> Parser (IRBS SomeTerm)
-- invalidSymbol f = irbs $ invalid . f <$> anySymbol

-- catchParseErrors :: Parser a -> Parser (Either String a)
-- catchParseErrors p = withRecovery2 (pure . Left . parseErrorTextPretty)
--                                    (Right <$> p)

-- catchInvalidWith :: HasCallStack => (Span.LeftSpacedSpan -> Span.LeftSpacedSpan)
--                  -> (IRBS SomeTerm -> a) -> Parser a -> Parser a
-- catchInvalidWith spanf f p = undefined -- do
--     -- (span, result) <- spanned $ catchParseErrors p
--     -- pure $ flip fromRight result $ f . irbsFromSpan (spanf span) . invalid . convert



---------------------
-- === Markers === --
---------------------

markerIRB :: Parser (Marker.ID, IRBS SomeTerm)
markerIRB = Marker.getAndClearLast >>= \case
    Nothing -> expected "marker"
    Just t  -> do
        crange <- unwrap <$> State.get @CodeSpanRange
        foEnd  <- unwrap <$> State.get @FileOffset
        let markerLen  = t ^. Lexer.span
            markerOffR = t ^. Lexer.offset
            markerOffL = foEnd - crange - markerLen - markerOffR
            markerSpan = Span.leftSpacedSpan markerOffL markerLen
        State.modify_ @CodeSpanRange $ wrapped .~ foEnd
        pure $ ( t ^. Lexer.element
               , irbsFromSpan (CodeSpan.mkPhantomSpan markerSpan)
                                  (id $ IR.marker' $ t ^. Lexer.element)
               )

marked :: Parser (IRBS SomeTerm -> IRBS SomeTerm)
marked = option registerUnmarkedExpr $ uncurry markedExpr <$> markerIRB where
    markedExpr mid expr = registerMarkedExpr mid . inheritCodeSpan2 (IR.marked')
                          expr

registerUnmarkedExpr ::             IRBS SomeTerm -> IRBS SomeTerm
registerMarkedExpr   :: Marker.ID -> IRBS SomeTerm -> IRBS SomeTerm
registerUnmarkedExpr = wrapped %~ (>>~ Marker.registerOrphan) ; {-# INLINE registerUnmarkedExpr #-}
registerMarkedExpr m = wrapped %~ (>>~ Marker.register m) ; {-# INLINE registerMarkedExpr   #-}



---------------------
-- === Symbols === --
---------------------

stx, etx, eol :: Parser ()
stx = symbol Lexer.STX ; {-# INLINE stx #-}
etx = symbol Lexer.ETX ; {-# INLINE etx #-}
eol = symbol Lexer.EOL ; {-# INLINE eol #-}

braceBegin, braceEnd :: Parser ()
braceBegin = symbol $ Lexer.List Lexer.Begin ; {-# INLINE braceBegin #-}
braceEnd   = symbol $ Lexer.List Lexer.End   ; {-# INLINE braceEnd   #-}

groupBegin, groupEnd :: Parser ()
groupBegin = symbol $ Lexer.Group Lexer.Begin ; {-# INLINE groupBegin #-}
groupEnd   = symbol $ Lexer.Group Lexer.End   ; {-# INLINE groupEnd   #-}

parensed :: Parser a -> Parser a
parensed p = groupBegin *> p <* groupEnd ; {-# INLINE parensed #-}



-------------------------
-- === Identifiers === --
-------------------------

cons, var, op, wildcard :: Parser (IRBS SomeTerm)
cons     = snd <$> namedCons                             ; {-# INLINE cons     #-}
var      = snd <$> namedVar                              ; {-# INLINE var      #-}
op       = snd <$> namedOp                               ; {-# INLINE op       #-}
wildcard = irbs $ IR.blank' <$ symbol Lexer.Wildcard ; {-# INLINE wildcard #-}

namedCons, namedVar, namedOp, namedIdent :: Parser (Name, IRBS SomeTerm)
namedCons  = irbsNamed (flip IR.cons' []) consName ; {-# INLINE namedCons  #-}
namedVar   = irbsNamed IR.var'            varName  ; {-# INLINE namedVar   #-}
namedOp    = irbsNamed IR.var'            opName   ; {-# INLINE namedOp    #-}
namedIdent = namedVar <|> namedCons <|> namedOp    ; {-# INLINE namedIdent #-}

consName, varName, opName, identName, modifierName :: Parser Name
foreignLangName                                    :: Parser Name
consName        = convert <$> satisfTest Lexer.matchCons     ; {-# INLINE consName        #-}
varName         = convert <$> satisfTest Lexer.matchVar      ; {-# INLINE varName         #-}
opName          = convert <$> satisfTest Lexer.matchOperator ; {-# INLINE opName          #-}
identName       = varName <|> consName <|> opName            ; {-# INLINE identName       #-}
funcName        = varName <|> opName                         ; {-# INLINE funcName        #-}
modifierName    = convert <$> satisfTest Lexer.matchModifier ; {-# INLINE modifierName    #-}
foreignLangName = consName                                   ; {-# INLINE foreignLangName #-}

previewVarName :: Parser Name
previewVarName = do
    s <- previewNextSymbol
    maybe (unexpected . fromString $ "Expecting variable, got: " <> show s)
          (pure . convert) $ Lexer.matchVar =<< s
{-# INLINE previewVarName #-}

matchVar, matchCons, matchOp :: Text32 -> Parser ()
matchVar  = symbol . Lexer.Var      ; {-# INLINE matchVar  #-}
matchCons = symbol . Lexer.Cons     ; {-# INLINE matchCons #-}
matchOp   = symbol . Lexer.Operator ; {-# INLINE matchOp   #-}


-- === Helpers === --

irbsNamed :: (Name -> IRB (Term a)) -> Parser Name -> Parser (Name, IRBS (Term a))
irbsNamed cons = irbsF . fmap (\name -> (name, cons name)) ; {-# INLINE irbsNamed #-}


-- Qualified names

qualVarName, qualConsName :: Parser [Name]
qualConsName = qualNameBase consName ; {-# INLINE qualConsName #-}
qualVarName  = qualNameBase varName  ; {-# INLINE qualVarName  #-}

qualNameBase :: Parser Name -> Parser [Name]
qualNameBase p = (\a b -> a <> [b])
             <$> many (try $ consName <* symbol Lexer.Accessor) <*> p
{-# INLINE qualNameBase #-}



-- TODO: Review Literals
----------------------
-- === Literals === --
----------------------

rawQuoteBegin, rawQuoteEnd :: Parser ()
fmtQuoteBegin, fmtQuoteEnd :: Parser ()
rawQuoteBegin = symbol $ Lexer.Quote Lexer.RawStr Lexer.Begin ; {-# INLINE rawQuoteBegin #-}
rawQuoteEnd   = symbol $ Lexer.Quote Lexer.RawStr Lexer.End   ; {-# INLINE rawQuoteEnd   #-}
fmtQuoteBegin = symbol $ Lexer.Quote Lexer.FmtStr Lexer.Begin ; {-# INLINE fmtQuoteBegin #-}
fmtQuoteEnd   = symbol $ Lexer.Quote Lexer.FmtStr Lexer.End   ; {-# INLINE fmtQuoteEnd   #-}

number :: Parser (IRBS SomeTerm)
number = irbs $ do
    Lexer.NumRep base i f <- satisfTest Lexer.matchNumber
    pure $ IR.number' base (convert i) (convert f)


list :: Parser (IRBS SomeTerm) -> Parser (IRBS SomeTerm)
list p = irbs $ Reserved.withoutLocal sep $ braced $ (\g -> liftIRBS1 IR.list' $ sequence g) <$> elems where
    missing :: Parser (IRBS SomeTerm)
    missing   = irbs . pure $ IR.missing'
    sep       = Lexer.Operator ","
    elem      = Reserved.withLocal sep p
    optElem   = elem <|> missing
    bodyH     = (:) <$> elem    <*> many       (separator *> optElem)
    bodyT     = (:) <$> missing <*> someAsList (separator *> optElem)
    elems     = option mempty $ bodyH <|> bodyT
    braced p  = braceBegin *> p <* braceEnd
    separator = symbol sep

-- FIXME[WD]: This `try` should be refactored out
-- FIXME[WD]: Tuple and List parsers are too similar no to be refactored to common part. However tuples will disappear soon.
tuple :: Parser (IRBS SomeTerm) -> Parser (IRBS SomeTerm)
tuple p = try $ irbs $ Reserved.withoutLocal sep $ parensed
        $ (liftIRBS1 IR.tuple' . sequence) <$> elems where
    missing :: Parser (IRBS SomeTerm)
    missing    = irbs $ pure IR.missing'
    sep        = Lexer.Operator ","
    elem       = Reserved.withLocal sep p
    optElem    = elem <|> missing
    body       = (:) <$> optElem <*> someAsList (separator *> optElem)
    elems      = option mempty body
    parensed p = groupBegin *> p <* groupEnd
    separator  = symbol sep

string :: Parser (IRBS SomeTerm)
string = rawStr <|> fmtStr

rawStr :: Parser (IRBS SomeTerm)
rawStr = irbs $ do
    rawQuoteBegin
    (IR.string' . convertVia @String) <$> Indent.withCurrent (strBody rawQuoteEnd) -- FIXME[WD]: We're converting Text -> String here.
    -- withRecovery (\e -> invalid "Invalid string literal" <$ Loc.unregisteredDropSymbolsUntil' (== (Lexer.Quote Lexer.RawStr Lexer.End)))
    --              $ (IR.string' . convertVia @String)
    --            <$> Indent.withCurrent (strBody rawQuoteEnd) -- FIXME[WD]: We're converting Text -> String here.

fmtStr :: Parser (IRBS SomeTerm)
fmtStr = irbs $ do
    fmtQuoteBegin
    (IR.string' . convertVia @String) <$> Indent.withCurrent (strBody fmtQuoteEnd) -- FIXME[WD]: We're converting Text -> String here.
    -- withRecovery (\e -> invalid "Invalid string literal" <$ Loc.unregisteredDropSymbolsUntil' (== (Lexer.Quote Lexer.FmtStr Lexer.End)))
    --              $ (IR.string' . convertVia @String)
    --            <$> Indent.withCurrent (strBody fmtQuoteEnd) -- FIXME[WD]: We're converting Text -> String here.

strBody :: Parser () -> Parser Text32
strBody ending = segStr <|> end <|> nl where
    segStr = (<>) <$> strContent <*> strBody ending
    end    = mempty <$ ending
    nl     = Text32.cons '\n' <$ eol <*> (line <|> nl)
    line   = do Indent.indentedOrEq
                (<>) . convert . flip replicate ' ' <$> Indent.indentation <*> strBody ending

strContent :: Parser Text32
strContent = satisfTest Lexer.matchStr <|> strEsc

strEsc :: Parser Text32
strEsc = satisfTest Lexer.matchStrEsc >>= pure . \case
    Lexer.CharStrEsc i             -> convert $ Char.chr i
    Lexer.NumStrEsc i              -> convert $ Char.chr i
    Lexer.SlashEsc                 -> "\\"
    Lexer.QuoteEscape Lexer.RawStr -> "\""
    Lexer.QuoteEscape Lexer.FmtStr -> "'"



-------------------------
-- === Expressions === --
-------------------------

-- === Utils === --

app, seq, unify, appFlipped, sectionLeft, sectionRight
    :: IRBS SomeTerm -> IRBS SomeTerm -> IRBS SomeTerm
app          = inheritCodeSpan2 IR.app'                        ; {-# INLINE app          #-}
seq          = inheritCodeSpan2 IR.seq'                        ; {-# INLINE seq          #-}
unify        = inheritCodeSpan2 IR.unify'                      ; {-# INLINE unify        #-}
sectionLeft  = inheritCodeSpan2 IR.sectionLeft'                ; {-# INLINE sectionLeft  #-}
appFlipped   = flip . inheritCodeSpan2 $ flip IR.app'          ; {-# INLINE appFlipped   #-}
sectionRight = flip . inheritCodeSpan2 $ flip IR.sectionRight' ; {-# INLINE sectionRight #-}

appSides :: IRBS SomeTerm -> IRBS SomeTerm -> IRBS SomeTerm -> IRBS SomeTerm
appSides = app .: appFlipped ; {-# INLINE appSides #-}

apps, seqs :: IRBS SomeTerm -> [IRBS SomeTerm] -> IRBS SomeTerm
apps = foldl' app ; {-# INLINE apps #-}
seqs = foldl' seq ; {-# INLINE seqs #-}

grouped :: Parser (IRBS SomeTerm) -> Parser (IRBS SomeTerm)
grouped = irbs . parensed . fmap (liftIRBS1 IR.grouped') ; {-# INLINE grouped #-}


-- === Metadata === --

-- FIXME: performance
metadata :: Parser (IRBS SomeTerm)
metadata = irbs $ IR.metadata' . convertVia @String <$> metaContent ; {-# INLINE metadata #-}

metaContent :: Parser Text32
metaContent = satisfTest Lexer.matchMetadata ; {-# INLINE metaContent #-}


-- === Disabled === --

possiblyDisabled :: Parser (IRBS SomeTerm) -> Parser (IRBS SomeTerm)
possiblyDisabled p = disabled p <|> p ; {-# INLINE possiblyDisabled #-}

disabled :: Parser (IRBS SomeTerm) -> Parser (IRBS SomeTerm)
disabled p = irbs $ liftIRBS1 IR.disabled' <$ symbol Lexer.Disable <*> p ; {-# INLINE disabled #-}


-- === Segments === --

type IsFirst = Bool
type IsLast  = Bool

type    ExprSegment        = ExprToken (IRBS SomeTerm)
type    ExprSegments       = NonEmpty ExprSegment
type    ExprSegmentBuilder = SegmentBuilder ExprSegment
newtype SegmentBuilder a   = SegmentBuilder
    { runSegmentBuilder :: IsFirst -> IsLast -> NonEmpty a
    } deriving (Functor)

type ExprToken       a = Expr.Token (ExprSymbol      a)
type ExprTokenProto  a = Expr.Token (ExprSymbolProto a)
type ExprSymbol      a = Labeled SpacedName (SomeSymbol Symbol.Expr    a)
type ExprSymbolProto a = Labeled SpacedName (SomeSymbol Symbol.Phantom a)

instance Semigroup ExprSegmentBuilder where
    a <> b = SegmentBuilder $ \l r -> runSegmentBuilder a l False
                                   <> runSegmentBuilder b False r
    {-# INLINE (<>) #-}

expr :: Parser (IRBS SomeTerm)
expr = possiblyDocumented $ func <|> lineExpr ; {-# INLINE expr #-}

lineExpr :: Parser (IRBS SomeTerm)
lineExpr = marked <*> possiblyDisabled (valExpr <**> option id assignment) where
    assignment = flip unify <$ symbol Lexer.Assignment <*> valExpr
{-# INLINE lineExpr #-}

valExpr :: Parser (IRBS SomeTerm)
valExpr = buildTokenExpr =<< exprSegments ; {-# INLINE valExpr #-}

nonemptyValExpr :: Parser (IRBS SomeTerm)
nonemptyValExpr = do
    es <- exprSegments
    -- when (null es) $ unexpected "Empty expression" -- FIXME[WD]
    buildTokenExpr es

nonemptyValExprLocal :: Parser (IRBS SomeTerm)
nonemptyValExprLocal = do
    es <- exprSegmentsLocal
    -- when (null es) $ unexpected "Empty expression" -- FIXME[WD]
    buildTokenExpr es

nonSpacedValExpr :: Parser (IRBS SomeTerm)
nonSpacedValExpr = do
    es <- exprSegmentsNonSpaced
    -- when (null es) $ unexpected "Empty expression" -- FIXME[WD]
    buildTokenExpr es

nonSpacedPattern :: Parser (IRBS SomeTerm)
nonSpacedPattern = nonSpacedValExpr

concatExprSegmentBuilders :: NonEmpty ExprSegmentBuilder -> ExprSegmentBuilder
concatExprSegmentBuilders bldrs = SegmentBuilder $ \l r -> case bldrs of
    (s:|[])     -> runSegmentBuilder s l r
    (s:|(t:ts)) -> runSegmentBuilder s l False
                <> runSegmentBuilder (concatExprSegmentBuilders $ t:|ts) False r

exprSegments    , exprSegmentsLocal     :: Parser ExprSegments
exprFreeSegments, exprFreeSegmentsLocal :: Parser ExprSegmentBuilder
exprSegments          = buildExprTok <$> exprFreeSegments
exprSegmentsLocal     = buildExprTok <$> exprFreeSegmentsLocal
exprFreeSegments      = Reserved.withNewLocal exprFreeSegmentsLocal
exprFreeSegmentsLocal = fmap concatExprSegmentBuilders . some
                      $ choice [ mfixVarSeg, consSeg, wildSeg, numSeg, strSeg
                               , opSeg, accSeg, tupleSeg, grpSeg, listSeg
                               , lamSeg, matchseg, typedSeg, funcSeg, invalidSeg
                               ]

exprSegmentsNonSpaced    , exprSegmentsNonSpacedLocal     :: Parser ExprSegments
exprFreeSegmentsNonSpaced, exprFreeSegmentsNonSpacedLocal :: Parser ExprSegmentBuilder
exprSegmentsNonSpaced          = buildExprTok <$> exprFreeSegmentsNonSpaced
exprSegmentsNonSpacedLocal     = buildExprTok <$> exprFreeSegmentsNonSpacedLocal
exprFreeSegmentsNonSpaced      = Reserved.withNewLocal exprFreeSegmentsNonSpacedLocal
exprFreeSegmentsNonSpacedLocal = choice [ varSeg, consSeg, wildSeg, numSeg
                                        , strSeg, tupleSeg, grpSeg, listSeg
                                        , invalidSeg
                                        ]


-- === Components === --

-- Utils
posIndependent :: a -> SegmentBuilder (Expr.Token a)
posIndependent = SegmentBuilder . P.curry . const . pure . Expr.tokenx ; {-# INLINE posIndependent #-}

-- FIXME[WD]: change the API to monadic one, so we can register symbols and mixfix monads could gather needed info (like var names)
--            without the need to keep the label in the term
unlabeledAtom :: a -> Labeled SpacedName (SomeSymbol Symbol.Expr a)
unlabeledAtom = labeled (Name.spaced "#unnamed#") . Symbol.atom

-- -- Possible tokens
consSeg, funcSeg, grpSeg, invalidSeg, lamSeg, matchseg, numSeg, strSeg,
    tupleSeg, varSeg, wildSeg :: Parser ExprSegmentBuilder
consSeg    = posIndependent . unlabeledAtom <$> cons
funcSeg    = posIndependent . unlabeledAtom <$> func
grpSeg     = posIndependent . unlabeledAtom <$> grouped nonemptyValExpr
invalidSeg = posIndependent . unlabeledAtom <$> invalidToken
lamSeg     = posIndependent . labeled (Name.unspaced SpecialName.lam) . Symbol.suffix <$> lamBldr
listSeg    = posIndependent . unlabeledAtom <$> list  nonemptyValExprLocal
matchseg   = posIndependent . unlabeledAtom <$> match
numSeg     = posIndependent . unlabeledAtom <$> number
strSeg     = posIndependent . unlabeledAtom <$> string
tupleSeg   = posIndependent . unlabeledAtom <$> tuple nonemptyValExprLocal
varSeg     = posIndependent . unlabeledAtom <$> var
wildSeg    = posIndependent . unlabeledAtom <$> wildcard


mfixVarSeg :: Parser ExprSegmentBuilder
mfixVarSeg  = do
    (span, name) <- spanned varName
    nameSet      <- Scope.lookupMultipartNames name
    let cvar = posIndependent $ unlabeledAtom $ irbsFromSpan span (IR.var' name)
    if TreeSet.null nameSet
        then pure cvar
        else -- catchInvalidWith (span <>) (pure . posIndependent . unlabeledAtom)
           {-$-} Reserved.withMany (Lexer.Var . convertVia @String <$> TreeSet.keys nameSet) -- FIXME: conversion Name -> Text
           $ do segmentToks <- exprFreeSegmentsLocal
                namedSegs   <- option mempty (parseMixfixSegments nameSet)
                if null namedSegs then pure (cvar <> segmentToks) else do
                    segment <- buildTokenExpr (buildExprTok segmentToks)
                    let segments  = snd <$> namedSegs
                        nameParts = fst <$> namedSegs
                        mfixVar   = irbsFromSpan span
                                  . IR.var' . Name.mixfix $ name :| nameParts
                        mfixExpr  = apps (app mfixVar segment) segments
                    pure . posIndependent . unlabeledAtom $ mfixExpr

opSeg :: Parser ExprSegmentBuilder
opSeg   = do
    (before, after) <- checkOffsets
    (span, name)    <- spanned opName
    let segment isFirst isLast = pure . Expr.tokenx $ operator & if
            | isSingle    -> labeled (Name.unspaced            name) . Symbol.atom
            | isUMinus    -> labeled (Name.unspaced SpecialName.uminus) . Symbol.prefix . app
            | isFirst     -> labeled (Name.spacedIf after  name) . Symbol.prefix . sectionLeft
            | isLast      -> labeled (Name.spacedIf before name) . Symbol.suffix . sectionRight
            | symmetrical -> labeled (Name.spacedIf after  name) . Symbol.infixx . appSides
            | before      -> labeled (Name.lspaced         name) . Symbol.prefix . sectionLeft
            | otherwise   -> labeled (Name.rspaced         name) . Symbol.suffix . sectionRight
            where isMinus     = name == SpecialName.minus
                  isSingle    = isFirst && isLast
                  isUMinus    = isMinus && not after && (isFirst || before) && not isLast
                  operator    = irbsFromSpan span . IR.var' $ if isUMinus then SpecialName.uminus else name
                  symmetrical = before == after
    pure $ SegmentBuilder segment

typedSeg :: Parser ExprSegmentBuilder
typedSeg   = do
    (off, off') <- checkOffsets
    let typedExpr = if off then valExpr else nonSpacedValExpr
    tp <- symbol Lexer.Typed *> typedExpr
    when_ (off /= off') $ error "TODO: before and after offsets have to match"
    let seg :: IRBS SomeTerm -> IRBS SomeTerm
        seg = flip (inheritCodeSpan2 IR.typed') tp
    pure . posIndependent . labeled (Name.spacedIf off SpecialName.typed) $ Symbol.suffix seg

accSectNames :: Parser (NonEmpty (CodeSpan, Name))
accSectNames = do
    sname <- spanned $ symbol Lexer.Accessor *> identName
    (beforeDot, afterDot) <- checkOffsets
    (sname :|) <$> if beforeDot || afterDot then pure   mempty
                                            else option mempty (convert <$> accSectNames)


accSeg :: Parser ExprSegmentBuilder
accSeg = fmap2 Expr.tokenx $ do
    (beforeDot, afterDot) <- checkOffsets
    snames@(n :| ns) <- accSectNames
    mupdt <- option Nothing (Just <$ symbol Lexer.Assignment <*> nonemptyValExpr)
    mmod  <- option Nothing (Just .: (,) <$> modifierName <*> nonemptyValExpr)
    let (spans, (names :: [Name])) = unzip $ convert snames
        symmetrical    = beforeDot == afterDot
        accCons s n    = inheritCodeSpan1With (<> s) (flip IR.acc' n)
        accSect        = labeled uname . Symbol.atom
                       $ irbsFromSpan (mconcat spans)
                       $ IR.accSection' (convert names)
        uname          = Name.unspaced SpecialName.acc
        sname          = Name.spacedIf beforeDot SpecialName.acc
        accConss       =  labeled sname ( Symbol.suffix $ uncurry accCons n )
                      :| (labeled uname . Symbol.suffix . uncurry accCons <$> ns)
        Just fupdt           = mupdt -- FIXME[WD]: make it nicer
        Just (modName, fmod) = mmod  -- FIXME[WD]: make it nicer
        path = convert names

        -- FIXME[WD]: make it nicer vvv
        updateAtom = labeled sname . Symbol.suffix
                   $ flip (inheritCodeSpan2With (<>) (flip IR.update' path))
                          (unsafeModifyCodeSpan (CodeSpan.asOffsetSpan (mconcat spans) <>) fupdt)
        modifyAtom = labeled sname . Symbol.suffix
                   $ flip (inheritCodeSpan2With (<>) (flip (flip IR.modify' path) modName))
                          (unsafeModifyCodeSpan (CodeSpan.asOffsetSpan (mconcat spans) <>) fmod)


    -- INFO: The following code contains hack allowing for fields updates
    --       and modifications that are not nested e.g. `a' = a.x = 5`, but
    --       does not support `a' = a.foo.x = 5`. If we use the above definition
    --       instead all nested use cases will be supported in parser, but
    --       need to be supported in backend too. Using this hack, the above
    --       definition is translated in parser to `a' = a.x= 5`,
    --       where `x=` is method name!
    -- vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

    let (nameSpan, name) = n
        updateAtomHack   = pure $ labeled sname ( Symbol.suffix $ \x -> app (accCons nameSpan (name <> "=") x) fupdt )
        modifyAtomHack   = pure $ labeled sname ( Symbol.suffix $ \x -> app (accCons nameSpan (name <> modName <> "=") x) fmod )

    when_ (isJust mupdt && not (null ns)) $ fail "Updating nested fields is not supported yet."
    when_ (isJust mmod  && not (null ns)) $ fail "Modification of nested fields is not supported yet."

    -- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    let segment isFirst isLast = if
            | isFirst     -> pure accSect
             -- FIXME[WD]: make it nicer vvv
            | symmetrical -> if
                | isJust mupdt -> if True == True then updateAtomHack else pure updateAtom -- hack for typechecker (see the above description)
                | isJust mmod  -> if True == True then modifyAtomHack else pure modifyAtom -- hack for typechecker (see the above description)
                | otherwise    -> accConss
            | beforeDot   -> pure accSect
            | otherwise   -> error "unsupported" -- FIXME[WD]: make nice error here
    pure $ SegmentBuilder segment

match :: Parser (IRBS SomeTerm)
match = irbs $ (\n (p,ps) -> liftIRBS2 IR.match' n (sequence $ p:ps))
      <$ symbol Lexer.KwCase <*> nonemptyValExprLocal
      <* symbol Lexer.KwOf   <*> discover (nonEmptyBlock clause)
      where clause = pat <**> lamBldr

pat :: Parser (IRBS SomeTerm)
pat = Reserved.withLocal Lexer.BlockStart nonemptyValExprLocal


lamBldr :: Parser (IRBS SomeTerm -> IRBS SomeTerm)
lamBldr = do
    body <- symbol Lexer.BlockStart *> discover (nonEmptyBlock lineExpr)
    pure $ flip (inheritCodeSpan2 IR.lam') (uncurry seqs body)


-- === Utils === --

buildExprTok :: ExprSegmentBuilder -> ExprSegments
buildExprTok bldr = runSegmentBuilder bldr True True

parseMixfixSegments :: SparseTreeSet Name -> Parser [(Name, IRBS SomeTerm)]
parseMixfixSegments nameSet = do
    name <- previewVarName
    let total = TreeSet.check' name nameSet
    case nameSet ^? ix name of
        Nothing       -> unexpected . fromString $ "Identifier `" <> convert name <> "` is not part of a mutli-name expression"
        Just nameSet' -> do
            let possiblePaths = TreeSet.keys nameSet'
            anySymbol_
            segment <- Reserved.withMany (Lexer.Var . convertVia @String <$> possiblePaths) nonemptyValExpr -- FIXME: conversion Name -> Text
            let restMod = if total then option mempty else (<|> unexpected (fromString $ "Unexpected end of mixfix expression, expecting one of " <> show possiblePaths))
            ((name,segment):) <$> restMod (parseMixfixSegments nameSet')

buildTokenExpr (s:|ss) = buildTokenExpr' (s:ss)

buildTokenExpr' :: Expr.Tokens (Labeled SpacedName (SomeSymbol Symbol.Expr (IRBS SomeTerm))) -> Parser (IRBS SomeTerm)
buildTokenExpr' = Expr.buildExpr_termApp . Labeled (Name.spaced SpecialName.app) $ Symbol.Symbol app


---------------------------
-- === Documentation === --
---------------------------

possiblyDocumented :: Parser (IRBS SomeTerm) -> Parser (IRBS SomeTerm)
possiblyDocumented p = documented p <|> p

-- FIXME: performance
documented :: Parser (IRBS SomeTerm) -> Parser (IRBS SomeTerm)
documented p = irbs $ (\d t -> liftIRBS1 (IR.documented' $ convertVia @String d) t) <$> doc <*> p

doc :: Parser Text32
doc = intercalate "\n" <$> some (satisfTest Lexer.matchDocComment <* eol)


--------------------------
-- === Declarations === --
--------------------------

topLvlDecl :: Parser (IRBS SomeTerm)
topLvlDecl = possiblyDocumented $ func <|> record <|> metadata

-- === Functions === --

func :: Parser (IRBS SomeTerm)
func = irbs $ funcHdr <**> (funcSig <|> funcDef) where
    funcDef, funcSig :: Parser (IRBS SomeTerm -> IRB SomeTerm)
    funcHdr     = symbol Lexer.KwDef *> withRecovery headerRec (var <|> op)
    headerRec e = irbs $ invalid Invalid.FunctionHeader
               <$ Loc.unregisteredDropSymbolsUntil
                  (`elem` [Lexer.BlockStart, Lexer.EOL, Lexer.ETX])
    funcSig     = (\tp name -> liftIRBS2 IR.functionSig' name tp)
               <$ symbol Lexer.Typed <*> valExpr
    funcDef     = (\args body name -> liftIRBS3 IR.function' name (sequence args) (uncurry seqs body))
              <$> many nonSpacedPattern
              <*> withRecovery blockRec block
    block       = symbol Lexer.BlockStart
               *> discover (nonEmptyBlock lineExpr)
    -- blockRec :: Int -> Parser ((IRBS SomeTerm),[IRBS SomeTerm])
    blockRec e  = (,[]) <$> (irbs $ invalid Invalid.FunctionBlock
               <$ optionalBlockAny)

            -- withRecovery (\e -> invalid "Invalid string literal" <$ Loc.unregisteredDropSymbolsUntil' (== (Lexer.Quote Lexer.RawStr Lexer.End)))
    --              $ (IR.string' . convertVia @String)
    --            <$> Indent.withCurrent (strBody rawQuoteEnd) -- FIXME[WD]: We're converting Text -> String here.


-- -- === Classes == --

record :: Parser (IRBS SomeTerm)
record = irbs $ (\nat n args (cs,ds) -> liftIRBS3 (IR.record' nat n) (sequence args) (sequence cs) (sequence ds))
   <$> try (option False (True <$ symbol Lexer.KwNative) <* symbol Lexer.KwClass) <*> consName <*> many var <*> body
    where body      = option mempty $ symbol Lexer.BlockStart *> bodyBlock
          funcBlock = optionalBlockBody (possiblyDocumented func)
          consBlock = breakableNonEmptyBlockBody' recordCons <|> breakableOptionalBlockBody recordNamedFields
          bodyBlock = discoverIndent ((,) <$> consBlock <*> funcBlock)

recordCons :: Parser (IRBS SomeTerm)
recordCons = irbs $ (\n fields -> liftIRBS1 (IR.recordCons' n) (sequence fields)) <$> consName <*> (blockDecl <|> inlineDecl) where
    blockDecl  = symbol Lexer.BlockStart *> discover (nonEmptyBlock' recordNamedFields)
    inlineDecl = many unnamedField

recordNamedFields :: Parser (IRBS SomeTerm)
recordNamedFields = irbs $ liftIRBS1 . IR.recordFields' . convert
                <$> many varName <* symbol Lexer.Typed <*> valExpr

unnamedField :: Parser (IRBS SomeTerm)
unnamedField = irbs $ liftIRBS1 (IR.recordFields' mempty)
                      <$> nonSpacedValExpr



--------------------
-- === Units === --
--------------------

-- === Imports === --

impHub :: Parser (IRBS SomeTerm)
impHub = irbs $ (\(imps :: [IRBS SomeTerm])
                 -> liftIRBS1 IR.importHub'
                    (sequence imps :: IRBS [SomeTerm])) <$> impHeader

impHeader :: Parser [IRBS SomeTerm]
impHeader = option mempty $ breakableOptionalBlockTop imp -- <* skipEmptyLines

imp :: Parser (IRBS SomeTerm)
imp = irbs $ (\a tgts -> liftIRBS1 (flip IR.imp' tgts) a)
   <$ symbol Lexer.KwImport <*> importSource <*> impTgt where
    impTgt  = option Import.Everything $ symbol Lexer.BlockStart *> listTgt
    listTgt = Import.Listed . convert <$> many (varName <|> consName)

importSource :: Parser (IRBS SomeTerm)
importSource = irbs . fmap IR.importSource' $ choice
    [ Import.World              <$  matchCons "World"
    , Import.Relative . convert <$  symbol Lexer.Accessor <*> qualConsName
    , Import.Absolute . convert <$> qualConsName
    ]


-- ------------------------------------
-- -- === Foreign Import Parsing === --+
-- ------------------------------------

foreignImportList :: Parser (IRBS SomeTerm)
foreignImportList = irbs $
    (\lang imports ->
        liftIRBS1 (IR.foreignImport' lang) (sequence imports))
    <$  (symbol Lexer.KwForeign *> symbol Lexer.KwImport)
    <*> foreignLangName
    <*  symbol Lexer.BlockStart
    <*> discover (nonEmptyBlock' foreignLocationImportList)

foreignLocationImportList :: Parser (IRBS SomeTerm)
foreignLocationImportList = irbs $
    (\loc imports ->
        liftIRBS2 IR.foreignImportList' loc (sequence imports))
    <$> stringOrVarName
    <*  symbol Lexer.BlockStart
    <*> discover (nonEmptyBlock' foreignSymbolImport)

foreignSymbolImport :: Parser (IRBS SomeTerm)
foreignSymbolImport = irbs $ withRecovery recover
    $   try (foreignSymbolImportWithSafety defaultFISafety)
    <|> foreignSymbolImportWithSafety specifiedFISafety
    where recover e = invalid Invalid.ForeignImportSafety
                      <$ Loc.unregisteredDropSymbolsUntil
                      (`elem` [Lexer.ETX, Lexer.EOL])

foreignSymbolImportWithSafety :: Parser (IRBS SomeTerm) -> Parser (IRB SomeTerm)
foreignSymbolImportWithSafety safe =
    (\safety forName localName importType ->
        liftIRBS3 (foreignSymbolProxy localName) safety forName importType)
    <$> safe <*> stringOrVarName <*> funcName <*  symbol Lexer.Typed <*> valExpr
    where foreignSymbolProxy a b c d = IR.foreignImportSymbol' b c a d

defaultFISafety :: Parser (IRBS SomeTerm)
defaultFISafety = irbs $
    (\safety -> id (IR.foreignImportSafety' safety))
    <$> ((pure Import.Default) :: Parser IR.ForeignImportType)

-- TODO [Ara, WD] Need to have the lexer deal with these as contextual keywords
-- using a positive lookahead in the _Lexer_.
specifiedFISafety :: Parser (IRBS SomeTerm)
specifiedFISafety = irbs $
    (\(importSafety :: IR.ForeignImportType) ->
        id (IR.foreignImportSafety' importSafety))
    <$> getImpSafety (optionMaybe varName)
    where getImpSafety :: Parser (Maybe Name) -> Parser IR.ForeignImportType
          getImpSafety p = p >>= \case
              Nothing -> pure Import.Default
              Just n  -> case n of
                  "safe"   -> pure Import.Safe
                  "unsafe" -> pure Import.Unsafe
                  _        -> fail "Invalid safety specification."

stringOrVarName :: Parser (IRBS SomeTerm)
stringOrVarName = string <|> (asgNameParser varName)

asgNameParser :: Parser Name -> Parser (IRBS SomeTerm)
asgNameParser nameParser = irbs $ (\varN -> id (IR.var' varN))
     <$> nameParser



-- -- === Unit body === --

unit' :: Parser (IRBS SomeTerm)
unit  :: Parser (IRBS (IR.Term IR.Unit))
unit' = Layout.relayout <<$>> unit
unit  = irbs $
    (\imps cls
        -> Layout.unsafeRelayout <$> liftIRBS2 (flip IR.unit []) imps cls)
        <$ spacing <*> (foreignImportList <|> impHub) <*> unitCls <* spacing
    where spacing = many eol

unitCls :: Parser (IRBS SomeTerm)
unitCls = irbs $ (\ds -> liftIRBS1 (IR.record' False "" [] [])
                     (sequence ds)) <$> optionalBlockTop topLvlDecl



----------------------------
-- === Layout parsing === --
----------------------------

skipEOLs :: Parser ()
skipEOLs = void $ many eol

discover :: Parser a -> Parser a
discover p = skipEOLs >> p

discoverIndent :: Parser a -> Parser a
discoverIndent = discover . Indent.withCurrent

nonEmptyBlock', nonEmptyBlockBody' :: Parser a -> Parser [a]
nonEmptyBlock'     = uncurry (:) <<$>> nonEmptyBlock
nonEmptyBlockBody' = uncurry (:) <<$>> nonEmptyBlockBody

nonEmptyBlock , nonEmptyBlockBody  :: Parser a -> Parser (a, [a])
nonEmptyBlock     p = Indent.indented >> Indent.withCurrent (nonEmptyBlockBody p)
nonEmptyBlockTop    = Indent.withRoot    . nonEmptyBlockBody
nonEmptyBlockBody p = (,) <$> p <*> lines where
    spacing = many eol
    indent  = spacing <* Indent.indentedEq <* notFollowedBy etx
    lines   = many $ try indent *> p

optionalBlock, optionalBlockTop, optionalBlockBody :: Parser a -> Parser [a]
optionalBlock     p = option mempty $ uncurry (:) <$> nonEmptyBlock     p
optionalBlockTop  p = option mempty $ uncurry (:) <$> nonEmptyBlockTop  p
optionalBlockBody p = option mempty $ uncurry (:) <$> nonEmptyBlockBody p

breakableNonEmptyBlock', breakableNonEmptyBlockTop', breakableNonEmptyBlockBody' :: Parser a -> Parser     [a]
breakableNonEmptyBlock , breakableNonEmptyBlockTop , breakableNonEmptyBlockBody  :: Parser a -> Parser (a, [a])
breakableNonEmptyBlock'      = uncurry (:) <<$>> breakableNonEmptyBlock
breakableNonEmptyBlockTop'   = uncurry (:) <<$>> breakableNonEmptyBlockTop
breakableNonEmptyBlockBody'  = uncurry (:) <<$>> breakableNonEmptyBlockBody
breakableNonEmptyBlock       = Indent.withCurrent . breakableNonEmptyBlockBody
breakableNonEmptyBlockTop    = Indent.withRoot    . breakableNonEmptyBlockBody
breakableNonEmptyBlockBody p = (,) <$> lexedp <*> lines where
    spacing = many eol
    lines   = many $ Indent.indentedEq *> lexedp
    lexedp  = p <* spacing

breakableOptionalBlock, breakableOptionalBlockTop, breakableOptionalBlockBody :: Parser a -> Parser [a]
breakableOptionalBlock     p = option mempty $ uncurry (:) <$> breakableNonEmptyBlock     p
breakableOptionalBlockTop  p = option mempty $ uncurry (:) <$> breakableNonEmptyBlockTop  p
breakableOptionalBlockBody p = option mempty $ uncurry (:) <$> breakableNonEmptyBlockBody p


nonEmptyBlockAny :: Parser [Token]
nonEmptyBlockAny = concat . convertTo @[[Token]] <$> some line where
    line = Indent.indented >> (body <* eol)
    body = Loc.getTokensUntil (\t -> (t == Lexer.EOL) || (t == Lexer.ETX))

optionalBlockAny :: Parser [Token]
optionalBlockAny = option mempty nonEmptyBlockAny


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
