{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE UndecidableInstances      #-}

module Luna.Syntax.Text.Parser.Parsing where

import           Prologue hiding (seq, some)
import qualified Prologue

import qualified Control.Monad.State.Layered       as State
import qualified Data.Char                         as Char
import qualified Data.Set                          as Set
import qualified Data.Text.Span                    as Span
import qualified Data.Text32                       as Text32
import qualified Data.TreeSet                      as TreeSet
import qualified Language.Symbol                   as Symbol
import qualified Luna.Data.Name                    as Name
import qualified Luna.IR                           as IR
import qualified Luna.IR.Layer                     as Layer
import qualified Luna.IR.Term.Ast                  as Import
import qualified Luna.Syntax.Text.Lexer            as Lexer
import qualified Luna.Syntax.Text.Lexer.Symbol     as Lexer
import qualified Luna.Syntax.Text.Parser.CodeSpan  as CodeSpan
import qualified Luna.Syntax.Text.Parser.Errors    as Invalid
import qualified Luna.Syntax.Text.Parser.Hardcoded as Builtin
import qualified Luna.Syntax.Text.Parser.Loc       as Loc
import qualified Luna.Syntax.Text.Parser.Marker    as Marker
import qualified Luna.Syntax.Text.Parser.Name      as Name
import qualified Luna.Syntax.Text.Parser.Reserved  as Reserved
import qualified Luna.Syntax.Text.Scope            as Scope
import qualified OCI.Data.Name.Multipart           as Name.Multipart
import qualified OCI.IR.Layout                     as Layout
import qualified Text.Parser.Expr                  as Expr
import qualified Text.Parser.Indent                as Indent

import Data.List.NonEmpty               ((<|))
import Data.Set                         (Set)
import Data.Text.Position               (FileOffset (..))
import Data.Text.Position               (Delta)
import Data.Text32                      (Text32)
import Data.TreeSet                     (SparseTreeSet)
import Language.Symbol                  (Labeled (Labeled), SomeSymbol, labeled)
import Luna.IR                          (SomeTerm, Term)
import Luna.Pass                        (Pass)
import Luna.Syntax.Text.Parser.Class    (Stream)
import Luna.Syntax.Text.Parser.CodeSpan (CodeSpan (CodeSpan),
                                         CodeSpanRange (..))
import Luna.Syntax.Text.Parser.Loc      (LeftSpanner (LeftSpanner),
                                         checkNextOffset, previewNextSymbol,
                                         token', withRecovery2)
import Luna.Syntax.Text.Parser.Marker   (MarkedExprMap, MarkerId, MarkerState,
                                         UnmarkedExprs, addMarkedExpr,
                                         addUnmarkedExpr, getLastTokenMarker,
                                         useLastTokenMarker)
import Luna.Syntax.Text.Parser.Marker   (MarkedExprMap, UnmarkedExprs)
import Luna.Syntax.Text.Parser.Name     (SpacedName)
import Luna.Syntax.Text.Parser.Parser   (IRB, IRBS (IRBS, fromIRBS), IRBSParser,
                                         Parser, SymParser, bindIRBS1,
                                         bindIRBS2, bindIRBS3, runParserT,
                                         withAsgBldr)
import OCI.Data.Name                    (Name)
import Text.Megaparsec                  (ErrorItem (Tokens), MonadParsec,
                                         ParseError, between, choice, hidden,
                                         lookAhead, manyTill, notFollowedBy,
                                         skipMany, try, unexpected,
                                         withRecovery)
import Text.Megaparsec.Char             (anyChar, char, digitChar, letterChar,
                                         lowerChar, spaceChar, upperChar)
import Text.Megaparsec.Error            (parseErrorPretty, parseErrorTextPretty)
import Text.Megaparsec.Ext              (expected)
import Text.Parser.Combinators


-- TODO: Can we do better?
instance Convertible Text32.Text32 Name where
    convert = convertVia @String ; {-# INLINE convert #-}



---------------------
-- === Satisfy === --
---------------------

satisfy    :: (Lexer.Symbol -> Bool)    -> SymParser Lexer.Symbol
satisfy_   :: (Lexer.Symbol -> Bool)    -> SymParser ()
satisfTest :: (Lexer.Symbol -> Maybe a) -> SymParser a
satisfy_   f = void $ satisfy f                  ; {-# INLINE satisfy_ #-}
satisfy    f = satisfTest $ \s -> justIf (f s) s ; {-# INLINE satisfy  #-}
satisfTest f = token' testSymbol Nothing where
    testSymbol r t = case Reserved.lookupSymbolReservation r (t ^. Lexer.element) of
        True  -> Left (Just $ Tokens (pure t), Set.empty)
        False -> satisfyTestSymbol f t
{-# INLINE satisfTest #-}

satisfyUnchecked     :: (Lexer.Symbol -> Bool)    -> SymParser Lexer.Symbol
satisfyUnchecked_    :: (Lexer.Symbol -> Bool)    -> SymParser ()
satisfyUncheckedTest :: (Lexer.Symbol -> Maybe a) -> SymParser a
satisfyUnchecked_    f = void $ satisfyUnchecked f                    ; {-# INLINE satisfyUnchecked_    #-}
satisfyUnchecked     f = satisfyUncheckedTest $ \s -> justIf (f s) s  ; {-# INLINE satisfyUnchecked     #-}
satisfyUncheckedTest f = token' (const $ satisfyTestSymbol f) Nothing ; {-# INLINE satisfyUncheckedTest #-}

satisfyTestSymbol :: (t -> Maybe b) -> Lexer.Token t
                  -> Either (Maybe (ErrorItem (Lexer.Token t)), Set a) b
satisfyTestSymbol f t = note (Just $ Tokens (pure t), Set.empty)
                      $ f (t ^. Lexer.element)
{-# INLINE satisfyTestSymbol #-}

symbol :: Lexer.Symbol -> SymParser ()
symbol = satisfy_ . (==) ; {-# INLINE symbol #-}

anySymbol  :: SymParser Lexer.Symbol
anySymbol_ :: SymParser ()
anySymbol  = satisfyUnchecked $ const True ; {-# INLINE anySymbol #-}
anySymbol_ = void anySymbol                ; {-# INLINE anySymbol_ #-}

getLastOffset   :: State.Getter LeftSpanner m => m Delta
checkLastOffset :: State.Getter LeftSpanner m => m Bool
getLastOffset   = unwrap <$> State.get @LeftSpanner ; {-# INLINE getLastOffset   #-}
checkLastOffset = (>0)   <$> getLastOffset          ; {-# INLINE checkLastOffset #-}

checkOffsets :: (MonadParsec e Stream m, State.Getter LeftSpanner m) => m (Bool, Bool)
checkOffsets = (,) <$> checkLastOffset <*> checkNextOffset ; {-# INLINE checkOffsets #-}



----------------------------------
-- === Code span management === --
----------------------------------

attachCodeSpanLayer :: CodeSpan -> IRB (Term a) -> IRB (Term a)
attachCodeSpanLayer s = (>>~ flip (Layer.write @CodeSpan) s) ; {-# INLINE attachCodeSpanLayer #-}

spanned :: SymParser a -> SymParser (CodeSpan, a)
spanned p = phantomSpan $ do
    fileOffStart <- unwrap <$> State.get @FileOffset
    State.put @CodeSpanRange $ wrap fileOffStart
    p

-- -- | Function `phantomSpan` do not register it's beginning as new element start.
--   It is a very rarely needed functionality, use with care.
phantomSpan :: SymParser a -> SymParser (CodeSpan, a)
phantomSpan p = do
    range   <- unwrap <$> State.get @CodeSpanRange
    fileOffStart <- unwrap <$> State.get @FileOffset
    marker  <- Marker.getLastTokenMarker
    out     <- p
    foEnd   <- unwrap <$> State.get @FileOffset
    sfxOff  <- getLastOffset
    let end       = foEnd   - sfxOff
        off       = fileOffStart - range
        emptySpan = Span.leftSpacedSpan mempty mempty
        (rs,vs)   = (realSpan, viewSpan)
        -- FIXME[WD]: The `foo` and `bar` helpers are here just to make it work with empty spans in list sections / empty module headers.
        --            We should think how to refactor them and describe better where they originate from.
        foo       = max 0 (end - fileOffStart)
        bar       = max end fileOffStart
        realSpan  = Span.leftSpacedSpan off foo
        viewSpan  = case marker of
            Nothing -> realSpan
            Just m  -> Span.leftSpacedSpan
                       (off - m ^. Lexer.span - m ^. Lexer.offset) foo
    State.put @CodeSpanRange $ convert bar
    pure (CodeSpan rs vs, out)

spanOf :: SymParser a -> SymParser a
spanOf = fmap snd . spanned

inheritCodeSpan1 :: (SomeTerm -> IRB SomeTerm) -> IRBS SomeTerm -> IRBS SomeTerm
inheritCodeSpan1 = inheritCodeSpan1With id

inheritCodeSpan2 :: (SomeTerm -> SomeTerm -> IRB SomeTerm) -> IRBS SomeTerm -> IRBS SomeTerm -> IRBS SomeTerm
inheritCodeSpan2 = inheritCodeSpan2With (CodeSpan.concat) -- IT WAS HERE: CodeSpan.concat

inheritCodeSpan1With :: (CodeSpan -> CodeSpan) -> (SomeTerm -> IRB SomeTerm) -> IRBS SomeTerm -> IRBS SomeTerm
inheritCodeSpan1With sf f (IRBS irb1) = wrap $ do
    t1 <- irb1
    s1 <- Layer.read @CodeSpan t1
    Layer.write @CodeSpan t1 (CodeSpan.dropOffset s1) -- the newly constructed IR is the new parent, so it handles the left offset
    unwrap . buildAsgFromSpan (sf s1) $ f t1

inheritCodeSpan2With :: (CodeSpan -> CodeSpan -> CodeSpan)
                     -> (SomeTerm -> SomeTerm -> IRB SomeTerm)
                     -> IRBS SomeTerm -> IRBS SomeTerm -> IRBS SomeTerm
inheritCodeSpan2With sf f (IRBS irb1) (IRBS irb2) = wrap $ do
    t1 <- irb1
    s1 <- Layer.read @CodeSpan t1
    -- | the new IR is the new parent, so it handles the left offset
    Layer.write @CodeSpan t1 (CodeSpan.dropOffset s1)
    t2 <- irb2
    s2 <- Layer.read @CodeSpan t2
    unwrap . buildAsgFromSpan (sf s1 s2) $ f t1 t2

-- | Magic helper. Use with care only if you really know what you do.
modifyCodeSpan :: (CodeSpan -> CodeSpan) -> IRBS SomeTerm -> IRBS SomeTerm
modifyCodeSpan f (IRBS irb1) = wrap $ do
    t1 <- irb1
    s1 <- Layer.read @CodeSpan t1
    Layer.write @CodeSpan t1 (f s1)
    pure t1


-- === ASG preparation === --

buildAsgFromSpan :: CodeSpan -> IRB (Term a) -> IRBS (Term a)
buildAsgFromSpan = IRBS .: attachCodeSpanLayer

buildAsg   ::                    SymParser       (IRB (Term a))   -> SymParser       (IRBS (Term a))
buildAsgF  :: Functor f       => SymParser (f    (IRB (Term a)))  -> SymParser (f    (IRBS (Term a)))
buildAsgF2 :: Functors '[f,g] => SymParser (f (g (IRB (Term a)))) -> SymParser (f (g (IRBS (Term a))))
buildAsg   p = uncurry          buildAsgFromSpan  <$> spanned p
buildAsgF  p = uncurry (fmap  . buildAsgFromSpan) <$> spanned p
buildAsgF2 p = uncurry (fmap2 . buildAsgFromSpan) <$> spanned p



-- --------------------
-- -- === Errors === --
-- --------------------

invalid :: Text32 -> IRB SomeTerm
invalid txt = id $ do
    inv <- IR.invalid' $ convertVia @String txt -- FIXME: performance
    Invalid.register inv
    pure $ Layout.relayout inv

invalidSymbol :: (Lexer.Symbol -> Text32) -> IRBSParser SomeTerm
invalidSymbol f = buildAsg $ invalid . f <$> anySymbol

catchParseErrors :: SymParser a -> SymParser (Either String a)
catchParseErrors p = withRecovery2 (pure . Left . parseErrorTextPretty) (Right <$> p)

catchInvalidWith :: HasCallStack => (Span.LeftSpacedSpan -> Span.LeftSpacedSpan)
                 -> (IRBS SomeTerm -> a) -> SymParser a -> SymParser a
catchInvalidWith spanf f p = undefined -- do
    -- (span, result) <- spanned $ catchParseErrors p
    -- pure $ flip fromRight result $ f . buildAsgFromSpan (spanf span) . invalid . convert



-- ---------------------
-- -- === Markers === --
-- ---------------------

markerIRB :: SymParser (MarkerId, IRBS SomeTerm)
markerIRB = useLastTokenMarker >>= \case
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
               , buildAsgFromSpan (CodeSpan.mkPhantomSpan markerSpan)
                                  (id $ IR.marker' $ t ^. Lexer.element)
               )

marked :: SymParser (IRBS SomeTerm -> IRBS SomeTerm)
marked = option registerUnmarkedExpr $ uncurry markedExpr <$> markerIRB where
    markedExpr mid expr = registerMarkedExpr mid . inheritCodeSpan2 (IR.marked') expr

registerUnmarkedExpr ::             IRBS SomeTerm -> IRBS SomeTerm
registerMarkedExpr   :: MarkerId -> IRBS SomeTerm -> IRBS SomeTerm
registerUnmarkedExpr = withAsgBldr (>>~ addUnmarkedExpr)
registerMarkedExpr m = withAsgBldr (>>~ addMarkedExpr m)



---------------------
-- === Symbols === --
---------------------

stx, etx, eol :: SymParser ()
stx = symbol Lexer.STX
etx = symbol Lexer.ETX
eol = symbol Lexer.EOL

braceBegin, braceEnd :: SymParser ()
braceBegin = symbol $ Lexer.List Lexer.Begin
braceEnd   = symbol $ Lexer.List Lexer.End

groupBegin, groupEnd :: SymParser ()
groupBegin = symbol $ Lexer.Group Lexer.Begin
groupEnd   = symbol $ Lexer.Group Lexer.End



----------------------------
-- === IRB Primitives === --
----------------------------

-- varIRB :: Name -> IRB SomeTerm
-- varIRB = IR.var' ; {-# INLINE varIRB #-}

accIRB :: Name -> SomeTerm -> IRB SomeTerm
accIRB name a = (flip IR.acc' name) a

accSectionIRB :: [Name] -> IRB SomeTerm
accSectionIRB path = IR.accSection' $ convert path

updateIRB :: [Name] -> SomeTerm -> SomeTerm -> IRB SomeTerm
updateIRB path = flip IR.update' (convert path)

modifyIRB :: [Name] -> Name -> SomeTerm -> SomeTerm -> IRB SomeTerm
modifyIRB path op base value = IR.modify' base (convert path) op value


-------------------------
-- === Identifiers === --
-------------------------

cons, var, op, wildcard :: IRBSParser SomeTerm
cons     = snd <$> namedCons                             ; {-# INLINE cons     #-}
var      = snd <$> namedVar                              ; {-# INLINE var      #-}
op       = snd <$> namedOp                               ; {-# INLINE op       #-}
wildcard = buildAsg $ IR.blank' <$ symbol Lexer.Wildcard ; {-# INLINE wildcard #-}

namedCons, namedVar, namedOp, namedIdent :: SymParser (Name, IRBS SomeTerm)
namedCons  = mkNamedAsg (flip IR.cons' []) consName ; {-# INLINE namedCons  #-}
namedVar   = mkNamedAsg IR.var'            varName  ; {-# INLINE namedVar   #-}
namedOp    = mkNamedAsg IR.var'            opName   ; {-# INLINE namedOp    #-}
namedIdent = namedVar <|> namedCons <|> namedOp     ; {-# INLINE namedIdent #-}

consName, varName, opName, identName, modifierName :: SymParser Name
foreignLangName                                    :: SymParser Name
consName        = convert <$> satisfTest Lexer.matchCons     ; {-# INLINE consName        #-}
varName         = convert <$> satisfTest Lexer.matchVar      ; {-# INLINE varName         #-}
opName          = convert <$> satisfTest Lexer.matchOperator ; {-# INLINE opName          #-}
identName       = varName <|> consName <|> opName         ; {-# INLINE identName       #-}
funcName        = varName <|> opName                      ; {-# INLINE funcName        #-}
modifierName    = convert <$> satisfTest Lexer.matchModifier ; {-# INLINE modifierName    #-}
foreignLangName = consName                                ; {-# INLINE foreignLangName #-}

previewVarName :: SymParser Name
previewVarName = do
    s <- previewNextSymbol
    maybe (unexpected . fromString $ "Expecting variable, got: " <> show s) (pure . convert) $ Lexer.matchVar =<< s

specificVar, specificCons, specificOp :: Text32 -> SymParser ()
specificVar  = symbol . Lexer.Var
specificCons = symbol . Lexer.Cons
specificOp   = symbol . Lexer.Operator

-- -- Helpers

mkNamedAsg :: (Name -> IRB (Term a)) -> SymParser Name -> SymParser (Name, IRBS (Term a))
mkNamedAsg cons = buildAsgF . fmap (\name -> (name, cons name))


-- Qualified names

qualVarName, qualConsName :: SymParser [Name]
qualConsName = qualNameBase consName
qualVarName  = qualNameBase varName

qualNameBase :: SymParser Name -> SymParser [Name]
qualNameBase p = (\a b -> a <> [b])
             <$> many (try $ consName <* symbol Lexer.Accessor) <*> p



-- ----------------------
-- -- === Literals === --
-- ----------------------

rawQuoteBegin, rawQuoteEnd :: SymParser ()
fmtQuoteBegin, fmtQuoteEnd :: SymParser ()
rawQuoteBegin = symbol $ Lexer.Quote Lexer.RawStr Lexer.Begin
rawQuoteEnd   = symbol $ Lexer.Quote Lexer.RawStr Lexer.End
fmtQuoteBegin = symbol $ Lexer.Quote Lexer.FmtStr Lexer.Begin
fmtQuoteEnd   = symbol $ Lexer.Quote Lexer.FmtStr Lexer.End

-- FIXME[WD]: move the Char -> Number conversion logic to lexer
number :: IRBSParser SomeTerm
number = buildAsg $ do
    Lexer.NumRep base i f _ <- satisfTest Lexer.matchNumber
    pure $ IR.number' (convert base)
                      (convert $ convertChar <$> convertTo @String i)
                      (convert $ convertChar <$> convertTo @String f)
    where convertChar c = let ord = Char.ord c in
              if | ord >= 48 && ord <= 57  -> (fromIntegral $ ord - 48      :: Word8)
                 | ord >= 97 && ord <= 122 -> (fromIntegral $ ord - 97 + 10 :: Word8)
                 | otherwise               -> error "wrong char"


list :: IRBSParser SomeTerm -> IRBSParser SomeTerm
list p = buildAsg $ Reserved.withLocalUnreservedSymbol sep $ braced $ (\g -> bindIRBS1 IR.list' $ sequence g) <$> elems where
    missing :: IRBSParser SomeTerm
    missing   = buildAsg . pure $ IR.missing'
    sep       = Lexer.Operator ","
    elem      = Reserved.withLocalReservedSymbol sep p
    optElem   = elem <|> missing
    bodyH     = (:) <$> elem    <*> many       (separator *> optElem)
    bodyT     = (:) <$> missing <*> someAsList (separator *> optElem)
    elems     = option mempty $ bodyH <|> bodyT
    braced p  = braceBegin *> p <* braceEnd
    separator = symbol sep

-- FIXME[WD]: This `try` should be refactored out
-- FIXME[WD]: Tuple and List parsers are too similar no to be refactored to common part. However tuples will disappear soon.
tuple :: IRBSParser SomeTerm -> IRBSParser SomeTerm
tuple p = try $ buildAsg $ Reserved.withLocalUnreservedSymbol sep $ parensed
        $ (bindIRBS1 IR.tuple' . sequence) <$> elems where
    missing :: IRBSParser SomeTerm
    missing    = buildAsg $ pure IR.missing'
    sep        = Lexer.Operator ","
    elem       = Reserved.withLocalReservedSymbol sep p
    optElem    = elem <|> missing
    body       = (:) <$> optElem <*> someAsList (separator *> optElem)
    elems      = option mempty body
    parensed p = groupBegin *> p <* groupEnd
    separator  = symbol sep

string :: IRBSParser SomeTerm
string = rawStr <|> fmtStr

rawStr :: IRBSParser SomeTerm
rawStr = buildAsg $ do
    rawQuoteBegin
    withRecovery (\e -> invalid "Invalid string literal" <$ Loc.unregisteredDropSymbolsUntil' (== (Lexer.Quote Lexer.RawStr Lexer.End)))
                 $ (IR.string' . convertVia @String)
               <$> Indent.withCurrent (strBody rawQuoteEnd) -- FIXME[WD]: We're converting Text -> String here.

fmtStr :: IRBSParser SomeTerm
fmtStr = buildAsg $ do
    fmtQuoteBegin
    withRecovery (\e -> invalid "Invalid string literal" <$ Loc.unregisteredDropSymbolsUntil' (== (Lexer.Quote Lexer.FmtStr Lexer.End)))
                 $ (IR.string' . convertVia @String)
               <$> Indent.withCurrent (strBody fmtQuoteEnd) -- FIXME[WD]: We're converting Text -> String here.


strBody :: SymParser () -> SymParser Text32
strBody ending = segStr <|> end <|> nl where
    segStr = (<>) <$> strContent <*> strBody ending
    end    = mempty <$ ending
    nl     = Text32.cons '\n' <$ eol <*> (line <|> nl)
    line   = do Indent.indentedOrEq
                (<>) . convert . flip replicate ' ' <$> Indent.indentation <*> strBody ending

strContent :: SymParser Text32
strContent = satisfTest Lexer.matchStr <|> strEsc

strEsc :: SymParser Text32
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
app          = inheritCodeSpan2 IR.app'
seq          = inheritCodeSpan2 IR.seq'
unify        = inheritCodeSpan2 IR.unify'
sectionLeft  = inheritCodeSpan2 IR.sectionLeft'
appFlipped   = flip . inheritCodeSpan2 $ flip IR.app'
sectionRight = flip . inheritCodeSpan2 $ flip IR.sectionRight'

appSides :: IRBS SomeTerm -> IRBS SomeTerm -> IRBS SomeTerm -> IRBS SomeTerm
appSides = app .: appFlipped

apps, seqs :: IRBS SomeTerm -> [IRBS SomeTerm] -> IRBS SomeTerm
apps = foldl app
seqs = foldl seq

nestedLam :: [IRBS SomeTerm] -> IRBS SomeTerm -> IRBS SomeTerm
nestedLam args body = foldr lam' body args where
    lam' :: IRBS SomeTerm -> IRBS SomeTerm -> IRBS SomeTerm
    lam' = inheritCodeSpan2 $ IR.lam'

grouped :: IRBSParser SomeTerm -> IRBSParser SomeTerm
grouped p = buildAsg $ parensed $ (\g -> bindIRBS1 IR.grouped' g) <$> p where
    parensed p = groupBegin *> p <* groupEnd


-- -- === Metadata === --

-- FIXME: performance
metadata :: IRBSParser SomeTerm
metadata = buildAsg $ IR.metadata' . convertVia @String <$> metaContent

metaContent :: SymParser Text32
metaContent = satisfTest Lexer.matchMetadata


-- === Disabled === --

possiblyDisabled :: IRBSParser SomeTerm -> IRBSParser SomeTerm
possiblyDisabled p = disabled p <|> p

disabled :: IRBSParser SomeTerm -> IRBSParser SomeTerm
disabled p = buildAsg $ bindIRBS1 IR.disabled' <$ symbol Lexer.Disable <*> p


-- === Segments === --

type ExprSegment         = ExprToken (IRBS SomeTerm)
type ExprSegments        = NonEmpty ExprSegment
type ExprSegmentBuilder  = SegmentBuilder ExprSegment
newtype SegmentBuilder a = SegmentBuilder { runSegmentBuilder :: (Bool, Bool) -> NonEmpty a } deriving (Functor)

type ExprToken       a = Expr.Token (ExprSymbol      a)
type ExprTokenProto  a = Expr.Token (ExprSymbolProto a)
type ExprSymbol      a = Labeled SpacedName (SomeSymbol Symbol.Expr    a)
type ExprSymbolProto a = Labeled SpacedName (SomeSymbol Symbol.Phantom a)

instance Semigroup ExprSegmentBuilder where
    a <> b = SegmentBuilder $ \(l,r) -> runSegmentBuilder a (l,False) <> runSegmentBuilder b (False,r)

expr :: IRBSParser SomeTerm
expr = possiblyDocumented $ lineExpr -- WAS: func <|> lineExpr

lineExpr :: IRBSParser SomeTerm
lineExpr = marked <*> (possiblyDisabled (valExpr <**> option id assignment)) where
    assignment = flip unify <$ symbol Lexer.Assignment <*> valExpr

valExpr :: IRBSParser SomeTerm
valExpr =  buildTokenExpr =<< exprSegments

nonemptyValExpr :: IRBSParser SomeTerm
nonemptyValExpr = do
    es <- exprSegments
    -- when (null es) $ unexpected "Empty expression" -- FIXME[WD]
    buildTokenExpr es

nonemptyValExprLocal :: IRBSParser SomeTerm
nonemptyValExprLocal = do
    es <- exprSegmentsLocal
    -- when (null es) $ unexpected "Empty expression" -- FIXME[WD]
    buildTokenExpr es

nonSpacedValExpr :: IRBSParser SomeTerm
nonSpacedValExpr = do
    es <- exprSegmentsNonSpaced
    -- when (null es) $ unexpected "Empty expression" -- FIXME[WD]
    buildTokenExpr es

nonSpacedPattern :: IRBSParser SomeTerm
nonSpacedPattern = nonSpacedValExpr

concatExprSegmentBuilders :: NonEmpty ExprSegmentBuilder -> ExprSegmentBuilder
concatExprSegmentBuilders bldrs = SegmentBuilder $ \(l,r) -> case bldrs of
    (s:|[])     -> runSegmentBuilder s (l,r)
    (s:|(t:ts)) -> runSegmentBuilder s (l,False) <> runSegmentBuilder (concatExprSegmentBuilders $ t:|ts) (False,r)

exprSegments    , exprSegmentsLocal     :: SymParser ExprSegments
exprFreeSegments, exprFreeSegmentsLocal :: SymParser ExprSegmentBuilder
exprSegments          = buildExprTok <$> exprFreeSegments
exprSegmentsLocal     = buildExprTok <$> exprFreeSegmentsLocal
exprFreeSegments      = Reserved.withNewLocal exprFreeSegmentsLocal
exprFreeSegmentsLocal = fmap concatExprSegmentBuilders . some
                      $ choice [ mfixVarSeg, consSeg, wildSeg, numSeg, strSeg
                               , opSeg, accSeg, tupleSeg, grpSeg, listSeg
                               , lamSeg, matchseg, typedSeg, funcSeg
                               ]

exprSegmentsNonSpaced    , exprSegmentsNonSpacedLocal     :: SymParser ExprSegments
exprFreeSegmentsNonSpaced, exprFreeSegmentsNonSpacedLocal :: SymParser ExprSegmentBuilder
exprSegmentsNonSpaced          = buildExprTok <$> exprFreeSegmentsNonSpaced
exprSegmentsNonSpacedLocal     = buildExprTok <$> exprFreeSegmentsNonSpacedLocal
exprFreeSegmentsNonSpaced      = Reserved.withNewLocal exprFreeSegmentsNonSpacedLocal
exprFreeSegmentsNonSpacedLocal = choice [ varSeg, consSeg, wildSeg, numSeg
                                        , strSeg, tupleSeg, grpSeg, listSeg
                                        ]


-- === Components === --

-- Utils
posIndependent = SegmentBuilder . const . pure . Expr.tokenx

-- FIXME[WD]: change the API to monadic one, so we can register symbols and mixfix monads could gather needed info (like var names)
--            without the need to keep the label in the term
unlabeledAtom :: a -> Labeled SpacedName (SomeSymbol Symbol.Expr a)
unlabeledAtom = labeled (Name.spaced "#unnamed#") . Symbol.atom

-- -- Possible tokens
varSeg, consSeg, wildSeg, numSeg, strSeg, listSeg, grpSeg, tupleSeg, matchseg
      , lamSeg , funcSeg :: SymParser ExprSegmentBuilder
varSeg   = posIndependent . unlabeledAtom <$> var
consSeg  = posIndependent . unlabeledAtom <$> cons
wildSeg  = posIndependent . unlabeledAtom <$> wildcard
numSeg   = posIndependent . unlabeledAtom <$> number
strSeg   = posIndependent . unlabeledAtom <$> string
grpSeg   = posIndependent . unlabeledAtom <$> grouped nonemptyValExpr
listSeg  = posIndependent . unlabeledAtom <$> list  nonemptyValExprLocal
tupleSeg = posIndependent . unlabeledAtom <$> tuple nonemptyValExprLocal
matchseg = posIndependent . unlabeledAtom <$> match
lamSeg   = posIndependent . labeled (Name.unspaced Builtin.lamName) . Symbol.suffix <$> lamBldr
funcSeg  = posIndependent . unlabeledAtom <$> func


mfixVarSeg :: SymParser ExprSegmentBuilder
mfixVarSeg  = do
    (span, name) <- spanned varName
    nameSet      <- Scope.lookupMultipartNames name
    let cvar = posIndependent $ unlabeledAtom $ buildAsgFromSpan span (IR.var' name)
    if TreeSet.null nameSet
        then pure cvar
        else -- catchInvalidWith (span <>) (pure . posIndependent . unlabeledAtom)
           {-$-} Reserved.withReservedSymbols (Lexer.Var . convertVia @String <$> TreeSet.keys nameSet) -- FIXME: conversion Name -> Text
           $ do segmentToks <- exprFreeSegmentsLocal
                namedSegs   <- option mempty (parseMixfixSegments nameSet)
                if null namedSegs then pure (cvar <> segmentToks) else do
                    segment <- buildTokenExpr (buildExprTok segmentToks)
                    let segments  = snd <$> namedSegs
                        nameParts = fst <$> namedSegs
                        mfixVar   = buildAsgFromSpan span
                                  . IR.var' . Name.mixfix $ name :| nameParts
                        mfixExpr  = apps (app mfixVar segment) segments
                    pure . posIndependent . unlabeledAtom $ mfixExpr

opSeg :: SymParser ExprSegmentBuilder
opSeg   = do
    (before, after) <- checkOffsets
    (span, name)    <- spanned opName
    let segment (isFirst, isLast) = pure . Expr.tokenx $ operator & if
            | isSingle    -> labeled (Name.unspaced            name) . Symbol.atom
            | isUMinus    -> labeled (Name.unspaced Builtin.uminusName) . Symbol.prefix . app
            | isFirst     -> labeled (Name.spacedNameIf after  name) . Symbol.prefix . sectionLeft
            | isLast      -> labeled (Name.spacedNameIf before name) . Symbol.suffix . sectionRight
            | symmetrical -> labeled (Name.spacedNameIf after  name) . Symbol.infixx . appSides
            | before      -> labeled (Name.lspaced             name) . Symbol.prefix . sectionLeft
            | otherwise   -> labeled (Name.rspaced             name) . Symbol.suffix . sectionRight
            where isMinus     = name == Builtin.minusName
                  isSingle    = isFirst && isLast
                  isUMinus    = isMinus && not after && (isFirst || before) && not isLast
                  operator    = buildAsgFromSpan span . IR.var' $ if isUMinus then Builtin.uminusName else name
                  symmetrical = before == after
    pure $ SegmentBuilder segment

typedSeg :: SymParser ExprSegmentBuilder
typedSeg   = do
    (off, off') <- checkOffsets
    let typedExpr = if off then valExpr else nonSpacedValExpr
    tp <- symbol Lexer.Typed *> typedExpr
    when_ (off /= off') $ error "TODO: before and after offsets have to match"
    let seg :: IRBS SomeTerm -> IRBS SomeTerm
        seg = flip (inheritCodeSpan2 IR.typed') tp
    pure . posIndependent . labeled (Name.spacedNameIf off Builtin.typedName) $ Symbol.suffix seg

accSectNames :: SymParser (NonEmpty (CodeSpan, Name))
accSectNames = do
    sname <- spanned $ symbol Lexer.Accessor *> identName
    (beforeDot, afterDot) <- checkOffsets
    (sname :|) <$> if beforeDot || afterDot then pure   mempty
                                            else option mempty (convert <$> accSectNames)



accSeg :: SymParser ExprSegmentBuilder
accSeg = fmap2 Expr.tokenx $ do
    (beforeDot, afterDot) <- checkOffsets
    snames@(n :| ns) <- accSectNames
    mupdt <- option Nothing (Just <$ symbol Lexer.Assignment <*> nonemptyValExpr)
    mmod  <- option Nothing (Just .: (,) <$> modifierName <*> nonemptyValExpr)
    let (spans, names) = unzip $ convert snames
        symmetrical    = beforeDot == afterDot
        accCons s n    = inheritCodeSpan1With (<> s) (accIRB n)
        accSect        = labeled uname . Symbol.atom
                       $ buildAsgFromSpan (mconcat spans)
                       $ accSectionIRB names
        uname          = Name.unspaced Builtin.accName
        sname          = Name.spacedNameIf beforeDot Builtin.accName
        accConss       =  labeled sname ( Symbol.suffix $ uncurry accCons n )
                      :| (labeled uname . Symbol.suffix . uncurry accCons <$> ns)
        Just fupdt           = mupdt -- FIXME[WD]: make it nicer
        Just (modName, fmod) = mmod  -- FIXME[WD]: make it nicer

        -- FIXME[WD]: make it nicer vvv
        updateAtom = labeled sname . Symbol.suffix
                   $ flip (inheritCodeSpan2With (<>) (updateIRB names))
                          (modifyCodeSpan (CodeSpan.asOffsetSpan (mconcat spans) <>) fupdt)
        modifyAtom = labeled sname . Symbol.suffix
                   $ flip (inheritCodeSpan2With (<>) (modifyIRB names modName))
                          (modifyCodeSpan (CodeSpan.asOffsetSpan (mconcat spans) <>) fmod)


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

    let segment (isFirst, isLast) = if
            | isFirst     -> pure accSect
             -- FIXME[WD]: make it nicer vvv
            | symmetrical -> if
                | isJust mupdt -> if True == True then updateAtomHack else pure updateAtom -- hack for typechecker (see the above description)
                | isJust mmod  -> if True == True then modifyAtomHack else pure modifyAtom -- hack for typechecker (see the above description)
                | otherwise    -> accConss
            | beforeDot   -> pure accSect
            | otherwise   -> error "unsupported" -- FIXME[WD]: make nice error here
    pure $ SegmentBuilder segment

match :: IRBSParser SomeTerm
match = buildAsg $ (\n (p,ps) -> bindIRBS2 IR.match' n (sequence $ p:ps))
      <$ symbol Lexer.KwCase <*> nonemptyValExprLocal
      <* symbol Lexer.KwOf   <*> discover (nonEmptyBlock clause)
      where clause = pat <**> lamBldr

pat :: IRBSParser SomeTerm
pat = Reserved.withLocalReservedSymbol Lexer.BlockStart nonemptyValExprLocal


lamBldr :: SymParser (IRBS SomeTerm -> IRBS SomeTerm)
lamBldr = do
    body <- symbol Lexer.BlockStart *> discover (nonEmptyBlock lineExpr)
    pure $ flip (inheritCodeSpan2 IR.lam') (uncurry seqs body)


-- === Utils === --

buildExprTok :: ExprSegmentBuilder -> ExprSegments
buildExprTok bldr = runSegmentBuilder bldr (True, True)

parseMixfixSegments :: SparseTreeSet Name -> SymParser [(Name, IRBS SomeTerm)]
parseMixfixSegments nameSet = do
    name <- previewVarName
    let total = TreeSet.check' name nameSet
    case nameSet ^? ix name of
        Nothing       -> unexpected . fromString $ "Identifier `" <> convert name <> "` is not part of a mutli-name expression"
        Just nameSet' -> do
            let possiblePaths = TreeSet.keys nameSet'
            anySymbol_
            segment <- Reserved.withReservedSymbols (Lexer.Var . convertVia @String <$> possiblePaths) nonemptyValExpr -- FIXME: conversion Name -> Text
            let restMod = if total then option mempty else (<|> unexpected (fromString $ "Unexpected end of mixfix expression, expecting one of " <> show possiblePaths))
            ((name,segment):) <$> restMod (parseMixfixSegments nameSet')

buildTokenExpr (s:|ss) = buildTokenExpr' (s:ss)

buildTokenExpr' :: Expr.Tokens (Labeled SpacedName (SomeSymbol Symbol.Expr (IRBS SomeTerm))) -> SymParser (IRBS SomeTerm)
buildTokenExpr' = Expr.buildExpr_termApp . Labeled (Name.spaced Builtin.appName) $ Symbol.Symbol app


---------------------------
-- === Documentation === --
---------------------------

possiblyDocumented :: IRBSParser SomeTerm -> IRBSParser SomeTerm
possiblyDocumented p = documented p <|> p

-- FIXME: performance
documented :: IRBSParser SomeTerm -> IRBSParser SomeTerm
documented p = buildAsg $ (\d t -> bindIRBS1 (IR.documented' $ convertVia @String d) t) <$> doc <*> p

doc :: SymParser Text32
doc = intercalate "\n" <$> some (satisfTest Lexer.matchDocComment <* eol)


--------------------------
-- === Declarations === --
--------------------------

topLvlDecl :: IRBSParser SomeTerm
topLvlDecl = possiblyDocumented $ func <|> record <|> metadata


-- === Functions === --

func :: IRBSParser SomeTerm
func = buildAsg $ funcHdr <**> (funcDef <|> funcSig) where
    funcDef, funcSig :: SymParser (IRBS SomeTerm -> IRB SomeTerm)
    funcHdr = symbol Lexer.KwDef *> (var <|> op)
    funcSig = (\tp name -> bindIRBS2 IR.functionSig' name tp) <$ symbol Lexer.Typed <*> valExpr
    funcDef = (\args body name -> bindIRBS3 IR.function' name (sequence args) (uncurry seqs body))
        <$> many nonSpacedPattern
        <*  symbol Lexer.BlockStart
        <*> discover (nonEmptyBlock lineExpr)


-- -- === Classes == --

record :: IRBSParser SomeTerm
record = buildAsg $ (\nat n args (cs,ds) -> bindIRBS3 (IR.record' nat n) (sequence args) (sequence cs) (sequence ds))
   <$> try (option False (True <$ symbol Lexer.KwNative) <* symbol Lexer.KwClass) <*> consName <*> many var <*> body
    where body      = option mempty $ symbol Lexer.BlockStart *> bodyBlock
          funcBlock = optionalBlockBody (possiblyDocumented func)
          consBlock = breakableNonEmptyBlockBody' recordCons <|> breakableOptionalBlockBody recordNamedFields
          bodyBlock = discoverIndent ((,) <$> consBlock <*> funcBlock)

recordCons :: IRBSParser SomeTerm
recordCons = buildAsg $ (\n fields -> bindIRBS1 (IR.recordCons' n) (sequence fields)) <$> consName <*> (blockDecl <|> inlineDecl) where
    blockDecl  = symbol Lexer.BlockStart *> discover (nonEmptyBlock' recordNamedFields)
    inlineDecl = many unnamedField

recordNamedFields :: IRBSParser SomeTerm
recordNamedFields = buildAsg $ bindIRBS1 . IR.recordFields' . convert
                <$> many varName <* symbol Lexer.Typed <*> valExpr

unnamedField :: IRBSParser SomeTerm
unnamedField = buildAsg $ bindIRBS1 (IR.recordFields' mempty)
                      <$> nonSpacedValExpr



--------------------
-- === Units === --
--------------------

-- === Imports === --

impHub :: IRBSParser SomeTerm
impHub = buildAsg $ (\(imps :: [IRBS SomeTerm])
                 -> bindIRBS1 IR.importHub'
                    (sequence imps :: IRBS [SomeTerm])) <$> impHeader

impHeader :: SymParser [IRBS SomeTerm]
impHeader = option mempty $ breakableOptionalBlockTop imp -- <* skipEmptyLines

imp :: IRBSParser SomeTerm
imp = buildAsg $ (\a tgts -> bindIRBS1 (flip IR.imp' tgts) a)
   <$ symbol Lexer.KwImport <*> importSource <*> impTgt where
    impTgt  = option Import.Everything $ symbol Lexer.BlockStart *> listTgt
    listTgt = Import.Listed . convert <$> many (varName <|> consName)

importSource :: IRBSParser SomeTerm
importSource = buildAsg . fmap IR.importSource' $ choice
    [ Import.World              <$  specificCons "World"
    , Import.Relative . convert <$  symbol Lexer.Accessor <*> qualConsName
    , Import.Absolute . convert <$> qualConsName
    ]


-- ------------------------------------
-- -- === Foreign Import Parsing === --+
-- ------------------------------------

foreignImportList :: IRBSParser SomeTerm
foreignImportList = buildAsg $
    (\lang imports ->
        bindIRBS1 (IR.foreignImport' lang) (sequence imports))
    <$  (symbol Lexer.KwForeign *> symbol Lexer.KwImport)
    <*> foreignLangName
    <*  symbol Lexer.BlockStart
    <*> discover (nonEmptyBlock' foreignLocationImportList)

foreignLocationImportList :: IRBSParser SomeTerm
foreignLocationImportList = buildAsg $
    (\loc imports ->
        bindIRBS2 IR.foreignImportList' loc (sequence imports))
    <$> stringOrVarName
    <*  symbol Lexer.BlockStart
    <*> discover (nonEmptyBlock' foreignSymbolImport)

foreignSymbolImport :: IRBSParser SomeTerm
foreignSymbolImport = buildAsg $ withRecovery recover
    $   try (foreignSymbolImportWithSafety defaultFISafety)
    <|> foreignSymbolImportWithSafety specifiedFISafety
    where recover e = invalid "Invalid safety specification."
                      <$ Loc.unregisteredDropSymbolsUntil
                      (`elem` [Lexer.ETX, Lexer.EOL])

foreignSymbolImportWithSafety :: IRBSParser SomeTerm -> SymParser (IRB SomeTerm)
foreignSymbolImportWithSafety safe =
    (\safety forName localName importType ->
        bindIRBS3 (foreignSymbolProxy localName) safety forName importType)
    <$> safe <*> stringOrVarName <*> funcName <*  symbol Lexer.Typed <*> valExpr
    where foreignSymbolProxy a b c d = IR.foreignImportSymbol' b c a d

defaultFISafety :: IRBSParser SomeTerm
defaultFISafety = buildAsg $
    (\safety -> id (IR.foreignImportSafety' safety))
    <$> ((pure Import.Default) :: SymParser IR.ForeignImportType)

-- TODO [Ara, WD] Need to have the lexer deal with these as contextual keywords
-- using a positive lookahead in the _Lexer_.
specifiedFISafety :: IRBSParser SomeTerm
specifiedFISafety = buildAsg $
    (\(importSafety :: IR.ForeignImportType) ->
        id (IR.foreignImportSafety' importSafety))
    <$> getImpSafety (optionMaybe varName)
    where getImpSafety :: SymParser (Maybe Name) -> SymParser IR.ForeignImportType
          getImpSafety p = p >>= \case
              Nothing -> pure Import.Default
              Just n  -> case n of
                  "safe"   -> pure Import.Safe
                  "unsafe" -> pure Import.Unsafe
                  _        -> fail "Invalid safety specification."

stringOrVarName :: IRBSParser SomeTerm
stringOrVarName = string <|> (asgNameParser varName)

asgNameParser :: SymParser Name -> IRBSParser SomeTerm
asgNameParser nameParser = buildAsg $ (\varN -> id (IR.var' varN))
     <$> nameParser



-- -- === Unit body === --

unit' :: IRBSParser SomeTerm
unit  :: IRBSParser (IR.Term IR.Unit)
unit' = Layout.relayout <<$>> unit
unit  = buildAsg $
    (\imps cls
        -> Layout.unsafeRelayout <$> bindIRBS2 (flip IR.unit []) imps cls)
        <$ spacing <*> (foreignImportList <|> impHub) <*> unitCls <* spacing
    where spacing = many eol

unitCls :: IRBSParser SomeTerm
unitCls = buildAsg $ (\ds -> bindIRBS1 (IR.record' False "" [] [])
                     (sequence ds)) <$> optionalBlockTop topLvlDecl



----------------------------
-- === Layout parsing === --
----------------------------

discover :: SymParser a -> SymParser a
discover p = many eol >> p

discoverIndent :: SymParser a -> SymParser a
discoverIndent = discover . Indent.withCurrent

nonEmptyBlock', nonEmptyBlockBody' :: SymParser a -> SymParser [a]
nonEmptyBlock'     = uncurry (:) <<$>> nonEmptyBlock
nonEmptyBlockBody' = uncurry (:) <<$>> nonEmptyBlockBody

nonEmptyBlock , nonEmptyBlockBody  :: SymParser a -> SymParser (a, [a])
nonEmptyBlock       = Indent.withCurrent . nonEmptyBlockBody
nonEmptyBlockTop    = Indent.withRoot    . nonEmptyBlockBody
nonEmptyBlockBody p = (,) <$> p <*> lines where
    spacing = many eol
    indent  = spacing <* Indent.indentedEq <* notFollowedBy etx
    lines   = many $ try indent *> p

optionalBlock, optionalBlockTop, optionalBlockBody :: SymParser a -> SymParser [a]
optionalBlock     p = option mempty $ uncurry (:) <$> nonEmptyBlock     p
optionalBlockTop  p = option mempty $ uncurry (:) <$> nonEmptyBlockTop  p
optionalBlockBody p = option mempty $ uncurry (:) <$> nonEmptyBlockBody p

breakableNonEmptyBlock', breakableNonEmptyBlockTop', breakableNonEmptyBlockBody' :: SymParser a -> SymParser     [a]
breakableNonEmptyBlock , breakableNonEmptyBlockTop , breakableNonEmptyBlockBody  :: SymParser a -> SymParser (a, [a])
breakableNonEmptyBlock'      = uncurry (:) <<$>> breakableNonEmptyBlock
breakableNonEmptyBlockTop'   = uncurry (:) <<$>> breakableNonEmptyBlockTop
breakableNonEmptyBlockBody'  = uncurry (:) <<$>> breakableNonEmptyBlockBody
breakableNonEmptyBlock       = Indent.withCurrent . breakableNonEmptyBlockBody
breakableNonEmptyBlockTop    = Indent.withRoot    . breakableNonEmptyBlockBody
breakableNonEmptyBlockBody p = (,) <$> lexedp <*> lines where
    spacing = many eol
    lines   = many $ Indent.indentedEq *> lexedp
    lexedp  = p <* spacing

breakableOptionalBlock, breakableOptionalBlockTop, breakableOptionalBlockBody :: SymParser a -> SymParser [a]
breakableOptionalBlock     p = option mempty $ uncurry (:) <$> breakableNonEmptyBlock     p
breakableOptionalBlockTop  p = option mempty $ uncurry (:) <$> breakableNonEmptyBlockTop  p
breakableOptionalBlockBody p = option mempty $ uncurry (:) <$> breakableNonEmptyBlockBody p



-- --------------------
-- -- === Parser === --
-- --------------------

-- type FileName = Literal.String


-- type ParsingPassReq2 m = Req m '[ Editor // Net   // '[AnyExpr, AnyExprLink]
--                                , Editor  // Layer // AnyExpr // CodeSpan
--                                , Reader  // Attr  // Source
--                                , Writer  // Attr  // '[ParsedExpr, MarkedExprMap]
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




-- parsingPassM :: (MonadPassManager m, ParsingPassReq2 m) => IRBSParser SomeTerm -> m ()
-- parsingPassM p = do
--     src <- getAttr @Source
--     (ref, gidMap) <- parsingBase p (convert src)
--     putAttr @ParsedExpr    $ wrap ref
--     putAttr @MarkedExprMap $ gidMap


parsingBase :: IRBSParser a -> Text32.Text32 -> Pass Parser (a, MarkedExprMap)
parsingBase p src = do
    let stream = Lexer.evalDefLexer src
    result <- runParserT (stx *> p <* etx) stream
    case result of
        Left e -> error ("Parser error: " <> parseErrorPretty e) -- FIXME[WD]: handle it the proper way
        Right (IRBS irb) -> do
            ((ref, unmarked), gidMap) <- State.runDefT @MarkedExprMap $ State.runDefT @UnmarkedExprs irb
            pure (ref, gidMap)

-- parsingBase_ :: ( MonadPassManager m, ParsingPassReq_2 m
--                 , UnsafeGeneralizable a SomeTerm, UnsafeGeneralizable a SomeTerm -- FIXME[WD]: Constraint for testing only
--                 ) => IRBSParser a -> Text32 -> m a
-- parsingBase_ = view _1 .:. parsingBase

-- parserPassX  :: MonadPassManager m => IRBSParser SomeTerm -> Pass Parsing   m
-- parserPassX  = parsingPassM

-- reparserPass :: MonadPassManager m => IRBSParser SomeTerm -> Pass Reparsing m
-- reparserPass p = do
--     -- Reading previous analysis
--     gidMapOld <- getAttr @MarkedExprMap
--     refOld    <- getAttr @ParsedExpr

--     -- parsing new file and updating updated analysis
--     putAttr @MarkedExprMap mempty
--     parsingPassM p
--     gidMap    <- getAttr @MarkedExprMap
--     ref       <- getAttr @ParsedExpr

--     -- Preparing reparsing status
--     rs        <- cmpMarkedExprMaps gidMapOld gidMap
--     putAttr @ReparsingStatus (wrap rs)



-- cmpMarkedExprMaps :: IsomorphicCheckCtx m => MarkedExprMap -> MarkedExprMap -> m [ReparsingChange]
-- cmpMarkedExprMaps (oldMap'@(_unwrap -> oldMap)) (_unwrap -> newMap) = (remExprs <>) <$> mapM (uncurry $ cmpMarkedExpr oldMap') newAssocs where
--     newAssocs = Map.assocs newMap
--     oldAssocs = Map.assocs oldMap
--     remExprs  = fmap RemovedExpr . catMaybes $ (\(k,v) -> if_ (not $ Map.member k newMap) (Just v)) <$> oldAssocs

-- cmpMarkedExpr :: IsomorphicCheckCtx m => MarkedExprMap -> MarkerId -> SomeTerm -> m ReparsingChange
-- cmpMarkedExpr (_unwrap -> map) mid newExpr = case map ^. at mid of
--     Nothing      -> pure $ AddedExpr newExpr
--     Just oldExpr -> checkIsoExpr (unsafeGeneralize oldExpr) (unsafeGeneralize newExpr) <&> \case -- FIXME [WD]: remove unsafeGeneralize, we should use SomeTerm / SomeTerm everywhere
--         False -> ChangedExpr   oldExpr newExpr
--         True  -> UnchangedExpr oldExpr newExpr
