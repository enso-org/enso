{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

module Luna.Syntax.Text.Parser.Parsing where

import Prologue_old hiding (Cons, String, Type, Symbol, UniSymbol, (|>), (<|), cons, seq, span, op)
import qualified Prologue_old as P

import qualified Text.Megaparsec as Parser
import           Text.Megaparsec (notFollowedBy, ErrorItem (Tokens), withRecovery, char, manyTill, anyChar, ParseError, try, hidden, spaceChar, skipMany, digitChar, letterChar, unexpected, between, lowerChar, upperChar, choice, string, lookAhead)
import           Text.Megaparsec.Error (parseErrorPretty, parseErrorTextPretty)
import           Text.Megaparsec.Prim (MonadParsec)
import           Text.Megaparsec.String hiding (Parser)
import qualified Text.Megaparsec.String as M
import qualified Text.Megaparsec.Lexer as Lex
import qualified Text.Megaparsec.Char  as Charhiding (eol)

import qualified Luna.IR as IR
import           Luna.IR hiding (typed, unify, accSection, leftSection, rightSection, unit', match, list, tuple, clause, Atom, State, IRB, number, string, var, expr, app, acc, grouped, blank, get, put, modify_, cons, lam, seq, function, withIR, fieldLens, clsASG, unit, imp, impSrc, impHub, invalid, marker, marked, metadata, disabled, documented)
import Luna.IR.ToRefactor
import OCI.Pass.Class hiding (get, put, modify_)
import OCI.Pass.Definition

import Luna.Syntax.Text.Layer.Loc

import Luna.Syntax.Text.Parser.Class
import Luna.Syntax.Text.Parser.Parser
import Luna.Syntax.Text.Parser.CodeSpan (CodeSpan, CodeSpanRange)
import qualified Luna.Syntax.Text.Parser.CodeSpan as CodeSpan

import qualified Luna.IR.Term.Function as Function

import qualified Data.Text.Span as Span
import           Data.Text.Span (LeftSpacedSpan, leftSpacedSpan)
import System.Log (MonadLogging)
import qualified Data.Char as Char
import Data.List.NonEmpty (NonEmpty((:|)))
import Text.Megaparsec.Ext

import qualified Data.Set as Set
import           Data.Set (Set)
import qualified Data.Map as Map
import           Data.Map (Map)
import qualified Control.Monad.State as S
import           Text.Parser.Expr hiding (PrecRelMap)
import Luna.Syntax.Text.Scope

import System.IO.Unsafe (unsafePerformIO)
import qualified Luna.Syntax.Text.Pretty.Pretty as CodeGen
import qualified Language.Symbol.Operator.Assoc as Assoc
import qualified Language.Symbol.Operator.Prec  as Prec
import           Language.Symbol hiding (Expr, symbol, Symbol, ExprSymbol)
import qualified Language.Symbol as Symbol
import qualified Language.Symbol as Tok
import           Luna.Syntax.Text.Parser.Hardcoded
import qualified OCI.Pass        as Pass
import Data.Container.Mono hiding (empty)
import           OCI.IR.Name.Qualified (QualName, mkQualName)
import qualified OCI.IR.Name.Path      as Name
import qualified OCI.IR.Name.Multipart as MName
import           OCI.IR.Name.Multipart (mkMultipartName)
import qualified Luna.IR.Term.Literal  as Literal
import qualified Luna.IR.Term.Literal  as Num
import           Text.Parser.Indent (Indent, indentation)
import Luna.Syntax.Text.Parser.Marker (MarkerState, MarkerId, MarkedExprMap, UnmarkedExprs, addMarkedExpr, addUnmarkedExpr, getLastTokenMarker, useLastTokenMarker)
import qualified Text.Parser.Indent as Indent
import Control.Monad.State.Dependent
import Text.Parser.Combinators
import Data.Text.Position
import Text.Parser.Char hiding (eol)
import qualified Text.Parser.Char as Char
import Luna.Syntax.Text.Parser.Name

import qualified System.Environment as Env
import Luna.Syntax.Text.Source
import Luna.IR.Analysis (IsomorphicCheckCtx, checkIsoExpr)
import qualified Luna.IR.Term.Unit as Import
import OCI.IR.Layout.Typed (type (>>))
import qualified Control.Monad as M


import qualified Control.Monad as Monad
import qualified Luna.Syntax.Text.Lexer as Lexer

import Luna.Syntax.Text.Parser.Loc (withRecovery2, token', getNextToken, previewNextSymbol, uncheckedPreviewNextToken, uncheckedGetNextSymbol, checkNextOffset, getNextOffset, previewSymbols)
import qualified Luna.Syntax.Text.Parser.Loc as Loc
import           Luna.Syntax.Text.Parser.Loc (LeftSpanner, MonadLoc)


import Luna.Syntax.Text.Parser.Errors (Invalids, registerInvalid)
import qualified Data.List as List
import qualified Luna.Syntax.Text.Parser.Reserved as Reserved
import           Luna.Syntax.Text.Parser.Reserved (Reservation, withReservedSymbols, withLocalReservedSymbol, withLocalUnreservedSymbol, withNewLocal, checkForSymbolReservation)
import qualified Data.TreeSet as TreeSet
import           Data.TreeSet (SparseTreeSet)
import Luna.Syntax.Text.Parser.Class (Stream, Symbol)

import           Data.Text32 (Text32)
import qualified Data.Text32 as Text32
import qualified Data.Text as Text


some' :: (Applicative m, Alternative m) => m a -> m (NonEmpty a)
some' p = (:|) <$> p <*> many p


------------------------
-- === Primitives === --
------------------------

satisfy' :: (Symbol -> Bool)    -> SymParser Symbol
satisfy_ :: (Symbol -> Bool)    -> SymParser ()
satisfy  :: (Symbol -> Maybe a) -> SymParser a
satisfy_ f = void $ satisfy' f
satisfy' f = satisfy $ \s -> justIf (f s) s
satisfy  f = token' testSymbol Nothing where
    testSymbol r t = case Reserved.lookupSymbolReservation r (t ^. Lexer.element) of
        True  -> Left (Set.singleton (Tokens (t:|[])), Set.empty, Set.empty)
        False -> case f (t ^. Lexer.element) of
            Just a  -> Right a
            Nothing -> Left (Set.singleton (Tokens (t:|[])), Set.empty, Set.empty)

satisfyReserved' :: (Symbol -> Bool)    -> SymParser Symbol
satisfyReserved_ :: (Symbol -> Bool)    -> SymParser ()
satisfyReserved  :: (Symbol -> Maybe a) -> SymParser a
satisfyReserved_ f = void $ satisfyReserved' f
satisfyReserved' f = satisfyReserved $ \s -> justIf (f s) s
satisfyReserved  f = token' testSymbol Nothing where
    testSymbol r t = case f (t ^. Lexer.element) of
        Just a  -> Right a
        Nothing -> Left (Set.singleton (Tokens (t:|[])), Set.empty, Set.empty)

symbol :: Symbol -> SymParser ()
symbol = satisfy_ . (==)

anySymbol :: SymParser Symbol
anySymbol = satisfyReserved' $ const True

dropNextToken :: SymParser ()
dropNextToken = satisfyReserved_ $ const True

getLastOffset   :: MonadGetter LeftSpanner m => m Delta
checkLastOffset :: MonadGetter LeftSpanner m => m Bool
getLastOffset   = unwrap <$> get @LeftSpanner
checkLastOffset = (>0)   <$> getLastOffset

checkOffsets :: (MonadParsec e Stream m, MonadGetter LeftSpanner m) => m (Bool, Bool)
checkOffsets = (,) <$> checkLastOffset <*> checkNextOffset



----------------------------------
-- === AST function lifting === --
----------------------------------

-- === Lifting === --

liftIRB0 :: (forall m. IRBuilding m =>                               m out) -> (                              IRB out)
liftIRB1 :: (forall m. IRBuilding m => t1                         -> m out) -> (t1                         -> IRB out)
liftIRB2 :: (forall m. IRBuilding m => t1 -> t2                   -> m out) -> (t1 -> t2                   -> IRB out)
liftIRB3 :: (forall m. IRBuilding m => t1 -> t2 -> t3             -> m out) -> (t1 -> t2 -> t3             -> IRB out)
liftIRB4 :: (forall m. IRBuilding m => t1 -> t2 -> t3 -> t4       -> m out) -> (t1 -> t2 -> t3 -> t4       -> IRB out)
liftIRB5 :: (forall m. IRBuilding m => t1 -> t2 -> t3 -> t4 -> t5 -> m out) -> (t1 -> t2 -> t3 -> t4 -> t5 -> IRB out)
liftIRB0 f                = IRB $ f
liftIRB1 f t1             = IRB $ f t1
liftIRB2 f t1 t2          = IRB $ f t1 t2
liftIRB3 f t1 t2 t3       = IRB $ f t1 t2 t3
liftIRB4 f t1 t2 t3 t4    = IRB $ f t1 t2 t3 t4
liftIRB5 f t1 t2 t3 t4 t5 = IRB $ f t1 t2 t3 t4 t5

liftIRBApp0 :: (forall m. IRBuilding m =>                               m out) -> IRB out
liftIRBApp1 :: (forall m. IRBuilding m => t1                         -> m out) -> IRB t1 -> IRB out
liftIRBApp2 :: (forall m. IRBuilding m => t1 -> t2                   -> m out) -> IRB t1 -> IRB t2 -> IRB out
liftIRBApp3 :: (forall m. IRBuilding m => t1 -> t2 -> t3             -> m out) -> IRB t1 -> IRB t2 -> IRB t3 -> IRB out
liftIRBApp4 :: (forall m. IRBuilding m => t1 -> t2 -> t3 -> t4       -> m out) -> IRB t1 -> IRB t2 -> IRB t3 -> IRB t4 -> IRB out
liftIRBApp5 :: (forall m. IRBuilding m => t1 -> t2 -> t3 -> t4 -> t5 -> m out) -> IRB t1 -> IRB t2 -> IRB t3 -> IRB t4 -> IRB t5 -> IRB out
liftIRBApp0 f                     = do liftIRB0 f
liftIRBApp1 f mt1                 = do t1 <- mt1; liftIRB1 f t1
liftIRBApp2 f mt1 mt2             = do t1 <- mt1; t2 <- mt2; liftIRB2 f t1 t2
liftIRBApp3 f mt1 mt2 mt3         = do t1 <- mt1; t2 <- mt2; t3 <- mt3; liftIRB3 f t1 t2 t3
liftIRBApp4 f mt1 mt2 mt3 mt4     = do t1 <- mt1; t2 <- mt2; t3 <- mt3; t4 <- mt4; liftIRB4 f t1 t2 t3 t4
liftIRBApp5 f mt1 mt2 mt3 mt4 mt5 = do t1 <- mt1; t2 <- mt2; t3 <- mt3; t4 <- mt4; t5 <- mt5; liftIRB5 f t1 t2 t3 t4 t5

liftAstApp0 :: (forall m. IRBuilding m =>                               m out) -> IRB out
liftAstApp1 :: (forall m. IRBuilding m => t1                         -> m out) -> AsgBldr t1 -> IRB out
liftAstApp2 :: (forall m. IRBuilding m => t1 -> t2                   -> m out) -> AsgBldr t1 -> AsgBldr t2 -> IRB out
liftAstApp3 :: (forall m. IRBuilding m => t1 -> t2 -> t3             -> m out) -> AsgBldr t1 -> AsgBldr t2 -> AsgBldr t3 -> IRB out
liftAstApp4 :: (forall m. IRBuilding m => t1 -> t2 -> t3 -> t4       -> m out) -> AsgBldr t1 -> AsgBldr t2 -> AsgBldr t3 -> AsgBldr t4 -> IRB out
liftAstApp5 :: (forall m. IRBuilding m => t1 -> t2 -> t3 -> t4 -> t5 -> m out) -> AsgBldr t1 -> AsgBldr t2 -> AsgBldr t3 -> AsgBldr t4 -> AsgBldr t5 -> IRB out
liftAstApp0 f                = liftIRBApp0 f
liftAstApp1 f t1             = liftIRBApp1 f (unwrap t1)
liftAstApp2 f t1 t2          = liftIRBApp2 f (unwrap t1) (unwrap t2)
liftAstApp3 f t1 t2 t3       = liftIRBApp3 f (unwrap t1) (unwrap t2) (unwrap t3)
liftAstApp4 f t1 t2 t3 t4    = liftIRBApp4 f (unwrap t1) (unwrap t2) (unwrap t3) (unwrap t4)
liftAstApp5 f t1 t2 t3 t4 t5 = liftIRBApp5 f (unwrap t1) (unwrap t2) (unwrap t3) (unwrap t4) (unwrap t5)

-- FIXME[WD]: remove
xliftAstApp2 :: (forall m. IRBuilding m => t1 -> t2                   -> m SomeExpr) -> AsgBldr t1 -> AsgBldr t2 -> IRB SomeExpr
xliftAstApp2 = liftAstApp2



-----------------------------
-- === AST Combinators === --
-----------------------------

-- === CodeSpan === --

attachCodeSpanLayer :: CodeSpan -> IRB (Expr l) -> IRB (Expr l)
attachCodeSpanLayer s = withIRx (>>~ flip (putLayer @CodeSpan) s)

spanned :: SymParser a -> SymParser (CodeSpan, a)
spanned p = phantomSpan p' where
    p' = do foStart <- unwrap <$> get @FileOffset
            put @CodeSpanRange $ wrap foStart
            p

-- | Function `phantomSpan` do not register it's beginning as new element start.
--   It is a very rarely needed functionality, use with care.
phantomSpan :: SymParser a -> SymParser (CodeSpan, a)
phantomSpan p = do
    range   <- unwrap <$> get @CodeSpanRange
    foStart <- unwrap <$> get @FileOffset
    marker  <- getLastTokenMarker
    out     <- p
    foEnd   <- unwrap <$> get @FileOffset
    sfxOff  <- getLastOffset
    let end       = foEnd   - sfxOff
        off       = foStart - range
        emptySpan = leftSpacedSpan mempty mempty
        (rs,vs)   = (realSpan, viewSpan)
        -- FIXME[WD]: The `foo` and `bar` helpers are here just to make it work with empty spans in list sections / empty module headers.
        --            We should think how to refactor them and describe better where they originate from.
        foo       = max 0 (end - foStart)
        bar       = max end foStart
        realSpan  = leftSpacedSpan off foo
        viewSpan  = case marker of
            Nothing -> realSpan
            Just m  -> leftSpacedSpan (off - m ^. Lexer.span - m ^. Lexer.offset) foo
    put @CodeSpanRange $ wrap bar
    return (CodeSpan.CodeSpan rs vs, out)

spanOf :: SymParser a -> SymParser a
spanOf = fmap snd . spanned

inheritCodeSpan1 :: (SomeExpr -> IRB SomeExpr) -> AsgBldr SomeExpr -> AsgBldr SomeExpr
inheritCodeSpan1 = inheritCodeSpan1With id

inheritCodeSpan2 :: (SomeExpr -> SomeExpr -> IRB SomeExpr) -> AsgBldr SomeExpr -> AsgBldr SomeExpr -> AsgBldr SomeExpr
inheritCodeSpan2 = inheritCodeSpan2With CodeSpan.concat

inheritCodeSpan1With :: (CodeSpan -> CodeSpan) -> (SomeExpr -> IRB SomeExpr) -> AsgBldr SomeExpr -> AsgBldr SomeExpr
inheritCodeSpan1With sf f (AsgBldr (IRB irb1)) = wrap $ IRB $ do
    t1 <- irb1
    s1 <- getLayer @CodeSpan t1
    putLayer @CodeSpan t1 (CodeSpan.dropOffset s1) -- the newly constructed IR is the new parent, so it handles the left offset
    fromIRB . unwrap . buildAsgFromSpan (sf s1) $ f t1

inheritCodeSpan2With :: (CodeSpan -> CodeSpan -> CodeSpan)
                     -> (SomeExpr -> SomeExpr -> IRB SomeExpr)
                     -> AsgBldr SomeExpr -> AsgBldr SomeExpr -> AsgBldr SomeExpr
inheritCodeSpan2With sf f (AsgBldr (IRB irb1)) (AsgBldr (IRB irb2)) = wrap $ IRB $ do
    t1 <- irb1
    s1 <- getLayer @CodeSpan t1
    putLayer @CodeSpan t1 (CodeSpan.dropOffset s1) -- the newly constructed IR is the new parent, so it handles the left offset
    t2 <- irb2
    s2 <- getLayer @CodeSpan t2
    fromIRB . unwrap . buildAsgFromSpan (sf s1 s2) $ f t1 t2

-- | Magic helper. Use with care only if you really know what you do.
modifyCodeSpan :: (CodeSpan -> CodeSpan) -> AsgBldr SomeExpr -> AsgBldr SomeExpr
modifyCodeSpan f (AsgBldr (IRB irb1)) = wrap $ IRB $ do
    t1 <- irb1
    s1 <- getLayer @CodeSpan t1
    putLayer @CodeSpan t1 (f s1)
    return t1


-- === ASG preparation === --

buildAsgFromSpan :: CodeSpan -> IRB (Expr a) -> AsgBldr (Expr a)
buildAsgFromSpan = AsgBldr .: attachCodeSpanLayer

buildAsg   ::                    SymParser       (IRB (Expr a))   -> SymParser       (AsgBldr (Expr a))
buildAsgF  :: Functor f       => SymParser (f    (IRB (Expr a)))  -> SymParser (f    (AsgBldr (Expr a)))
buildAsgF2 :: Functors '[f,g] => SymParser (f (g (IRB (Expr a)))) -> SymParser (f (g (AsgBldr (Expr a))))
buildAsg   p = uncurry          buildAsgFromSpan  <$> spanned p
buildAsgF  p = uncurry (fmap  . buildAsgFromSpan) <$> spanned p
buildAsgF2 p = uncurry (fmap2 . buildAsgFromSpan) <$> spanned p



--------------------
-- === Errors === --
--------------------

invalid :: Text32 -> IRB SomeExpr
invalid txt = liftIRBApp0 $ do
    inv <- IR.invalid txt
    registerInvalid inv
    return $ generalize inv

invalidSymbol :: (Symbol -> Text32) -> AsgParser SomeExpr
invalidSymbol f = buildAsg $ invalid . f <$> anySymbol

catchParseErrors :: SymParser a -> SymParser (Either P.String a)
catchParseErrors p = withRecovery2 (pure . Left . parseErrorTextPretty) (Right <$> p)

catchInvalidWith :: HasCallStack => (LeftSpacedSpan Delta -> LeftSpacedSpan Delta) -> (AsgBldr SomeExpr -> a) -> SymParser a -> SymParser a
catchInvalidWith spanf f p = undefined -- do
    -- (span, result) <- spanned $ catchParseErrors p
    -- return $ flip fromRight result $ f . buildAsgFromSpan (spanf span) . invalid . convert



---------------------
-- === Markers === --
---------------------

markerIRB :: SymParser (MarkerId, AsgBldr SomeExpr)
markerIRB = useLastTokenMarker >>= \case
    Nothing -> expected "marker"
    Just t  -> do
        crange <- unwrap <$> get @CodeSpanRange
        foEnd  <- unwrap <$> get @FileOffset
        let markerLen  = t ^. Lexer.span
            markerOffR = t ^. Lexer.offset
            markerOffL = foEnd - crange - markerLen - markerOffR
            markerSpan = leftSpacedSpan markerOffL markerLen
        modify_ @CodeSpanRange $ wrapped .~ foEnd
        return $ (t ^. Lexer.element, buildAsgFromSpan (CodeSpan.mkPhantomSpan markerSpan) (liftIRBApp0 $ IR.marker' $ t ^. Lexer.element))

marked :: SymParser (AsgBldr SomeExpr -> AsgBldr SomeExpr)
marked = option registerUnmarkedExpr $ uncurry markedExpr <$> markerIRB where
    markedExpr mid expr = registerMarkedExpr mid . inheritCodeSpan2 (liftIRB2 IR.marked') expr

registerUnmarkedExpr ::             AsgBldr SomeExpr -> AsgBldr SomeExpr
registerMarkedExpr   :: MarkerId -> AsgBldr SomeExpr -> AsgBldr SomeExpr
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


-- === Instances === --

instance Convertible Lexer.Number Literal.Number where
    convert (Lexer.NumRep base i f e) = Literal.Number (convert base) (convert i) (convert f) (convert e)

instance Convertible Lexer.Numbase Num.Base where
    convert = \case Lexer.Dec -> Num.Dec
                    Lexer.Bin -> Num.Bin
                    Lexer.Oct -> Num.Oct
                    Lexer.Hex -> Num.Hex


----------------------------
-- === IRB Primitives === --
----------------------------

varIRB :: Name -> IRB SomeExpr
varIRB name = liftIRBApp0 $ IR.var' name

accIRB :: Name -> SomeExpr -> IRB SomeExpr
accIRB name a = liftIRB1 (flip IR.acc' name) a

accSectionIRB :: [Name] -> IRB SomeExpr
accSectionIRB name = liftIRB0 $ IR.accSection name

updateIRB :: [Name] -> SomeExpr -> SomeExpr -> IRB SomeExpr
updateIRB name = liftIRB2 $ flip IR.update' name

modifyIRB :: [Name] -> Name -> SomeExpr -> SomeExpr -> IRB SomeExpr
modifyIRB ns n = liftIRB2 $ flip (flip IR.modify' ns) n


-------------------------
-- === Identifiers === --
-------------------------

cons, var, op, wildcard :: AsgParser SomeExpr
cons     = snd <$> namedCons
var      = snd <$> namedVar
op       = snd <$> namedOp
wildcard = buildAsg $ liftIRBApp0 IR.blank' <$ symbol Lexer.Wildcard

namedCons, namedVar, namedOp, namedIdent :: SymParser (Name, AsgBldr SomeExpr)
namedCons  = mkNamedAsg IR.cons'_ consName
namedVar   = mkNamedAsg IR.var'   varName
namedOp    = mkNamedAsg IR.var'   opName
namedIdent = namedVar <|> namedCons <|> namedOp

consName, varName, opName, identName, modifierName, foreignLangName
    :: SymParser Name
consName        = convert <$> satisfy Lexer.matchCons
varName         = convert <$> satisfy Lexer.matchVar
opName          = convert <$> satisfy Lexer.matchOperator
identName       = varName <|> consName <|> opName
funcName        = varName <|> opName
modifierName    = convert <$> satisfy Lexer.matchModifier
foreignLangName = consName

previewVarName :: SymParser Name
previewVarName = do
    s <- previewNextSymbol
    maybe (unexpected . fromString $ "Expecting variable, got: " <> show s) (return . convert) $ Lexer.matchVar =<< s

specificVar, specificCons, specificOp :: Text32 -> SymParser ()
specificVar  = symbol . Lexer.Var
specificCons = symbol . Lexer.Cons
specificOp   = symbol . Lexer.Operator

-- Helpers

mkNamedAsg :: (forall m. IRBuilding m => Name -> m (Expr a)) -> SymParser Name -> SymParser (Name, AsgBldr (Expr a))
mkNamedAsg ir p = buildAsgF $ (\name -> (name, liftIRBApp0 $ ir name)) <$> p


-- Qualified names

qualVarName, qualConsName :: SymParser QualName
qualConsName = qualNameBase consName
qualVarName  = qualNameBase varName

qualNameBase :: SymParser Name -> SymParser QualName
qualNameBase p = mkQualName . convert <$> many (try $ consName <* symbol Lexer.Accessor) <*> p



----------------------
-- === Literals === --
----------------------

rawQuoteBegin, rawQuoteEnd :: SymParser ()
fmtQuoteBegin, fmtQuoteEnd :: SymParser ()
rawQuoteBegin = symbol $ Lexer.Quote Lexer.RawStr Lexer.Begin
rawQuoteEnd   = symbol $ Lexer.Quote Lexer.RawStr Lexer.End
fmtQuoteBegin = symbol $ Lexer.Quote Lexer.FmtStr Lexer.Begin
fmtQuoteEnd   = symbol $ Lexer.Quote Lexer.FmtStr Lexer.End

num :: AsgParser SomeExpr
num = buildAsg $ (\n -> liftIRBApp0 $ IR.number' (convert n)) <$> satisfy Lexer.matchNumber

list :: AsgParser SomeExpr -> AsgParser SomeExpr
list p = buildAsg $ withLocalUnreservedSymbol sep $ braced $ (\g -> liftAstApp1 IR.list' $ sequence g) <$> elems where
    missing :: AsgParser SomeExpr
    missing   = buildAsg . pure $ liftIRB0 IR.missing'
    sep       = Lexer.Operator ","
    elem      = withLocalReservedSymbol sep p
    optElem   = elem <|> missing
    bodyH     = (:) <$> elem    <*> many (separator *> optElem)
    bodyT     = (:) <$> missing <*> some (separator *> optElem)
    elems     = option mempty $ bodyH <|> bodyT
    braced p  = braceBegin *> p <* braceEnd
    separator = symbol sep

-- FIXME[WD]: This `try` should be refactored out
-- FIXME[WD]: Tuple and List parsers are too similar no to be refactored to common part. However tuples will disappear soon.
tuple :: AsgParser SomeExpr -> AsgParser SomeExpr
tuple p = try $ buildAsg $ withLocalUnreservedSymbol sep $ parensed $ (\g -> liftAstApp1 IR.tuple' $ sequence g) <$> elems where
    missing :: AsgParser SomeExpr
    missing    = buildAsg . pure $ liftIRB0 IR.missing'
    sep        = Lexer.Operator ","
    elem       = withLocalReservedSymbol sep p
    optElem    = elem <|> missing
    body       = (:) <$> optElem <*> some (separator *> optElem)
    elems      = option mempty $ body
    parensed p = groupBegin *> p <* groupEnd
    separator  = symbol sep

str :: AsgParser SomeExpr
str = rawStr <|> fmtStr

rawStr :: AsgParser SomeExpr
rawStr = buildAsg $ do
    rawQuoteBegin
    withRecovery (\e -> invalid "Invalid string literal" <$ Loc.unregisteredDropSymbolsUntil' (== (Lexer.Quote Lexer.RawStr Lexer.End)))
                 $ (\s -> liftIRBApp0 $ IR.string' $ convert s) <$> Indent.withCurrent (strBody rawQuoteEnd) -- FIXME[WD]: We're converting Text -> String here.

fmtStr :: AsgParser SomeExpr
fmtStr = buildAsg $ do
    fmtQuoteBegin
    withRecovery (\e -> invalid "Invalid string literal" <$ Loc.unregisteredDropSymbolsUntil' (== (Lexer.Quote Lexer.FmtStr Lexer.End)))
                 $ (\s -> liftIRBApp0 $ IR.string' $ convert s) <$> Indent.withCurrent (strBody fmtQuoteEnd) -- FIXME[WD]: We're converting Text -> String here.


strBody :: SymParser () -> SymParser Text32
strBody ending = segStr <|> end <|> nl where
    segStr = (<>) <$> strContent <*> strBody ending
    end    = mempty <$ ending
    nl     = Text32.cons '\n' <$ eol <*> (line <|> nl)
    line   = do Indent.indentedOrEq
                (<>) . convert . flip replicate ' ' <$> indentation <*> strBody ending

strContent :: SymParser Text32
strContent = satisfy Lexer.matchStr <|> strEsc

strEsc :: SymParser Text32
strEsc = satisfy Lexer.matchStrEsc >>= return . \case
    Lexer.CharStrEsc i             -> convert $ Char.chr i
    Lexer.NumStrEsc i              -> convert $ Char.chr i
    Lexer.SlashEsc                 -> "\\"
    Lexer.QuoteEscape Lexer.RawStr -> "\""
    Lexer.QuoteEscape Lexer.FmtStr -> "'"


-------------------------
-- === Expressions === --
-------------------------

-- === Utils === --

app, seq, unify, appFlipped, leftSection, rightSection :: AsgBldr SomeExpr -> AsgBldr SomeExpr -> AsgBldr SomeExpr
app          = inheritCodeSpan2 $ liftIRB2 IR.app'
seq          = inheritCodeSpan2 $ liftIRB2 IR.seq'
unify        = inheritCodeSpan2 $ liftIRB2 IR.unify'
leftSection  = inheritCodeSpan2 $ liftIRB2 IR.leftSection'
appFlipped   = flip . inheritCodeSpan2 $ liftIRB2 (flip IR.app')
rightSection = flip . inheritCodeSpan2 $ flip (liftIRB2 IR.rightSection')

appSides :: AsgBldr SomeExpr -> AsgBldr SomeExpr -> AsgBldr SomeExpr -> AsgBldr SomeExpr
appSides = app .: appFlipped

apps, seqs :: AsgBldr SomeExpr -> [AsgBldr SomeExpr] -> AsgBldr SomeExpr
apps = foldl app
seqs = foldl seq

nestedLam :: [AsgBldr SomeExpr] -> AsgBldr SomeExpr -> AsgBldr SomeExpr
nestedLam args body = foldr lam' body args where
    lam' :: AsgBldr SomeExpr -> AsgBldr SomeExpr -> AsgBldr SomeExpr
    lam' = inheritCodeSpan2 $ liftIRB2 IR.lam'

grouped :: AsgParser SomeExpr -> AsgParser SomeExpr
grouped p = buildAsg $ parensed $ (\g -> liftAstApp1 IR.grouped' g) <$> p where
    parensed p = groupBegin *> p <* groupEnd


-- === Metadata === --

metadata :: AsgParser SomeExpr
metadata = buildAsg $ (\t -> liftIRBApp0 $ IR.metadata' t) <$> metaContent

metaContent :: SymParser Text32
metaContent = satisfy Lexer.matchMetadata


-- === Disabled === --

possiblyDisabled :: AsgParser SomeExpr -> AsgParser SomeExpr
possiblyDisabled p = disabled p <|> p

disabled :: AsgParser SomeExpr -> AsgParser SomeExpr
disabled p = buildAsg $ (\t -> liftAstApp1 IR.disabled' t) <$ symbol Lexer.Disable <*> p


-- === Segments === --

type ExprSegment         = ExprToken (AsgBldr SomeExpr)
type ExprSegments        = NonEmpty ExprSegment
type ExprSegmentBuilder  = SegmentBuilder ExprSegment
newtype SegmentBuilder a = SegmentBuilder { runSegmentBuilder :: (Bool, Bool) -> NonEmpty a } deriving (Functor)

type ExprToken       a = Token (ExprSymbol      a)
type ExprTokenProto  a = Token (ExprSymbolProto a)
type ExprSymbol      a = Labeled SpacedName (UniSymbol Symbol.Expr    a)
type ExprSymbolProto a = Labeled SpacedName (UniSymbol Symbol.Phantom a)

instance Semigroup ExprSegmentBuilder where
    a <> b = SegmentBuilder $ \(l,r) -> runSegmentBuilder a (l,False) <> runSegmentBuilder b (False,r)

expr :: AsgParser SomeExpr
expr = possiblyDocumented $ func <|> lineExpr

lineExpr :: AsgParser SomeExpr
lineExpr = marked <*> (possiblyDisabled (valExpr <**> option id assignment)) where
    assignment = flip unify <$ symbol Lexer.Assignment <*> valExpr

valExpr :: AsgParser SomeExpr
valExpr =  buildTokenExpr =<< exprSegments

nonemptyValExpr :: AsgParser SomeExpr
nonemptyValExpr = do
    es <- exprSegments
    -- when (null es) $ unexpected "Empty expression" -- FIXME[WD]
    buildTokenExpr es

nonemptyValExprLocal :: AsgParser SomeExpr
nonemptyValExprLocal = do
    es <- exprSegmentsLocal
    -- when (null es) $ unexpected "Empty expression" -- FIXME[WD]
    buildTokenExpr es

nonSpacedValExpr :: AsgParser SomeExpr
nonSpacedValExpr = do
    es <- exprSegmentsNonSpaced
    -- when (null es) $ unexpected "Empty expression" -- FIXME[WD]
    buildTokenExpr es

nonSpacedPattern :: AsgParser SomeExpr
nonSpacedPattern = nonSpacedValExpr

concatExprSegmentBuilders :: NonEmpty ExprSegmentBuilder -> ExprSegmentBuilder
concatExprSegmentBuilders bldrs = SegmentBuilder $ \(l,r) -> case bldrs of
    (s:|[])     -> runSegmentBuilder s (l,r)
    (s:|(t:ts)) -> runSegmentBuilder s (l,False) <> runSegmentBuilder (concatExprSegmentBuilders $ t:|ts) (False,r)

exprSegments    , exprSegmentsLocal     :: SymParser ExprSegments
exprFreeSegments, exprFreeSegmentsLocal :: SymParser ExprSegmentBuilder
exprSegments          = buildExprTok <$> exprFreeSegments
exprSegmentsLocal     = buildExprTok <$> exprFreeSegmentsLocal
exprFreeSegments      = withNewLocal exprFreeSegmentsLocal
exprFreeSegmentsLocal = fmap concatExprSegmentBuilders . some' $ choice [mfixVarSeg, consSeg, wildSeg, numSeg, strSeg, opSeg, accSeg, tupleSeg, grpSeg, listSeg, lamSeg, matchseg, typedSeg, funcSeg]

exprSegmentsNonSpaced    , exprSegmentsNonSpacedLocal     :: SymParser ExprSegments
exprFreeSegmentsNonSpaced, exprFreeSegmentsNonSpacedLocal :: SymParser ExprSegmentBuilder
exprSegmentsNonSpaced          = buildExprTok <$> exprFreeSegmentsNonSpaced
exprSegmentsNonSpacedLocal     = buildExprTok <$> exprFreeSegmentsNonSpacedLocal
exprFreeSegmentsNonSpaced      = withNewLocal exprFreeSegmentsNonSpacedLocal
exprFreeSegmentsNonSpacedLocal = choice [varSeg, consSeg, wildSeg, numSeg, strSeg, tupleSeg, grpSeg, listSeg]


-- === Components === --

-- Utils
posIndependent = SegmentBuilder . const . pure . tokenx

-- FIXME[WD]: change the API to monadic one, so we can register symbols and mixfix monads could gather needed info (like var names)
--            without the need to keep the label in the term
unlabeledAtom :: a -> Labeled SpacedName (UniSymbol Symbol.Expr a)
unlabeledAtom = labeled (spaced "#unnamed#") . atom

-- Possible tokens
varSeg, consSeg, wildSeg, numSeg, strSeg, grpSeg, listSeg, tupleSeg, matchseg, lamSeg, funcSeg :: SymParser ExprSegmentBuilder
varSeg   = posIndependent . unlabeledAtom <$> var
consSeg  = posIndependent . unlabeledAtom <$> cons
wildSeg  = posIndependent . unlabeledAtom <$> wildcard
numSeg   = posIndependent . unlabeledAtom <$> num
strSeg   = posIndependent . unlabeledAtom <$> str
grpSeg   = posIndependent . unlabeledAtom <$> grouped nonemptyValExpr
listSeg  = posIndependent . unlabeledAtom <$> list  nonemptyValExprLocal
tupleSeg = posIndependent . unlabeledAtom <$> tuple nonemptyValExprLocal
matchseg = posIndependent . unlabeledAtom <$> match
lamSeg   = posIndependent . labeled (unspaced lamName) . suffix <$> lamBldr
funcSeg  = posIndependent . unlabeledAtom <$> func


mfixVarSeg :: SymParser ExprSegmentBuilder
mfixVarSeg  = do
    (span, name) <- spanned varName
    nameSet      <- lookupMultipartNames name
    let cvar = posIndependent $ unlabeledAtom $ buildAsgFromSpan span (varIRB name)
    if TreeSet.null nameSet
        then return cvar
        else -- catchInvalidWith (span <>) (pure . posIndependent . unlabeledAtom)
           {-$-} withReservedSymbols (Lexer.Var . convert <$> TreeSet.keys nameSet) -- FIXME: conversion Name -> Text
           $ do segmentToks <- exprFreeSegmentsLocal
                namedSegs   <- option mempty (parseMixfixSegments nameSet)
                if null namedSegs then return (cvar <> segmentToks) else do
                    segment <- buildTokenExpr (buildExprTok segmentToks)
                    let segments  = snd <$> namedSegs
                        nameParts = fst <$> namedSegs
                        mfixVar   = buildAsgFromSpan span (varIRB . convert $ mkMultipartName name nameParts)
                        mfixExpr  = apps (app mfixVar segment) segments
                    return . posIndependent . unlabeledAtom $ mfixExpr

opSeg :: SymParser ExprSegmentBuilder
opSeg   = do
    (before, after) <- checkOffsets
    (span, name)    <- spanned opName
    let segment (isFirst, isLast) = pure . tokenx $ operator & if
            | isSingle    -> labeled (unspaced            name) . atom
            | isUMinus    -> labeled (unspaced      uminusName) . prefix . app
            | isFirst     -> labeled (spacedNameIf after  name) . prefix . leftSection
            | isLast      -> labeled (spacedNameIf before name) . suffix . rightSection
            | symmetrical -> labeled (spacedNameIf after  name) . infixx . appSides
            | before      -> labeled (lspaced             name) . prefix . leftSection
            | otherwise   -> labeled (rspaced             name) . suffix . rightSection
            where isMinus     = name == minusName
                  isSingle    = isFirst && isLast
                  isUMinus    = isMinus && not after && (isFirst || before) && not isLast
                  operator    = buildAsgFromSpan span . varIRB $ if isUMinus then uminusName else name
                  symmetrical = before == after
    return $ SegmentBuilder segment

typedSeg :: SymParser ExprSegmentBuilder
typedSeg   = do
    (off, off') <- checkOffsets
    let typedExpr = if off then valExpr else nonSpacedValExpr
    tp <- symbol Lexer.Typed *> typedExpr
    when (off /= off') $ error "TODO: before and after offsets have to match"
    let seg :: AsgBldr SomeExpr -> AsgBldr SomeExpr
        seg = flip (inheritCodeSpan2 $ liftIRB2 IR.typed') tp
    return . posIndependent . labeled (spacedNameIf off typedName) $ suffix seg

accSectNames :: SymParser (NonEmpty (CodeSpan, Name))
accSectNames = do
    sname <- spanned $ symbol Lexer.Accessor *> identName
    (beforeDot, afterDot) <- checkOffsets
    (sname :|) <$> if beforeDot || afterDot then pure   mempty
                                            else option mempty (convert <$> accSectNames)



accSeg :: SymParser ExprSegmentBuilder
accSeg = fmap2 tokenx $ do
    (beforeDot, afterDot) <- checkOffsets
    snames@(n :| ns) <- accSectNames
    mupdt <- option Nothing (Just <$ symbol Lexer.Assignment <*> nonemptyValExpr)
    mmod  <- option Nothing (Just .: (,) <$> modifierName <*> nonemptyValExpr)
    let (spans, names) = unzip $ convert snames
        symmetrical    = beforeDot == afterDot
        accCons s n    = inheritCodeSpan1With (<> s) (accIRB n)
        accSect        = labeled uname . atom $ buildAsgFromSpan (mconcat spans) (accSectionIRB names)
        uname          = unspaced accName
        sname          = spacedNameIf beforeDot accName
        accConss       =  labeled sname ( suffix $ uncurry accCons n )
                      :| (labeled uname . suffix . uncurry accCons <$> ns)
        Just fupdt           = mupdt -- FIXME[WD]: make it nicer
        Just (modName, fmod) = mmod  -- FIXME[WD]: make it nicer

        -- FIXME[WD]: make it nicer vvv
        updateAtom = labeled sname . suffix
                   $ flip (inheritCodeSpan2With (<>) (updateIRB names))
                          (modifyCodeSpan (CodeSpan.asOffsetSpan (mconcat spans) <>) fupdt)
        modifyAtom = labeled sname . suffix
                   $ flip (inheritCodeSpan2With (<>) (modifyIRB names modName))
                          (modifyCodeSpan (CodeSpan.asOffsetSpan (mconcat spans) <>) fmod)


    -- INFO: The following code contains hack allowing for fields updates and modifications that are not nested
    --       e.g. `a' = a.x = 5`, but does not support `a' = a.foo.x = 5`. If we use the above definition instead
    --       all nested use cases will be supported in parser, but need to be supported in backend too.
    --       Using this hack, the above definition is translated in parser to `a' = a.x= 5`, where `x=` is method name!
    -- vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

    let (nameSpan, name) = n
        updateAtomHack   = pure $ labeled sname ( suffix $ \x -> app (accCons nameSpan (name <> "=") x) fupdt )
        modifyAtomHack   = pure $ labeled sname ( suffix $ \x -> app (accCons nameSpan (name <> modName <> "=") x) fmod )

    when (isJust mupdt && not (null ns)) $ fail "Updating nested fields is not supported yet."
    when (isJust mmod  && not (null ns)) $ fail "Modification of nested fields is not supported yet."

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
    return $ SegmentBuilder segment

match :: AsgParser SomeExpr
match = buildAsg $ (\n (p,ps) -> liftAstApp2 IR.match' n (sequence $ p:ps))
      <$ symbol Lexer.KwCase <*> nonemptyValExprLocal
      <* symbol Lexer.KwOf   <*> discover (nonEmptyBlock clause)
      where clause = pattern <**> lamBldr

pattern :: AsgParser SomeExpr
pattern = withLocalReservedSymbol Lexer.BlockStart nonemptyValExprLocal


lamBldr :: SymParser (AsgBldr SomeExpr -> AsgBldr SomeExpr)
lamBldr = do
    body <- symbol Lexer.BlockStart *> discover (nonEmptyBlock lineExpr)
    return $ flip (inheritCodeSpan2 $ liftIRB2 IR.lam') (uncurry seqs body)


-- === Utils === --

buildExprTok :: ExprSegmentBuilder -> ExprSegments
buildExprTok bldr = runSegmentBuilder bldr (True, True)

parseMixfixSegments :: SparseTreeSet Name -> SymParser [(Name, AsgBldr SomeExpr)]
parseMixfixSegments nameSet = do
    name <- previewVarName
    let total = TreeSet.check' name nameSet
    case nameSet ^? ix name of
        Nothing       -> unexpected . fromString $ "Identifier `" <> convert name <> "` is not part of a mutli-name expression"
        Just nameSet' -> do
            let possiblePaths = TreeSet.keys nameSet'
            dropNextToken
            segment <- withReservedSymbols (Lexer.Var . convert <$> possiblePaths) nonemptyValExpr -- FIXME: conversion Name -> Text
            let restMod = if total then option mempty else (<|> unexpected (fromString $ "Unexpected end of mixfix expression, expecting one of " <> show possiblePaths))
            ((name,segment):) <$> restMod (parseMixfixSegments nameSet')

buildTokenExpr (s:|ss) = buildTokenExpr' (s:ss)

buildTokenExpr' :: Tokens (Labeled SpacedName (UniSymbol Symbol.Expr (AsgBldr SomeExpr))) -> SymParser (AsgBldr SomeExpr)
buildTokenExpr' = buildExpr_termApp . Labeled (spaced appName) $ Tok.Symbol app


---------------------------
-- === Documentation === --
---------------------------

possiblyDocumented :: AsgParser SomeExpr -> AsgParser SomeExpr
possiblyDocumented p = documented p <|> p

documented :: AsgParser SomeExpr -> AsgParser SomeExpr
documented p = buildAsg $ (\d t -> liftAstApp1 (IR.documented' d) t) <$> doc <*> p

doc :: SymParser Text32
doc = intercalate "\n" <$> some (satisfy Lexer.matchDocComment <* eol)


--------------------------
-- === Declarations === --
--------------------------

topLvlDecl :: AsgParser SomeExpr
topLvlDecl = possiblyDocumented $ rootedFunc <|> cls <|> metadata


-- === Functions === --

func :: AsgParser SomeExpr
func = buildAsg $ funcHdr <**> (funcDef <|> funcSig) where
    funcDef, funcSig :: SymParser (AsgBldr SomeExpr -> IRB SomeExpr)
    funcHdr = symbol Lexer.KwDef *> (var <|> op)
    funcSig = (\tp name -> liftAstApp2 IR.functionSig' name tp) <$ symbol Lexer.Typed <*> valExpr
    funcDef = (\args body name -> liftAstApp3 IR.asgFunction' name (sequence args) (uncurry seqs body))
        <$> many nonSpacedPattern
        <*  symbol Lexer.BlockStart
        <*> discover (nonEmptyBlock lineExpr)

rootedFunc :: AsgParser SomeExpr
rootedFunc = marked <*> rootedRawFunc

-- ======================================================
-- !!! Very hacky implementation of rooted function, which duplicates its name inside rooted IR's function definition
--     Moreover, function signature is not in rooted IR.
--     To be removed as soon as possible
-- vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

funcBase :: SymParser (AsgBldr SomeExpr, (Int, AsgBldr SomeExpr))
funcBase = buildAsgF2 $ do
    hdr <- funcHdr
    (\f -> (hdr, f hdr)) <$> (fmap2 (0,) funcDef <|> fmap2 (1,) funcSig)
    where
    funcDef, funcSig :: SymParser (AsgBldr SomeExpr -> IRB SomeExpr)
    funcHdr = symbol Lexer.KwDef *> (var <|> op)
    funcSig = (\tp name -> liftAstApp2 IR.functionSig' name tp) <$ symbol Lexer.Typed <*> valExpr
    funcDef = (\args body name -> liftAstApp3 IR.asgFunction' name (sequence args) (uncurry seqs body))
        <$> many nonSpacedPattern
        <*  symbol Lexer.BlockStart
        <*> discover (nonEmptyBlock lineExpr)

rootedRawFunc :: AsgParser SomeExpr
rootedRawFunc = buildAsg $ funcBase >>= \case
    (n,(0,bldr))         -> return $ liftAstApp2 IR.asgRootedFunction' n (snapshotRooted bldr)
    (_,(1,AsgBldr bldr)) -> return bldr

-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


-- === Classes == --

cls :: AsgParser SomeExpr
cls = buildAsg $ (\nat n args (cs,ds) -> liftAstApp3 (IR.clsASG' nat n) (sequence args) (sequence cs) (sequence ds))
   <$> try (option False (True <$ symbol Lexer.KwNative) <* symbol Lexer.KwClass) <*> consName <*> many var <*> body
    where body      = option mempty $ symbol Lexer.BlockStart *> bodyBlock
          funcBlock = optionalBlockBody (possiblyDocumented rootedFunc)
          consBlock = breakableNonEmptyBlockBody' clsRec <|> breakableOptionalBlockBody recNamedFieldLine
          bodyBlock = discoverIndent ((,) <$> consBlock <*> funcBlock)

clsRec :: AsgParser SomeExpr
clsRec = buildAsg $ (\n fields -> liftAstApp1 (IR.recASG' n) (sequence fields)) <$> consName <*> (blockDecl <|> inlineDecl) where
    blockDecl  = symbol Lexer.BlockStart *> discover (nonEmptyBlock' recNamedFieldLine)
    inlineDecl = many unnamedField

recNamedFieldLine :: AsgParser SomeExpr
recNamedFieldLine = buildAsg $ (\names tp -> liftAstApp1 (IR.fieldASG' names) tp) <$> many varName <* symbol Lexer.Typed <*> valExpr

unnamedField :: AsgParser SomeExpr
unnamedField = buildAsg $ (\a -> liftAstApp1 (IR.fieldASG' mempty) a) <$> nonSpacedValExpr



--------------------
-- === Units === --
--------------------

-- === Imports === --

impHub :: AsgParser SomeExpr
impHub = buildAsg $ (\(imps :: [AsgBldr SomeExpr]) -> liftAstApp1 unresolvedImpHub' (sequence imps :: AsgBldr [SomeExpr])) <$> impHeader

impHeader :: SymParser [AsgBldr SomeExpr]
impHeader = option mempty $ breakableOptionalBlockTop imp -- <* skipEmptyLines

imp :: AsgParser SomeExpr
imp = buildAsg $ (\a tgts -> liftAstApp1 (flip IR.unresolvedImp' tgts) a) <$ symbol Lexer.KwImport <*> impSrc <*> impTgt where
    impTgt  = option Import.Everything $ symbol Lexer.BlockStart *> listTgt
    listTgt = Import.Listed <$> many (varName <|> consName)

impSrc :: AsgParser SomeExpr
impSrc = buildAsg $ (\s -> liftIRBApp0 $ IR.unresolvedImpSrc' s) <$> (wrd <|> rel <|> abs) where
    wrd = Import.World    <$  specificCons "World"
    rel = Import.Relative <$  symbol Lexer.Accessor <*> qualConsName
    abs = Import.Absolute <$> qualConsName

------------------------------------
-- === Foreign Import Parsing === --
------------------------------------

foreignImportList :: AsgParser SomeExpr
foreignImportList = buildAsg $
    (\lang imports ->
        liftAstApp1 (IR.foreignImpList' lang) (sequence imports))
    <$  (symbol Lexer.KwForeign *> symbol Lexer.KwImport)
    <*> foreignLangName
    <*  symbol Lexer.BlockStart
    <*> discover (nonEmptyBlock' foreignLocationImportList)

foreignLocationImportList :: AsgParser SomeExpr
foreignLocationImportList = buildAsg $
    (\loc imports ->
        liftAstApp2 IR.foreignLocationImpList' loc (sequence imports))
    <$> stringOrVarName
    <*  symbol Lexer.BlockStart
    <*> discover (nonEmptyBlock' foreignSymbolImport)

foreignSymbolImport :: AsgParser SomeExpr
foreignSymbolImport = buildAsg $ withRecovery recover
    $   try (foreignSymbolImportWithSafety defaultFISafety)
    <|> foreignSymbolImportWithSafety specifiedFISafety
    where recover e = invalid "Invalid safety specification."
                      <$ Loc.unregisteredDropSymbolsUntil
                      (`elem` [Lexer.ETX, Lexer.EOL])

foreignSymbolImportWithSafety :: AsgParser SomeExpr -> IRParser SomeExpr
foreignSymbolImportWithSafety safe =
    (\safety forName localName importType ->
        liftAstApp3 (foreignSymbolProxy localName) safety forName importType)
    <$> safe <*> stringOrVarName <*> funcName <*  symbol Lexer.Typed <*> valExpr
    where foreignSymbolProxy a b c d = IR.foreignSymbolImp' b c a d

defaultFISafety :: AsgParser SomeExpr
defaultFISafety = buildAsg $
    (\safety -> liftIRBApp0 (IR.foreignImpSafety' safety))
    <$> ((return Import.Default) :: SymParser ForeignImportType)

-- TODO [Ara, WD] Need to have the lexer deal with these as contextual keywords
-- using a positive lookahead in the _Lexer_.
specifiedFISafety :: AsgParser SomeExpr
specifiedFISafety = buildAsg $
    (\(importSafety :: ForeignImportType) ->
        liftIRBApp0 (IR.foreignImpSafety' importSafety))
    <$> getImpSafety (optionMaybe varName)
    where getImpSafety :: SymParser (Maybe Name) -> SymParser ForeignImportType
          getImpSafety p = p >>= \case
              Nothing               -> return Import.Default
              Just (Name.Name text) -> case text of
                  "safe"   -> return Import.Safe
                  "unsafe" -> return Import.Unsafe
                  _        -> fail "Invalid safety specification."

stringOrVarName :: AsgParser SomeExpr
stringOrVarName = str <|> (asgNameParser varName)

asgNameParser :: SymParser Name -> AsgParser SomeExpr
asgNameParser nameParser = buildAsg $ (\varN -> liftIRBApp0 (IR.var' varN))
     <$> nameParser

-- === Unit body === --

unit' :: AsgParser SomeExpr
unit  :: AsgParser (Expr Unit)
unit' = generalize <<$>> unit
unit  = buildAsg $
    (\imps cls
        -> unsafeGeneralize <$> xliftAstApp2 (flip IR.unit' []) imps cls)
        <$ spacing <*> (foreignImportList <|> impHub) <*> unitCls <* spacing
    where spacing = many eol

unitCls :: AsgParser SomeExpr
unitCls = buildAsg $ (\ds -> liftAstApp1 (IR.clsASG' False "" [] []) (sequence ds)) <$> optionalBlockTop topLvlDecl

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



--------------------
-- === Parser === --
--------------------

type FileName = Literal.String


type ParsingPassReq2 m = Req m '[ Editor // Net   // '[AnyExpr, AnyExprLink]
                               , Editor  // Layer // AnyExpr // CodeSpan
                               , Reader  // Attr  // Source
                               , Writer  // Attr  // '[ParsedExpr, MarkedExprMap]
                               , Editor  // Attr  // Invalids
                               , Emitter // New   // '[AnyExpr, AnyExprLink]

                               -- codegen & codespan testing
                               , Reader // Layer // AnyExprLink // Model
                               , Reader // Layer // AnyExpr     // Model
                               , Reader // Layer // AnyExpr     // Succs
                               ]


type ParsingPassReq_2 m = Req m '[ Editor // Net   // '[AnyExpr, AnyExprLink]
                                 , Editor // Layer // AnyExpr // CodeSpan
                                 , Editor // Attr  // Invalids
                                 , Emitter // New   // '[AnyExpr, AnyExprLink]

                                 -- codegen & codespan testing
                                 , Reader // Layer // AnyExprLink // Model
                                 , Reader // Layer // AnyExpr     // Model
                                 , Reader // Layer // AnyExpr     // Succs
                                 ]




parsingPassM :: (MonadPassManager m, ParsingPassReq2 m) => AsgParser SomeExpr -> m ()
parsingPassM p = do
    src <- getAttr @Source
    (ref, gidMap) <- parsingBase p (convert src)
    putAttr @ParsedExpr    $ wrap ref
    putAttr @MarkedExprMap $ gidMap


parsingBase :: ( MonadPassManager m, ParsingPassReq_2 m
               , UnsafeGeneralizable a (Expr Draft), UnsafeGeneralizable a SomeExpr -- FIXME[WD]: Constraint for testing only
               ) => AsgParser a -> Text32 -> m (a, MarkedExprMap)
parsingBase p src = do
    let stream = Lexer.evalDefLexer src
    result <- runParserT (stx *> p <* etx) stream
    case result of
        Left e -> error ("Parser error: " <> parseErrorPretty e) -- FIXME[WD]: handle it the proper way
        Right (AsgBldr (IRB irb)) -> do
            ((ref, unmarked), gidMap) <- runDefStateT @MarkedExprMap $ runDefStateT @UnmarkedExprs irb
            return (ref, gidMap)

parsingBase_ :: ( MonadPassManager m, ParsingPassReq_2 m
                , UnsafeGeneralizable a (Expr Draft), UnsafeGeneralizable a SomeExpr -- FIXME[WD]: Constraint for testing only
                ) => AsgParser a -> Text32 -> m a
parsingBase_ = view _1 .:. parsingBase

parserPassX  :: MonadPassManager m => AsgParser SomeExpr -> Pass Parsing   m
parserPassX  = parsingPassM

reparserPass :: MonadPassManager m => AsgParser SomeExpr -> Pass Reparsing m
reparserPass p = do
    -- Reading previous analysis
    gidMapOld <- getAttr @MarkedExprMap
    refOld    <- getAttr @ParsedExpr

    -- parsing new file and updating updated analysis
    putAttr @MarkedExprMap mempty
    parsingPassM p
    gidMap    <- getAttr @MarkedExprMap
    ref       <- getAttr @ParsedExpr

    -- Preparing reparsing status
    rs        <- cmpMarkedExprMaps gidMapOld gidMap
    putAttr @ReparsingStatus (wrap rs)

runParserT :: MonadIO m => SymParser a -> Stream -> m (Either (ParseError Tok Error) a)
runParserT p s = flip runParserInternal s
               $ evalDefStateT @CodeSpanRange
               $ evalDefStateT @Reservation
               $ evalDefStateT @Scope
               $ evalDefStateT @LeftSpanner
               $ evalDefStateT @MarkerState
               $ evalDefStateT @Position
               $ evalDefStateT @FileOffset
               $ evalDefStateT @Indent
               $ hardcode >> p

runParserInternal :: MonadIO m => ParserBase2 a -> Stream -> m (Either (ParseError Tok Error) a)
runParserInternal p s = liftIO $ Parser.runParserT p "" s


cmpMarkedExprMaps :: IsomorphicCheckCtx m => MarkedExprMap -> MarkedExprMap -> m [ReparsingChange]
cmpMarkedExprMaps (oldMap'@(_unwrap -> oldMap)) (_unwrap -> newMap) = (remExprs <>) <$> mapM (uncurry $ cmpMarkedExpr oldMap') newAssocs where
    newAssocs = Map.assocs newMap
    oldAssocs = Map.assocs oldMap
    remExprs  = fmap RemovedExpr . catMaybes $ (\(k,v) -> if_ (not $ Map.member k newMap) (Just v)) <$> oldAssocs

cmpMarkedExpr :: IsomorphicCheckCtx m => MarkedExprMap -> MarkerId -> SomeExpr -> m ReparsingChange
cmpMarkedExpr (_unwrap -> map) mid newExpr = case map ^. at mid of
    Nothing      -> return $ AddedExpr newExpr
    Just oldExpr -> checkIsoExpr (unsafeGeneralize oldExpr) (unsafeGeneralize newExpr) <&> \case -- FIXME [WD]: remove unsafeGeneralize, we should use Expr Draft / SomeExpr everywhere
        False -> ChangedExpr   oldExpr newExpr
        True  -> UnchangedExpr oldExpr newExpr
