{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE UndecidableInstances      #-}

module Luna.Syntax.Text.Parser.Parsing where

import Prologue hiding (seq)

-- import Prologue_old hiding (Cons, String, Type, Symbol, UniSymbol, (|>), (<|), cons, seq, span, op)
-- import qualified Prologue_old as P

-- import qualified Text.Megaparsec as Parser
import Text.Megaparsec       (ErrorItem (Tokens), ParseError, anyChar, between,
                              char, choice, digitChar, hidden, letterChar,
                              lookAhead, lowerChar, manyTill, notFollowedBy,
                              skipMany, spaceChar, try, unexpected, upperChar,
                              withRecovery)
import Text.Megaparsec.Error (parseErrorPretty, parseErrorTextPretty)
import Text.Megaparsec.Prim  (MonadParsec)
-- import           Text.Megaparsec.String hiding (Parser)
-- import qualified Text.Megaparsec.String as M
-- import qualified Text.Megaparsec.Lexer as Lex
-- import qualified Text.Megaparsec.Char  as Charhiding (eol)

-- import qualified Luna.IR as IR
-- import           Luna.IR hiding (typed, unify, accSection, sectionLeft, sectionRight, unit', match, list, tuple, clause, Atom, State, IRB, number, string, var, expr, app, acc, grouped, blank, get, put, modify_, cons, lam, seq, function, withIR, fieldLens, clsASG, unit, imp, impSrc, impHub, invalid, marker, marked, metadata, disabled, documented)
-- import Luna.IR.ToRefactor
-- import OCI.Pass.Class hiding (get, put, modify_)
-- import OCI.Pass.Definition

-- import Luna.Syntax.Text.Layer.Loc

-- import Luna.Syntax.Text.Parser.Class
-- import Luna.Syntax.Text.Parser.Parser
-- import qualified Luna.Syntax.Text.Parser.CodeSpan as CodeSpan

-- import qualified Luna.IR.Term.Function as Function

-- import qualified Data.Text.Span as Span
-- import           Data.Text.Span (LeftSpacedSpan, leftSpacedSpan)
-- import System.Log (MonadLogging)
-- import qualified Data.Char as Char
-- import Data.List.NonEmpty (NonEmpty((:|)))
-- import Text.Megaparsec.Ext

-- import           Data.Set (Set)
-- import qualified Data.Map as Map
-- import           Data.Map (Map)
-- import qualified Control.Monad.State as S
-- import           Text.Parser.Expr hiding (PrecRelMap)
-- import Luna.Syntax.Text.Scope

-- import System.IO.Unsafe (unsafePerformIO)
-- import qualified Luna.Syntax.Text.Pretty.Pretty as CodeGen
-- import qualified Language.Symbol.Operator.Assoc as Assoc
-- import qualified Language.Symbol.Operator.Prec  as Prec
-- import           Language.Symbol hiding (Expr, symbol, Symbol, ExprSymbol)
-- import qualified Language.Symbol as Symbol
-- import qualified Language.Symbol as Tok
-- import           Luna.Syntax.Text.Parser.Hardcoded
-- import qualified OCI.Pass        as Pass
-- import Data.Container.Mono hiding (empty)
-- import           OCI.IR.Name.Qualified (QualName, mkQualName)
-- import qualified OCI.IR.Name.Path      as Name
-- import qualified OCI.IR.Name.Multipart as MName
-- import           OCI.IR.Name.Multipart (mkMultipartName)
-- import qualified Luna.IR.Term.Literal  as Literal
-- import qualified Luna.IR.Term.Literal  as Num
-- import Control.Monad.State.Dependent
-- import Text.Parser.Combinators
-- import Data.Text.Position
-- import Text.Parser.Char hiding (eol)
-- import qualified Text.Parser.Char as Char
-- import Luna.Syntax.Text.Parser.Name

-- import qualified System.Environment as Env
-- import Luna.Syntax.Text.Source
-- import Luna.IR.Analysis (IsomorphicCheckCtx, checkIsoExpr)
-- import qualified Luna.IR.Term.Unit as Import
-- import OCI.IR.Layout.Typed (type (>>))
-- import qualified Control.Monad as M


-- import qualified Control.Monad as Monad

-- import           Luna.Syntax.Text.Parser.Loc (LeftSpanner, MonadLoc)


-- import Luna.Syntax.Text.Parser.Errors (Invalids, registerInvalid)
-- import qualified Data.List as List
-- import           Luna.Syntax.Text.Parser.Reserved (Reservation, withReservedSymbols, withLocalReservedSymbol, withLocalUnreservedSymbol, withNewLocal, checkForSymbolReservation)
import Data.TreeSet                  (SparseTreeSet)
import Luna.Syntax.Text.Parser.Class (Stream)

-- import           Data.Text32 (Text32)
-- import qualified Data.Text32 as Text32
-- import qualified Data.Text as Text
import Text.Parser.Combinators

import qualified Control.Monad.State.Layered       as State
import qualified Data.Char                         as Char
import qualified Data.Set                          as Set
import qualified Data.Text.Span                    as Span
import qualified Data.Text32                       as Text32
import qualified Data.TreeSet                      as TreeSet
import qualified Language.Symbol                   as Symbol
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

import Data.Text.Position (FileOffset (..))
-- import           Text.Parser.Indent (Indent, indentation)
import Data.Text.Position               (Delta)
import Data.Text32                      (Text32)
import Language.Symbol                  (Labeled (Labeled), UniSymbol, labeled)
import Luna.IR                          (Term)
import Luna.Pass                        (Pass)
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
import Luna.Syntax.Text.Parser.Parser   (AsgBldr (AsgBldr, fromAsgBldr),
                                         AsgParser, IRB, Parser, SymParser,
                                         runParserT, withAsgBldr)
import OCI.Data.Name                    (Name)
import Text.Megaparsec.Ext              (expected)

-- TODO: Can we do better?
instance Convertible Text32.Text32 Name where
    convert = convertVia @String ; {-# INLINE convert #-}

type SomeTerm = IR.Term ()

some' :: (Applicative m, Alternative m) => m a -> m (NonEmpty a)
some' p = (:|) <$> p <*> many p


-- ------------------------
-- -- === Primitives === --
-- ------------------------

satisfy' :: (Lexer.Symbol -> Bool)    -> SymParser Lexer.Symbol
satisfy_ :: (Lexer.Symbol -> Bool)    -> SymParser ()
satisfy  :: (Lexer.Symbol -> Maybe a) -> SymParser a
satisfy_ f = void $ satisfy' f
satisfy' f = satisfy $ \s -> justIf (f s) s
satisfy  f = token' testSymbol Nothing where
    testSymbol r t = case Reserved.lookupSymbolReservation r (t ^. Lexer.element) of
        True  -> Left (Set.singleton (Tokens (t:|[])), Set.empty, Set.empty)
        False -> case f (t ^. Lexer.element) of
            Just a  -> Right a
            Nothing -> Left (Set.singleton (Tokens (t:|[])), Set.empty, Set.empty)

satisfyReserved' :: (Lexer.Symbol -> Bool)    -> SymParser Lexer.Symbol
satisfyReserved_ :: (Lexer.Symbol -> Bool)    -> SymParser ()
satisfyReserved  :: (Lexer.Symbol -> Maybe a) -> SymParser a
satisfyReserved_ f = void $ satisfyReserved' f
satisfyReserved' f = satisfyReserved $ \s -> justIf (f s) s
satisfyReserved  f = token' testSymbol Nothing where
    testSymbol r t = case f (t ^. Lexer.element) of
        Just a  -> Right a
        Nothing -> Left (Set.singleton (Tokens (t:|[])), Set.empty, Set.empty)

symbol :: Lexer.Symbol -> SymParser ()
symbol = satisfy_ . (==)

anySymbol :: SymParser Lexer.Symbol
anySymbol = satisfyReserved' $ const True

dropNextToken :: SymParser ()
dropNextToken = satisfyReserved_ $ const True

getLastOffset   :: State.Getter LeftSpanner m => m Delta
checkLastOffset :: State.Getter LeftSpanner m => m Bool
getLastOffset   = unwrap <$> State.get @LeftSpanner
checkLastOffset = (>0)   <$> getLastOffset

checkOffsets :: (MonadParsec e Stream m, State.Getter LeftSpanner m) => m (Bool, Bool)
checkOffsets = (,) <$> checkLastOffset <*> checkNextOffset



----------------------------------
-- === AST function lifting === --
----------------------------------

-- === Lifting === --

-- liftIRB0 :: (                              Pass Parser out) -> (                              IRB out)
-- liftIRB1 :: (t1                         -> Pass Parser out) -> (t1                         -> IRB out)
-- liftIRB2 :: (t1 -> t2                   -> Pass Parser out) -> (t1 -> t2                   -> IRB out)
-- liftIRB3 :: (t1 -> t2 -> t3             -> Pass Parser out) -> (t1 -> t2 -> t3             -> IRB out)
-- liftIRB4 :: (t1 -> t2 -> t3 -> t4       -> Pass Parser out) -> (t1 -> t2 -> t3 -> t4       -> IRB out)
-- liftIRB5 :: (t1 -> t2 -> t3 -> t4 -> t5 -> Pass Parser out) -> (t1 -> t2 -> t3 -> t4 -> t5 -> IRB out)
-- liftIRB0 f                = wrap $ f                ; {-# INLINE liftIRB0 #-}
-- liftIRB1 f t1             = wrap $ f t1             ; {-# INLINE liftIRB1 #-}
-- liftIRB2 f t1 t2          = wrap $ f t1 t2          ; {-# INLINE liftIRB2 #-}
-- liftIRB3 f t1 t2 t3       = wrap $ f t1 t2 t3       ; {-# INLINE liftIRB3 #-}
-- liftIRB4 f t1 t2 t3 t4    = wrap $ f t1 t2 t3 t4    ; {-# INLINE liftIRB4 #-}
-- liftIRB5 f t1 t2 t3 t4 t5 = wrap $ f t1 t2 t3 t4 t5 ; {-# INLINE liftIRB5 #-}

liftIRBApp1 :: (t1                         -> IRB out) -> IRB t1 -> IRB out
liftIRBApp2 :: (t1 -> t2                   -> IRB out) -> IRB t1 -> IRB t2 -> IRB out
liftIRBApp3 :: (t1 -> t2 -> t3             -> IRB out) -> IRB t1 -> IRB t2 -> IRB t3 -> IRB out
liftIRBApp4 :: (t1 -> t2 -> t3 -> t4       -> IRB out) -> IRB t1 -> IRB t2 -> IRB t3 -> IRB t4 -> IRB out
liftIRBApp5 :: (t1 -> t2 -> t3 -> t4 -> t5 -> IRB out) -> IRB t1 -> IRB t2 -> IRB t3 -> IRB t4 -> IRB t5 -> IRB out
liftIRBApp1 f mt1                 = do { t1 <- mt1; f t1                                                         } ; {-# INLINE liftIRBApp1 #-}
liftIRBApp2 f mt1 mt2             = do { t1 <- mt1; t2 <- mt2; f t1 t2                                           } ; {-# INLINE liftIRBApp2 #-}
liftIRBApp3 f mt1 mt2 mt3         = do { t1 <- mt1; t2 <- mt2; t3 <- mt3; f t1 t2 t3                             } ; {-# INLINE liftIRBApp3 #-}
liftIRBApp4 f mt1 mt2 mt3 mt4     = do { t1 <- mt1; t2 <- mt2; t3 <- mt3; t4 <- mt4; f t1 t2 t3 t4               } ; {-# INLINE liftIRBApp4 #-}
liftIRBApp5 f mt1 mt2 mt3 mt4 mt5 = do { t1 <- mt1; t2 <- mt2; t3 <- mt3; t4 <- mt4; t5 <- mt5; f t1 t2 t3 t4 t5 } ; {-# INLINE liftIRBApp5 #-}

-- liftAstApp0 :: (                              IRB out) -> IRB out
liftAstApp1 :: (t1                         -> IRB out) -> AsgBldr t1 -> IRB out
liftAstApp2 :: (t1 -> t2                   -> IRB out) -> AsgBldr t1 -> AsgBldr t2 -> IRB out
liftAstApp3 :: (t1 -> t2 -> t3             -> IRB out) -> AsgBldr t1 -> AsgBldr t2 -> AsgBldr t3 -> IRB out
liftAstApp4 :: (t1 -> t2 -> t3 -> t4       -> IRB out) -> AsgBldr t1 -> AsgBldr t2 -> AsgBldr t3 -> AsgBldr t4 -> IRB out
liftAstApp5 :: (t1 -> t2 -> t3 -> t4 -> t5 -> IRB out) -> AsgBldr t1 -> AsgBldr t2 -> AsgBldr t3 -> AsgBldr t4 -> AsgBldr t5 -> IRB out
-- liftAstApp0 f                = id f                                                                                      ; {-# INLINE liftAstApp0 #-}
liftAstApp1 f t1             = liftIRBApp1 f (fromAsgBldr t1)                                                                     ; {-# INLINE liftAstApp1 #-}
liftAstApp2 f t1 t2          = liftIRBApp2 f (fromAsgBldr t1) (fromAsgBldr t2)                                                    ; {-# INLINE liftAstApp2 #-}
liftAstApp3 f t1 t2 t3       = liftIRBApp3 f (fromAsgBldr t1) (fromAsgBldr t2) (fromAsgBldr t3)                                   ; {-# INLINE liftAstApp3 #-}
liftAstApp4 f t1 t2 t3 t4    = liftIRBApp4 f (fromAsgBldr t1) (fromAsgBldr t2) (fromAsgBldr t3) (fromAsgBldr t4)                  ; {-# INLINE liftAstApp4 #-}
liftAstApp5 f t1 t2 t3 t4 t5 = liftIRBApp5 f (fromAsgBldr t1) (fromAsgBldr t2) (fromAsgBldr t3) (fromAsgBldr t4) (fromAsgBldr t5) ; {-# INLINE liftAstApp5 #-}

-- -- FIXME[WD]: remove
-- xliftAstApp2 :: (forall m. IRBuilding m => t1 -> t2                   -> m SomeTerm) -> AsgBldr t1 -> AsgBldr t2 -> IRB SomeTerm
-- xliftAstApp2 = liftAstApp2



-- -----------------------------
-- -- === AST Combinators === --
-- -----------------------------

-- -- === CodeSpan === --

attachCodeSpanLayer :: CodeSpan -> IRB (Term a) -> IRB (Term a)
attachCodeSpanLayer s = (>>~ flip (Layer.write @CodeSpan) s)

spanned :: SymParser a -> SymParser (CodeSpan, a)
spanned p = phantomSpan p' where
    p' = do fileOffStart <- unwrap <$> State.get @FileOffset
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

inheritCodeSpan1 :: (SomeTerm -> IRB SomeTerm) -> AsgBldr SomeTerm -> AsgBldr SomeTerm
inheritCodeSpan1 = inheritCodeSpan1With id

inheritCodeSpan2 :: (SomeTerm -> SomeTerm -> IRB SomeTerm) -> AsgBldr SomeTerm -> AsgBldr SomeTerm -> AsgBldr SomeTerm
inheritCodeSpan2 = inheritCodeSpan2With (CodeSpan.concat) -- IT WAS HERE: CodeSpan.concat

inheritCodeSpan1With :: (CodeSpan -> CodeSpan) -> (SomeTerm -> IRB SomeTerm) -> AsgBldr SomeTerm -> AsgBldr SomeTerm
inheritCodeSpan1With sf f (AsgBldr irb1) = wrap $ do
    t1 <- irb1
    s1 <- Layer.read @CodeSpan t1
    Layer.write @CodeSpan t1 (CodeSpan.dropOffset s1) -- the newly constructed IR is the new parent, so it handles the left offset
    unwrap . buildAsgFromSpan (sf s1) $ f t1

inheritCodeSpan2With :: (CodeSpan -> CodeSpan -> CodeSpan)
                     -> (SomeTerm -> SomeTerm -> IRB SomeTerm)
                     -> AsgBldr SomeTerm -> AsgBldr SomeTerm -> AsgBldr SomeTerm
inheritCodeSpan2With sf f (AsgBldr irb1) (AsgBldr irb2) = wrap $ do
    t1 <- irb1
    s1 <- Layer.read @CodeSpan t1
    -- | the new IR is the new parent, so it handles the left offset
    Layer.write @CodeSpan t1 (CodeSpan.dropOffset s1)
    t2 <- irb2
    s2 <- Layer.read @CodeSpan t2
    unwrap . buildAsgFromSpan (sf s1 s2) $ f t1 t2

-- | Magic helper. Use with care only if you really know what you do.
modifyCodeSpan :: (CodeSpan -> CodeSpan) -> AsgBldr SomeTerm -> AsgBldr SomeTerm
modifyCodeSpan f (AsgBldr irb1) = wrap $ do
    t1 <- irb1
    s1 <- Layer.read @CodeSpan t1
    Layer.write @CodeSpan t1 (f s1)
    pure t1


-- === ASG preparation === --

buildAsgFromSpan :: CodeSpan -> IRB (Term a) -> AsgBldr (Term a)
buildAsgFromSpan = AsgBldr .: attachCodeSpanLayer

buildAsg   ::                    SymParser       (IRB (Term a))   -> SymParser       (AsgBldr (Term a))
buildAsgF  :: Functor f       => SymParser (f    (IRB (Term a)))  -> SymParser (f    (AsgBldr (Term a)))
buildAsgF2 :: Functors '[f,g] => SymParser (f (g (IRB (Term a)))) -> SymParser (f (g (AsgBldr (Term a))))
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

invalidSymbol :: (Lexer.Symbol -> Text32) -> AsgParser SomeTerm
invalidSymbol f = buildAsg $ invalid . f <$> anySymbol

catchParseErrors :: SymParser a -> SymParser (Either String a)
catchParseErrors p = withRecovery2 (pure . Left . parseErrorTextPretty) (Right <$> p)

catchInvalidWith :: HasCallStack => (Span.LeftSpacedSpan -> Span.LeftSpacedSpan)
                 -> (AsgBldr SomeTerm -> a) -> SymParser a -> SymParser a
catchInvalidWith spanf f p = undefined -- do
    -- (span, result) <- spanned $ catchParseErrors p
    -- pure $ flip fromRight result $ f . buildAsgFromSpan (spanf span) . invalid . convert



-- ---------------------
-- -- === Markers === --
-- ---------------------

markerIRB :: SymParser (MarkerId, AsgBldr SomeTerm)
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

marked :: SymParser (AsgBldr SomeTerm -> AsgBldr SomeTerm)
marked = option registerUnmarkedExpr $ uncurry markedExpr <$> markerIRB where
    markedExpr mid expr = registerMarkedExpr mid . inheritCodeSpan2 (IR.marked') expr

registerUnmarkedExpr ::             AsgBldr SomeTerm -> AsgBldr SomeTerm
registerMarkedExpr   :: MarkerId -> AsgBldr SomeTerm -> AsgBldr SomeTerm
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

cons, var, op, wildcard :: AsgParser SomeTerm
cons     = snd <$> namedCons                             ; {-# INLINE cons     #-}
var      = snd <$> namedVar                              ; {-# INLINE var      #-}
op       = snd <$> namedOp                               ; {-# INLINE op       #-}
wildcard = buildAsg $ IR.blank' <$ symbol Lexer.Wildcard ; {-# INLINE wildcard #-}

namedCons, namedVar, namedOp, namedIdent :: SymParser (Name, AsgBldr SomeTerm)
namedCons  = mkNamedAsg (flip IR.cons' []) consName ; {-# INLINE namedCons  #-}
namedVar   = mkNamedAsg IR.var'            varName  ; {-# INLINE namedVar   #-}
namedOp    = mkNamedAsg IR.var'            opName   ; {-# INLINE namedOp    #-}
namedIdent = namedVar <|> namedCons <|> namedOp     ; {-# INLINE namedIdent #-}

consName, varName, opName, identName, modifierName :: SymParser Name
foreignLangName                                    :: SymParser Name
consName        = convert <$> satisfy Lexer.matchCons     ; {-# INLINE consName        #-}
varName         = convert <$> satisfy Lexer.matchVar      ; {-# INLINE varName         #-}
opName          = convert <$> satisfy Lexer.matchOperator ; {-# INLINE opName          #-}
identName       = varName <|> consName <|> opName         ; {-# INLINE identName       #-}
funcName        = varName <|> opName                      ; {-# INLINE funcName        #-}
modifierName    = convert <$> satisfy Lexer.matchModifier ; {-# INLINE modifierName    #-}
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

mkNamedAsg :: (Name -> IRB (Term a)) -> SymParser Name -> SymParser (Name, AsgBldr (Term a))
mkNamedAsg cons = buildAsgF . fmap (\name -> (name, cons name))


-- -- Qualified names

-- qualVarName, qualConsName :: SymParser QualName
-- qualConsName = qualNameBase consName
-- qualVarName  = qualNameBase varName

-- qualNameBase :: SymParser Name -> SymParser QualName
-- qualNameBase p = mkQualName . convert <$> many (try $ consName <* symbol Lexer.Accessor) <*> p



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
number :: AsgParser SomeTerm
number = buildAsg $ do
    Lexer.NumRep base i f _ <- satisfy Lexer.matchNumber
    pure $ IR.number' (convert base)
                      (convert $ convertChar <$> convertTo @String i)
                      (convert $ convertChar <$> convertTo @String f)
    where convertChar c = let ord = Char.ord c in
              if | ord >= 48 && ord <= 57  -> (fromIntegral $ ord - 48      :: Word8)
                 | ord >= 97 && ord <= 122 -> (fromIntegral $ ord - 97 + 10 :: Word8)
                 | otherwise               -> error "wrong char"


list :: AsgParser SomeTerm -> AsgParser SomeTerm
list p = buildAsg $ Reserved.withLocalUnreservedSymbol sep $ braced $ (\g -> liftAstApp1 IR.list' $ sequence g) <$> elems where
    missing :: AsgParser SomeTerm
    missing   = buildAsg . pure $ IR.missing'
    sep       = Lexer.Operator ","
    elem      = Reserved.withLocalReservedSymbol sep p
    optElem   = elem <|> missing
    bodyH     = (:) <$> elem    <*> many (separator *> optElem)
    bodyT     = (:) <$> missing <*> some (separator *> optElem)
    elems     = option mempty $ bodyH <|> bodyT
    braced p  = braceBegin *> p <* braceEnd
    separator = symbol sep

-- FIXME[WD]: This `try` should be refactored out
-- FIXME[WD]: Tuple and List parsers are too similar no to be refactored to common part. However tuples will disappear soon.
tuple :: AsgParser SomeTerm -> AsgParser SomeTerm
tuple p = try $ buildAsg $ Reserved.withLocalUnreservedSymbol sep $ parensed
        $ (liftAstApp1 IR.tuple' . sequence) <$> elems where
    missing :: AsgParser SomeTerm
    missing    = buildAsg $ pure IR.missing'
    sep        = Lexer.Operator ","
    elem       = Reserved.withLocalReservedSymbol sep p
    optElem    = elem <|> missing
    body       = (:) <$> optElem <*> some (separator *> optElem)
    elems      = option mempty $ body
    parensed p = groupBegin *> p <* groupEnd
    separator  = symbol sep

string :: AsgParser SomeTerm
string = rawStr <|> fmtStr

rawStr :: AsgParser SomeTerm
rawStr = buildAsg $ do
    rawQuoteBegin
    withRecovery (\e -> invalid "Invalid string literal" <$ Loc.unregisteredDropSymbolsUntil' (== (Lexer.Quote Lexer.RawStr Lexer.End)))
                 $ (IR.string' . convertVia @String)
               <$> Indent.withCurrent (strBody rawQuoteEnd) -- FIXME[WD]: We're converting Text -> String here.

fmtStr :: AsgParser SomeTerm
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
strContent = satisfy Lexer.matchStr <|> strEsc

strEsc :: SymParser Text32
strEsc = satisfy Lexer.matchStrEsc >>= pure . \case
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
    :: AsgBldr SomeTerm -> AsgBldr SomeTerm -> AsgBldr SomeTerm
app          = inheritCodeSpan2 IR.app'
seq          = inheritCodeSpan2 IR.seq'
unify        = inheritCodeSpan2 IR.unify'
sectionLeft  = inheritCodeSpan2 IR.sectionLeft'
appFlipped   = flip . inheritCodeSpan2 $ flip IR.app'
sectionRight = flip . inheritCodeSpan2 $ flip IR.sectionRight'

appSides :: AsgBldr SomeTerm -> AsgBldr SomeTerm -> AsgBldr SomeTerm -> AsgBldr SomeTerm
appSides = app .: appFlipped

apps, seqs :: AsgBldr SomeTerm -> [AsgBldr SomeTerm] -> AsgBldr SomeTerm
apps = foldl app
seqs = foldl seq

nestedLam :: [AsgBldr SomeTerm] -> AsgBldr SomeTerm -> AsgBldr SomeTerm
nestedLam args body = foldr lam' body args where
    lam' :: AsgBldr SomeTerm -> AsgBldr SomeTerm -> AsgBldr SomeTerm
    lam' = inheritCodeSpan2 $ IR.lam'

grouped :: AsgParser SomeTerm -> AsgParser SomeTerm
grouped p = buildAsg $ parensed $ (\g -> liftAstApp1 IR.grouped' g) <$> p where
    parensed p = groupBegin *> p <* groupEnd


-- -- === Metadata === --

-- FIXME: performance
metadata :: AsgParser SomeTerm
metadata = buildAsg $ IR.metadata' . convertVia @String <$> metaContent

metaContent :: SymParser Text32
metaContent = satisfy Lexer.matchMetadata


-- === Disabled === --

possiblyDisabled :: AsgParser SomeTerm -> AsgParser SomeTerm
possiblyDisabled p = disabled p <|> p

disabled :: AsgParser SomeTerm -> AsgParser SomeTerm
disabled p = buildAsg $ liftAstApp1 IR.disabled' <$ symbol Lexer.Disable <*> p


-- === Segments === --

type ExprSegment         = ExprToken (AsgBldr SomeTerm)
type ExprSegments        = NonEmpty ExprSegment
type ExprSegmentBuilder  = SegmentBuilder ExprSegment
newtype SegmentBuilder a = SegmentBuilder { runSegmentBuilder :: (Bool, Bool) -> NonEmpty a } deriving (Functor)

type ExprToken       a = Expr.Token (ExprSymbol      a)
type ExprTokenProto  a = Expr.Token (ExprSymbolProto a)
type ExprSymbol      a = Labeled SpacedName (UniSymbol Symbol.Expr    a)
type ExprSymbolProto a = Labeled SpacedName (UniSymbol Symbol.Phantom a)

instance Semigroup ExprSegmentBuilder where
    a <> b = SegmentBuilder $ \(l,r) -> runSegmentBuilder a (l,False) <> runSegmentBuilder b (False,r)

expr :: AsgParser SomeTerm
expr = possiblyDocumented $ lineExpr -- WAS: func <|> lineExpr

lineExpr :: AsgParser SomeTerm
lineExpr = marked <*> (possiblyDisabled (valExpr <**> option id assignment)) where
    assignment = flip unify <$ symbol Lexer.Assignment <*> valExpr

valExpr :: AsgParser SomeTerm
valExpr =  buildTokenExpr =<< exprSegments

nonemptyValExpr :: AsgParser SomeTerm
nonemptyValExpr = do
    es <- exprSegments
    -- when (null es) $ unexpected "Empty expression" -- FIXME[WD]
    buildTokenExpr es

nonemptyValExprLocal :: AsgParser SomeTerm
nonemptyValExprLocal = do
    es <- exprSegmentsLocal
    -- when (null es) $ unexpected "Empty expression" -- FIXME[WD]
    buildTokenExpr es

nonSpacedValExpr :: AsgParser SomeTerm
nonSpacedValExpr = do
    es <- exprSegmentsNonSpaced
    -- when (null es) $ unexpected "Empty expression" -- FIXME[WD]
    buildTokenExpr es

nonSpacedPattern :: AsgParser SomeTerm
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
exprFreeSegmentsLocal = fmap concatExprSegmentBuilders . some'
                      $ choice [ mfixVarSeg, consSeg, wildSeg, numSeg, strSeg
                               , opSeg, accSeg, tupleSeg, grpSeg, listSeg
                               , lamSeg, matchseg, typedSeg{-, funcSeg-}
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
unlabeledAtom :: a -> Labeled SpacedName (UniSymbol Symbol.Expr a)
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
                        mfixVar   = buildAsgFromSpan span (IR.var' . convert
                                  $ Name.Multipart.make name nameParts)
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
    let seg :: AsgBldr SomeTerm -> AsgBldr SomeTerm
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

match :: AsgParser SomeTerm
match = buildAsg $ (\n (p,ps) -> liftAstApp2 IR.match' n (sequence $ p:ps))
      <$ symbol Lexer.KwCase <*> nonemptyValExprLocal
      <* symbol Lexer.KwOf   <*> discover (nonEmptyBlock clause)
      where clause = pat <**> lamBldr

pat :: AsgParser SomeTerm
pat = Reserved.withLocalReservedSymbol Lexer.BlockStart nonemptyValExprLocal


lamBldr :: SymParser (AsgBldr SomeTerm -> AsgBldr SomeTerm)
lamBldr = do
    body <- symbol Lexer.BlockStart *> discover (nonEmptyBlock lineExpr)
    pure $ flip (inheritCodeSpan2 IR.lam') (uncurry seqs body)


-- === Utils === --

buildExprTok :: ExprSegmentBuilder -> ExprSegments
buildExprTok bldr = runSegmentBuilder bldr (True, True)

parseMixfixSegments :: SparseTreeSet Name -> SymParser [(Name, AsgBldr SomeTerm)]
parseMixfixSegments nameSet = do
    name <- previewVarName
    let total = TreeSet.check' name nameSet
    case nameSet ^? ix name of
        Nothing       -> unexpected . fromString $ "Identifier `" <> convert name <> "` is not part of a mutli-name expression"
        Just nameSet' -> do
            let possiblePaths = TreeSet.keys nameSet'
            dropNextToken
            segment <- Reserved.withReservedSymbols (Lexer.Var . convertVia @String <$> possiblePaths) nonemptyValExpr -- FIXME: conversion Name -> Text
            let restMod = if total then option mempty else (<|> unexpected (fromString $ "Unexpected end of mixfix expression, expecting one of " <> show possiblePaths))
            ((name,segment):) <$> restMod (parseMixfixSegments nameSet')

buildTokenExpr (s:|ss) = buildTokenExpr' (s:ss)

buildTokenExpr' :: Expr.Tokens (Labeled SpacedName (UniSymbol Symbol.Expr (AsgBldr SomeTerm))) -> SymParser (AsgBldr SomeTerm)
buildTokenExpr' = Expr.buildExpr_termApp . Labeled (Name.spaced Builtin.appName) $ Symbol.Symbol app


---------------------------
-- === Documentation === --
---------------------------

possiblyDocumented :: AsgParser SomeTerm -> AsgParser SomeTerm
possiblyDocumented p = documented p <|> p

-- FIXME: performance
documented :: AsgParser SomeTerm -> AsgParser SomeTerm
documented p = buildAsg $ (\d t -> liftAstApp1 (IR.documented' $ convertVia @String d) t) <$> doc <*> p

doc :: SymParser Text32
doc = intercalate "\n" <$> some (satisfy Lexer.matchDocComment <* eol)


--------------------------
-- === Declarations === --
--------------------------

topLvlDecl :: AsgParser SomeTerm
topLvlDecl = possiblyDocumented $ func <|> record <|> metadata


-- === Functions === --

func :: AsgParser SomeTerm
func = buildAsg $ funcHdr <**> (funcDef <|> funcSig) where
    funcDef, funcSig :: SymParser (AsgBldr SomeTerm -> IRB SomeTerm)
    funcHdr = symbol Lexer.KwDef *> (var <|> op)
    funcSig = (\tp name -> liftAstApp2 IR.functionSig' name tp) <$ symbol Lexer.Typed <*> valExpr
    funcDef = (\args body name -> liftAstApp3 IR.function' name (sequence args) (uncurry seqs body))
        <$> many nonSpacedPattern
        <*  symbol Lexer.BlockStart
        <*> discover (nonEmptyBlock lineExpr)


-- -- === Classes == --

record :: AsgParser SomeTerm
record = buildAsg $ (\nat n args (cs,ds) -> liftAstApp3 (IR.record' nat n) (sequence args) (sequence cs) (sequence ds))
   <$> try (option False (True <$ symbol Lexer.KwNative) <* symbol Lexer.KwClass) <*> consName <*> many var <*> body
    where body      = option mempty $ symbol Lexer.BlockStart *> bodyBlock
          funcBlock = optionalBlockBody (possiblyDocumented func)
          consBlock = breakableNonEmptyBlockBody' recordCons <|> breakableOptionalBlockBody recordNamedFields
          bodyBlock = discoverIndent ((,) <$> consBlock <*> funcBlock)

recordCons :: AsgParser SomeTerm
recordCons = buildAsg $ (\n fields -> liftAstApp1 (IR.recordCons' n) (sequence fields)) <$> consName <*> (blockDecl <|> inlineDecl) where
    blockDecl  = symbol Lexer.BlockStart *> discover (nonEmptyBlock' recordNamedFields)
    inlineDecl = many unnamedField

recordNamedFields :: AsgParser SomeTerm
recordNamedFields = buildAsg $ liftAstApp1 . IR.recordFields' . convert
                <$> many varName <* symbol Lexer.Typed <*> valExpr

unnamedField :: AsgParser SomeTerm
unnamedField = buildAsg $ liftAstApp1 (IR.recordFields' mempty)
                      <$> nonSpacedValExpr



--------------------
-- === Units === --
--------------------

-- === Imports === --

-- impHub :: AsgParser SomeTerm
-- impHub = buildAsg $ (\(imps :: [AsgBldr SomeTerm]) -> liftAstApp1 unresolvedImpHub' (sequence imps :: AsgBldr [SomeTerm])) <$> impHeader

-- impHeader :: SymParser [AsgBldr SomeTerm]
-- impHeader = option mempty $ breakableOptionalBlockTop imp -- <* skipEmptyLines

-- imp :: AsgParser SomeTerm
-- imp = buildAsg $ (\a tgts -> liftAstApp1 (flip IR.unresolvedImp' tgts) a) <$ symbol Lexer.KwImport <*> impSrc <*> impTgt where
--     impTgt  = option Import.Everything $ symbol Lexer.BlockStart *> listTgt
--     listTgt = Import.Listed <$> many (varName <|> consName)

-- impSrc :: AsgParser SomeTerm
-- impSrc = buildAsg $ (\s -> id $ IR.imp' s) <$> (wrd <|> rel <|> abs) where
--     wrd = Import.World    <$  specificCons "World"
--     rel = Import.Relative <$  symbol Lexer.Accessor <*> qualConsName
--     abs = Import.Absolute <$> qualConsName

-- ------------------------------------
-- -- === Foreign Import Parsing === --+
-- ------------------------------------

-- foreignImportList :: AsgParser SomeTerm
-- foreignImportList = buildAsg $
--     (\lang imports ->
--         liftAstApp1 (IR.foreignImpList' lang) (sequence imports))
--     <$  (symbol Lexer.KwForeign *> symbol Lexer.KwImport)
--     <*> foreignLangName
--     <*  symbol Lexer.BlockStart
--     <*> discover (nonEmptyBlock' foreignLocationImportList)

-- foreignLocationImportList :: AsgParser SomeTerm
-- foreignLocationImportList = buildAsg $
--     (\loc imports ->
--         liftAstApp2 IR.foreignLocationImpList' loc (sequence imports))
--     <$> stringOrVarName
--     <*  symbol Lexer.BlockStart
--     <*> discover (nonEmptyBlock' foreignSymbolImport)

-- foreignSymbolImport :: AsgParser SomeTerm
-- foreignSymbolImport = buildAsg $ withRecovery recover
--     $   try (foreignSymbolImportWithSafety defaultFISafety)
--     <|> foreignSymbolImportWithSafety specifiedFISafety
--     where recover e = invalid "Invalid safety specification."
--                       <$ Loc.unregisteredDropSymbolsUntil
--                       (`elem` [Lexer.ETX, Lexer.EOL])

-- foreignSymbolImportWithSafety :: AsgParser SomeTerm -> IRParser SomeTerm
-- foreignSymbolImportWithSafety safe =
--     (\safety forName localName importType ->
--         liftAstApp3 (foreignSymbolProxy localName) safety forName importType)
--     <$> safe <*> stringOrVarName <*> funcName <*  symbol Lexer.Typed <*> valExpr
--     where foreignSymbolProxy a b c d = IR.foreignSymbolImp' b c a d

-- defaultFISafety :: AsgParser SomeTerm
-- defaultFISafety = buildAsg $
--     (\safety -> id (IR.foreignImpSafety' safety))
--     <$> ((pure Import.Default) :: SymParser ForeignImportType)

-- -- TODO [Ara, WD] Need to have the lexer deal with these as contextual keywords
-- -- using a positive lookahead in the _Lexer_.
-- specifiedFISafety :: AsgParser SomeTerm
-- specifiedFISafety = buildAsg $
--     (\(importSafety :: ForeignImportType) ->
--         id (IR.foreignImpSafety' importSafety))
--     <$> getImpSafety (optionMaybe varName)
--     where getImpSafety :: SymParser (Maybe Name) -> SymParser ForeignImportType
--           getImpSafety p = p >>= \case
--               Nothing               -> pure Import.Default
--               Just (Name.Name text) -> case text of
--                   "safe"   -> pure Import.Safe
--                   "unsafe" -> pure Import.Unsafe
--                   _        -> fail "Invalid safety specification."

-- stringOrVarName :: AsgParser SomeTerm
-- stringOrVarName = str <|> (asgNameParser varName)

-- asgNameParser :: SymParser Name -> AsgParser SomeTerm
-- asgNameParser nameParser = buildAsg $ (\varN -> id (IR.var' varN))
--      <$> nameParser

-- -- === Unit body === --

-- unit' :: AsgParser SomeTerm
-- unit  :: AsgParser (Expr Unit)
-- unit' = generalize <<$>> unit
-- unit  = buildAsg $
--     (\imps cls
--         -> unsafeGeneralize <$> xliftAstApp2 (flip IR.unit' []) imps cls)
--         <$ spacing <*> (foreignImportList <|> impHub) <*> unitCls <* spacing
--     where spacing = many eol

-- unitCls :: AsgParser SomeTerm
-- unitCls = buildAsg $ (\ds -> liftAstApp1 (IR.clsASG' False "" [] []) (sequence ds)) <$> optionalBlockTop topLvlDecl

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




-- parsingPassM :: (MonadPassManager m, ParsingPassReq2 m) => AsgParser SomeTerm -> m ()
-- parsingPassM p = do
--     src <- getAttr @Source
--     (ref, gidMap) <- parsingBase p (convert src)
--     putAttr @ParsedExpr    $ wrap ref
--     putAttr @MarkedExprMap $ gidMap


parsingBase :: AsgParser a -> Text32.Text32 -> Pass Parser (a, MarkedExprMap)
parsingBase p src = do
    let stream = Lexer.evalDefLexer src
    result <- runParserT (stx *> p <* etx) stream
    case result of
        Left e -> error ("Parser error: " <> parseErrorPretty e) -- FIXME[WD]: handle it the proper way
        Right (AsgBldr irb) -> do
            ((ref, unmarked), gidMap) <- State.runDefT @MarkedExprMap $ State.runDefT @UnmarkedExprs irb
            pure (ref, gidMap)

-- parsingBase_ :: ( MonadPassManager m, ParsingPassReq_2 m
--                 , UnsafeGeneralizable a SomeTerm, UnsafeGeneralizable a SomeTerm -- FIXME[WD]: Constraint for testing only
--                 ) => AsgParser a -> Text32 -> m a
-- parsingBase_ = view _1 .:. parsingBase

-- parserPassX  :: MonadPassManager m => AsgParser SomeTerm -> Pass Parsing   m
-- parserPassX  = parsingPassM

-- reparserPass :: MonadPassManager m => AsgParser SomeTerm -> Pass Reparsing m
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
