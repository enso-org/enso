{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Pass.Parsing.Macro where

import Prologue

import qualified Control.Monad.State.Layered               as State
import qualified Data.Graph.Component.Node.Destruction     as Component
import qualified Data.Map                                  as Map
import qualified Data.Set                                  as Set
import qualified Data.Text.Position                        as Position
import qualified Data.Text.Span                            as Span
import qualified Data.Vector                               as Vector
import qualified Language.Symbol.Operator.Assoc            as Assoc
import qualified Language.Symbol.Operator.Prec             as Prec
import qualified Luna.IR                                   as IR
import qualified Luna.IR.Aliases                           as Uni
import qualified Luna.IR.Term.Ast.Invalid                  as Invalid
import qualified Luna.Pass                                 as Pass
import qualified Luna.Syntax.Text.Parser.Data.CodeSpan     as CodeSpan
import qualified Luna.Syntax.Text.Parser.Data.Name.Special as Name
import qualified Luna.Syntax.Text.Parser.IR.Ast            as Ast
import qualified Luna.Syntax.Text.Scope                    as Scope
import qualified Text.Parser.State.Indent                  as Indent
import qualified Text.Parser.State.Indent                  as Indent

import Data.Map                              (Map)
import Data.Set                              (Set)
import Data.Text.Position                    (Delta (Delta), Position)
import Data.Vector                           (Vector)
import Luna.Syntax.Text.Parser.Data.CodeSpan (CodeSpan)
import Luna.Syntax.Text.Parser.Data.Result   (Result)
import Luna.Syntax.Text.Parser.IR.Ast        (Spanned (Spanned))
import Luna.Syntax.Text.Parser.IR.Term       (Ast)
import Luna.Syntax.Text.Source               (Source)
import OCI.Data.Name                         (Name)
import Text.Parser.State.Indent              (Indent)


import Luna.Pass.Parsing.ExprBuilder (ExprBuilderMonad, buildExpr,
                                      checkLeftSpacing)

import qualified Data.Attoparsec.Internal       as AParsec
import qualified Data.Attoparsec.Internal.Types as AParsec
import qualified Data.Attoparsec.List           as Parsec

import Data.Parser

import Data.Parser.Instances.Attoparsec ()



----------------------
-- === Reserved === --
----------------------

-- === Definition === --

newtype Reserved a = Reserved (Set a)
    deriving (Default, Show)
makeLenses ''Reserved

type family ReservedType m where
    ReservedType (State.StateT (Reserved a) _) = a
    ReservedType (t m)                         = ReservedType m

type ReservedM       m = Reserved (ReservedType m)
type MonadReserved'  m = MonadReserved (Token m) m
type MonadReserved a m =
    ( State.Monad (ReservedM m) m
    , MonadFail m
    , Ord (ReservedType m)
    , Convertible' a (ReservedType m)
    )


-- === API === --

withReserved :: ∀ a t m. MonadReserved a m => a -> m t -> m t
withReserved = \a -> State.withModified @(ReservedM m)
                   $ wrapped %~ Set.insert (convert' a)
{-# INLINE withReserved #-}

withManyReserved :: ∀ a t m. MonadReserved a m => [a] -> m t -> m t
withManyReserved = \a -> State.withModified @(ReservedM m)
                       $ wrapped %~ (Set.fromList (convert' <$> a) <>)
{-# INLINE withManyReserved #-}

checkReserved :: ∀ a m. MonadReserved a m => a -> m Bool
checkReserved = \a -> Set.member (convert' a) . unwrap
                  <$> State.get @(ReservedM m)
{-# INLINE checkReserved #-}

failIfReserved :: MonadReserved a m => a -> m ()
failIfReserved = \a -> checkReserved a >>= \case
    True  -> fail "reserved"
    False -> pure ()
{-# INLINE failIfReserved #-}

notReserved :: MonadReserved a m => m a -> m a
notReserved = (>>= (\a -> a <$ failIfReserved a))
{-# INLINE notReserved #-}

anyTokenNotReserved :: (MonadReserved' m, TokenParser m) => m (Token m)
anyTokenNotReserved = notReserved anyToken
{-# INLINE anyTokenNotReserved #-}

peekTokenNotReserved :: (MonadReserved' m, TokenParser m) => m (Token m)
peekTokenNotReserved = notReserved peekToken
{-# INLINE peekTokenNotReserved #-}



-------------------
-- === Macro === --
-------------------

-- | Macro patterns are used to define non-standard mkMacro structures like
--   'if ... then ... else ...' or 'def foo a b: ...'. They work in a similar
--   fashion to Lisp macros.
--
--   Macro consist of one or more segments. Each mkSegment starts with a special
--   token match (like special keyword) and consist of many chunks. Chunk is
--   one of pre-defined symbol group defined by the 'Chunk'.


-- === Definition === --

data Chunk
    = Expr
    | ManyNonSpacedExpr
    | NonSpacedExpr
    | ExprBlock
    deriving (Show)

data SegmentList
    = SegmentListCons SegmentType Segment SegmentList
    | SegmentListNull
    deriving (Show)

data SegmentType
    = Required
    | Optional
    deriving (Eq, Show)

data Segment = Segment
    { _segmentBase   :: Ast.Ast
    , _segmentChunks :: [Chunk]
    } deriving (Show)
makeLenses ''Segment

data Macro = Macro
    { _headSegment  :: Segment
    , _tailSegments :: SegmentList
    }
    deriving (Show)
makeLenses ''Macro



-- === Smart constructors === --

mkMacro :: Ast.Ast -> [Chunk] -> Macro
mkMacro = \ast pat -> Macro (Segment ast pat) SegmentListNull
{-# INLINE mkMacro #-}

mkSegment :: Ast.Ast -> [Chunk] -> Segment
mkSegment = Segment
{-# INLINE mkSegment #-}

singularOptionalSegmentList :: Segment -> SegmentList
singularOptionalSegmentList = \s -> SegmentListCons Optional s SegmentListNull
{-# INLINE singularOptionalSegmentList #-}

singularRequiredSegmentList :: Segment -> SegmentList
singularRequiredSegmentList = \s -> SegmentListCons Required s SegmentListNull
{-# INLINE singularRequiredSegmentList #-}

infixl 6 +?
infixl 6 +!
(+?), (+!) :: Macro -> Segment -> Macro
(+?) = \l r -> appendSegment l (singularOptionalSegmentList r)
(+!) = \l r -> appendSegment l (singularRequiredSegmentList r)
{-# INLINE (+?) #-}
{-# INLINE (+!) #-}


-- === Utils === --

concatSegmentList :: SegmentList -> SegmentList -> SegmentList
concatSegmentList = go where
    go l r = case l of
        SegmentListNull           -> r
        SegmentListCons tp sect t -> SegmentListCons tp sect (go t r)
{-# INLINE concatSegmentList #-}

appendSegment :: Macro -> SegmentList -> Macro
appendSegment = \sect r -> sect & tailSegments %~ flip concatSegmentList r
{-# INLINE appendSegment #-}



----------------------------
-- === Macro Registry === --
----------------------------

-- === Definition === --

newtype Registry = Registry (Map Ast.Ast Macro)
    deriving (Default, Show)
makeLenses ''Registry


-- === API === --

registerSection :: State.Monad Registry m => Macro -> m ()
registerSection = \section -> State.modify_ @Registry
    $ wrapped %~ Map.insert (section ^. headSegment . segmentBase) section
{-# INLINE registerSection #-}

lookupSection :: State.Getter Registry m => Ast.Ast -> m (Maybe Macro)
lookupSection = \ast -> Map.lookup ast . unwrap <$> State.get @Registry
{-# INLINE lookupSection #-}



-------------------
-- === Utils === --
-------------------

segmentsToks :: SegmentList -> [Ast.Ast]
segmentsToks = \case
    SegmentListNull -> mempty
    SegmentListCons _ (Segment seg _) lst -> seg : segmentsToks lst
{-# INLINE segmentsToks #-}

withNextSegmentsReserved :: SegmentList -> Parser a -> Parser a
withNextSegmentsReserved = withManyReserved . segmentsToks
{-# INLINE withNextSegmentsReserved #-}

nextTokenNotLeftSpaced :: Parser ()
nextTokenNotLeftSpaced = do
    tok <- peekToken
    when_ (checkLeftSpacing tok) $ fail "spaced"
{-# INLINE nextTokenNotLeftSpaced #-}

nextTokenLeftSpaced :: Parser ()
nextTokenLeftSpaced = do
    tok <- peekToken
    when_ (not $ checkLeftSpacing tok) $ fail "not spaced"
{-# INLINE nextTokenLeftSpaced #-}

leftSpaced :: Parser Ast -> Parser Ast
leftSpaced = \mtok -> do
    tok <- mtok
    if checkLeftSpacing tok
        then pure tok
        else fail "not spaced"
{-# INLINE leftSpaced #-}

anySymbol :: Parser Ast
anySymbol = peekSymbol <* dropToken
{-# INLINE anySymbol #-}

peekSymbol :: Parser Ast
peekSymbol = do
    tok <- peekToken
    case Ast.unspan tok of
        Ast.AstLineBreak {} -> fail "not a symbol"
        _                   -> pure tok
{-# INLINE peekSymbol #-}

anySymbolNotReserved :: Parser Ast
anySymbolNotReserved = notReserved anySymbol
{-# INLINE anySymbolNotReserved #-}

peekSymbolNotReserved :: Parser Ast
peekSymbolNotReserved = notReserved peekSymbol
{-# INLINE peekSymbolNotReserved #-}

satisfyAst :: (Ast.Ast -> Bool) -> Parser Ast
satisfyAst = \f -> peekSatisfyAst f <* dropToken
{-# INLINE satisfyAst #-}

peekSatisfyAst :: (Ast.Ast -> Bool) -> Parser Ast
peekSatisfyAst = \f -> do
    tok <- peekToken
    if f (Ast.unspan tok)
        then pure tok
        else fail "satisfy"
{-# INLINE peekSatisfyAst #-}

ast :: Ast.Ast -> Parser Ast
ast = satisfyAst . (==)
{-# INLINE ast #-}

unsafeLineBreak :: Parser Ast
unsafeLineBreak = do
    tok <- anyToken
    case Ast.unspan tok of
        Ast.LineBreak off -> do
            Position.succLine
            Position.incColumn off
            pure tok
        _ -> fail "not line break"
{-# INLINE unsafeLineBreak #-}

unsafeLineBreaks :: Parser [Ast]
unsafeLineBreaks = many1 unsafeLineBreak
{-# INLINE unsafeLineBreaks #-}

-- | The current implementation worss on token stream where some tokens are
--   line break indicators. After consuming such tokens we need to register them
--   as offset in the following token.
brokenLst :: Parser (NonEmpty Ast) -> Parser (NonEmpty Ast)
brokenLst = \f -> do
    spans     <- toksSpanAsSpace <$> unsafeLineBreaks
    (a :| as) <- f
    let a' = a & Ast.span %~ (spans <>)
    pure (a' :| as)
{-# INLINE brokenLst #-}

brokenLst' :: Parser (NonEmpty Ast) -> Parser [Ast]
brokenLst' = fmap convert . brokenLst
{-# INLINE brokenLst' #-}

broken :: Parser Ast -> Parser Ast
broken = \f -> do
    spans <- toksSpanAsSpace <$> unsafeLineBreaks
    tok   <- f
    pure $ tok & Ast.span %~ (spans <>)
{-# INLINE broken #-}

possiblyBroken :: Parser Ast -> Parser Ast
possiblyBroken = \p -> broken (Indent.indented *> p) <|> p
{-# INLINE possiblyBroken #-}

anyExprToken :: Parser Ast
anyExprToken = possiblyBroken anyToken
{-# INLINE anyExprToken #-}

toksSpanAsSpace :: [Ast] -> CodeSpan
toksSpanAsSpace = CodeSpan.asOffsetSpan . mconcat . fmap (view Ast.span)
{-# INLINE toksSpanAsSpace #-}



---------------------
-- === Parsers === --
---------------------

-- === Definition === --

type Parser = State.StatesT
   '[ Scope.Scope
    , Reserved Ast.Ast
    , Position
    , Indent
    , Registry
    ] (Parsec.Parser Ast)

run :: [Ast] -> Parser a -> Either String a
run = \stream
  -> flip Parsec.parseOnly (Vector.fromList stream)
    . State.evalDefT  @Registry
    . Indent.eval
    . State.evalDefT  @Position
    . State.evalDefT  @(Reserved Ast.Ast)
    . State.evalDefT  @Scope.Scope
{-# INLINE run #-}


-- === Macro building block parsers === --

chunk :: Chunk -> Parser Ast
chunk = \case
    Expr              -> possiblyBroken expr
    NonSpacedExpr     -> nonSpacedExpr
    ManyNonSpacedExpr -> manyNonSpacedExpr
{-# INLINE chunk #-}

chunks :: [Chunk] -> Parser [Ast]
chunks = mapM chunk
{-# NOINLINE chunks #-}


-- === Degment parsers === --

segment :: Segment -> Parser [Ast]
segment = chunks . view segmentChunks
{-# INLINE segment #-}

segmentList :: Name -> SegmentList -> Parser (Name, [Spanned [Ast]])
segmentList = go where
    go = \name -> \case
        SegmentListNull -> pure (name, mempty)
        SegmentListCons tp seg lst' -> do
            let name' = name <> "_" <> showSection (seg ^. segmentBase)
            acceptSegment name' seg lst' <|> discardSegment name name' lst' tp

    acceptSegment name seg lst = do
        tok <- anyExprToken
        when_ (Ast.unspan tok /= (seg ^. segmentBase)) $ fail "not a segment"
        outs <- withNextSegmentsReserved lst (segment seg)
        (Ast.Spanned (tok ^. Ast.span) outs:) <<$>> segmentList name lst

    discardSegment name name' lst = \case
        Optional -> pure (name, mempty)
        Required -> (Ast.Spanned mempty [Ast.invalid Invalid.MissingSection]:)
              <<$>> segmentList name' lst

-- minimalMacroMatchName :: Name -> SegmentList -> Name
-- minimalMacroMatchName = \r -> \case
--     SegmentListNull -> r
--     SegmentListCons tp seg lst' -> case tp of
--         Optional -> r
--         Required -> minimalMacroMatchName (r <> "_" <> (showSection $ seg ^. segmentBase)) lst'


-- === Section parsers === --

macro :: Ast -> Macro -> Parser Ast
macro = \(Ast.Spanned span tok) (Macro seg lst) -> do
    psegs            <- withNextSegmentsReserved lst $ segment seg
    (name, spanLst)  <- segmentList (showSection tok) lst
    -- error $ "afterMacro\n" <> ppShow (name, spanLst, psegs)
    let (tailSpan, slst) = mergeSpannedLists spanLst
    let header = Ast.Spanned span $ Ast.Var name
        group  = Ast.apps header  $ psegs <> slst
        out    = group & Ast.span %~ (<> tailSpan)
    pure out
{-# INLINE macro #-}

mergeSpannedLists :: [Spanned [Ast]] -> (CodeSpan, [Ast])
mergeSpannedLists = \lst -> let
    prependSpan span = Ast.span %~ (CodeSpan.asOffsetSpan span <>)
    in case lst of
        [] -> (mempty, mempty)
        (Ast.Spanned span a : as) -> case a of
            (t:ts) -> ((prependSpan span t : ts) <>) <$> mergeSpannedLists as
            []     -> let
                (tailSpan, lst) = mergeSpannedLists as
                in case lst of
                    []     -> (span <> tailSpan, lst)
                    (t:ts) -> (tailSpan, (prependSpan span t : ts))
{-# INLINE mergeSpannedLists #-}

showSection :: Ast.Ast -> Name
showSection = \case
    Ast.Var      n -> n
    Ast.Cons     n -> n
    Ast.Operator n -> n
    x -> error $ ppShow x
{-# INLINE showSection #-}


-- === API === --

expr :: Parser Ast
expr = option (Ast.invalid Invalid.EmptyExpression) expr'
{-# INLINE expr #-}

expr' :: Parser Ast
expr' = Indent.withCurrent $ buildExpr . convert =<< lines where
    line          = buildExpr =<< multiLineToks
    lines         = (:|) <$> line <*> nextLines
    nextLines     = option mempty (brokenLst' $ Indent.indented *> lines)
    lineJoin      = satisfyAst Ast.isOperator <* leftSpaced peekSymbol
    multiLineToks = lineToks <**> lineTailToks
    lineTailToks  = option id $ flip (<>) <$> brokenLst' lineContToks
    lineContToks  = Indent.indented *> ((:|) <$> lineJoin <*> multiLineToks)
    lineToks      = many1 $ do
        tok <- anySymbolNotReserved
        lookupSection (Ast.unspan tok) >>= \case
            Nothing   -> pure tok
            Just sect -> macro tok sect
{-# INLINE expr' #-}

manyNonSpacedExpr :: Parser Ast
manyNonSpacedExpr = Ast.list <$> many nonSpacedExpr
{-# INLINE manyNonSpacedExpr #-}

nonSpacedExpr :: Parser Ast
nonSpacedExpr = buildExpr =<< go where
    go = do
        tok  <- anySymbolNotReserved
        head <- lookupSection (Ast.unspan tok) >>= \case
            Nothing   -> pure  tok
            Just sect -> macro tok sect
        let loop = nextTokenNotLeftSpaced >> go
        (head:) <$> option mempty loop
{-# INLINE nonSpacedExpr #-}



-------------------------------
-- === Predefined Macros === --
-------------------------------

-- | We might want to define these macros in stdlib in the future. However,
--   the real question is if we want to give the end user power to define
--   such customizable syntax.

syntax_if_then_else :: Macro
syntax_if_then_else = mkMacro
                 (Ast.Var "if")   [Expr]
    +! mkSegment (Ast.Var "then") [Expr]
    +? mkSegment (Ast.Var "else") [Expr]

syntax_group :: Macro
syntax_group = mkMacro
                 (Ast.Operator "(") [Expr]
    +! mkSegment (Ast.Operator ")") []

syntax_list :: Macro
syntax_list = mkMacro
                 (Ast.Operator "[") [Expr]
    +! mkSegment (Ast.Operator "]") []

syntax_funcDed :: Macro
syntax_funcDed = mkMacro
                 (Ast.Var      "def") [ NonSpacedExpr, ManyNonSpacedExpr]
    +! mkSegment (Ast.Operator ":")   [Expr]

hardcodePredefinedMacros :: State.Monad Registry m => m ()
hardcodePredefinedMacros = mapM_ registerSection
    [ syntax_if_then_else
    , syntax_group
    , syntax_list
    , syntax_funcDed
    ]
