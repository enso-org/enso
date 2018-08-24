{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Pass.Parsing.Macro where

import Prologue

import qualified Control.Monad.State.Layered               as State
import qualified Data.Graph.Component.Node.Destruction     as Component
import qualified Data.Map                                  as Map
import qualified Data.Set                                  as Set
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

import Data.Map                              (Map)
import Data.Set                              (Set)
import Data.Text.Position                    (Delta (Delta))
import Data.Vector                           (Vector)
import Luna.Syntax.Text.Parser.Data.CodeSpan (CodeSpan)
import Luna.Syntax.Text.Parser.Data.Invalid  (Invalids)
import Luna.Syntax.Text.Parser.Data.Result   (Result)
import Luna.Syntax.Text.Parser.IR.Ast        (Spanned (Spanned))
import Luna.Syntax.Text.Parser.IR.Term       (Ast)
import Luna.Syntax.Text.Source               (Source)
import OCI.Data.Name                         (Name)


import Luna.Pass.Parsing.ExprBuilder (ExprBuilderMonad, buildExpr,
                                      checkLeftSpacing)

import qualified Data.Attoparsec.Internal       as AParsec
import qualified Data.Attoparsec.Internal.Types as AParsec
import qualified Data.Attoparsec.List           as Parsec

import Data.Parser



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
    { _ast           :: Ast.Ast
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
    $ wrapped %~ Map.insert (section ^. headSegment . ast) section
{-# INLINE registerSection #-}

lookupSection :: State.Getter Registry m => Ast.Ast -> m (Maybe Macro)
lookupSection = \ast -> Map.lookup ast . unwrap <$> State.get @Registry
{-# INLINE lookupSection #-}



-------------------
-- === Utils === --
-------------------

withNextSegmentReserved :: SegmentList -> Parser a -> Parser a
withNextSegmentReserved = \case
    SegmentListCons _ (Segment seg _) _ -> withReserved seg
    SegmentListNull -> id
{-# INLINE withNextSegmentReserved #-}

nextTokenNotLeftSpaced :: Parser ()
nextTokenNotLeftSpaced = do
    tok <- peekToken
    when_ (checkLeftSpacing tok) $ fail "spaced"
{-# INLINE nextTokenNotLeftSpaced #-}



---------------------
-- === Parsers === --
---------------------

-- === Definition === --

type Parser = State.StatesT
   '[ Registry
    , Reserved Ast.Ast
    , Scope.Scope
    ] (Parsec.Parser Ast)

run :: [Ast] -> Parser a -> Either String a
run = \stream
    -> flip Parsec.parseOnly (Vector.fromList stream)
     . State.evalDefT  @Scope.Scope
     . State.evalDefT  @(Reserved Ast.Ast)
     . State.evalDefT  @Registry
{-# INLINE run #-}


-- === Macro building block parsers === --

chunk :: Chunk -> Parser Ast
chunk = \case
    Expr              -> expr
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
    go name lst = (goTok name lst =<< peekToken) <|> pure (name, mempty)
    goTok name lst = \tok -> case lst of
        SegmentListNull -> pure (name, mempty)
        SegmentListCons tp (Segment seg chunkDefs) lst' ->
            if Ast.unspan tok == seg
                then acceptSegment  name lst' tok chunkDefs
                else discardSegment name lst' seg tp

    acceptSegment name lst tok chunkDefs = do
        dropToken
        outs <- withNextSegmentReserved lst (chunks chunkDefs)
        let name' = name <> "_" <> showSection (tok ^. Ast.ast)
        (Ast.Spanned (tok ^. Ast.span) outs:) <<$>> segmentList name' lst

    -- FIXME: Should discardSegment call segmentList ?
    discardSegment name lst seg = \case
        Optional -> pure (name, mempty)
        Required -> (Ast.Spanned mempty [Ast.invalid Invalid.MissingSection]:)
              <<$>> segmentList (name <> "_" <> showSection seg) lst


-- === Section parsers === --

macro :: Ast -> Macro -> Parser Ast
macro = \(Ast.Spanned span tok) (Macro seg lst) -> do
    psegs            <- withNextSegmentReserved lst $ segment seg
    (name, spanLst)  <- segmentList (showSection tok) lst
    let (tailSpan, slst) = mergeSpannedLists spanLst
    let header = Ast.Spanned span $ Ast.var' name
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
    Ast.AstVar      (Ast.Var      n) -> n
    Ast.AstCons     (Ast.Cons     n) -> n
    Ast.AstOperator (Ast.Operator n) -> n
    x -> error $ ppShow x
{-# INLINE showSection #-}


-- === API === --

expr :: Parser Ast
expr = buildExpr =<< many1 go where
    go = do
        tok <- anyTokenNotReserved
        lookupSection (Ast.unspan tok) >>= \case
            Nothing   -> pure tok
            Just sect -> macro tok sect
{-# INLINE expr #-}

manyNonSpacedExpr :: Parser Ast
manyNonSpacedExpr = Ast.list <$> many nonSpacedExpr
{-# INLINE manyNonSpacedExpr #-}

nonSpacedExpr :: Parser Ast
nonSpacedExpr = buildExpr =<< go where
    go = do
        tok  <- anyTokenNotReserved
        head <- lookupSection (Ast.unspan tok) >>= \case
            Nothing   -> pure tok
            Just sect -> macro tok sect
        let loop = nextTokenNotLeftSpaced >> go
        (head:) <$> (loop <|> pure [])
{-# INLINE nonSpacedExpr #-}



-------------------------------
-- === Predefined Macros === --
-------------------------------

-- | We might want to define these macros in stdlib in the future. However,
--   the real question is if we want to give the end user power to define
--   such customizable syntax.

syntax_if_then_else :: Macro
syntax_if_then_else = mkMacro
                 (Ast.AstVar $ Ast.Var "if")   [Expr]
    +! mkSegment (Ast.AstVar $ Ast.Var "then") [Expr]
    +? mkSegment (Ast.AstVar $ Ast.Var "else") [Expr]

syntax_group :: Macro
syntax_group = mkMacro
                 (Ast.AstOperator $ Ast.Operator "(") [Expr]
    +! mkSegment (Ast.AstOperator $ Ast.Operator ")") []

syntax_list :: Macro
syntax_list = mkMacro
                 (Ast.AstOperator $ Ast.Operator "[") [Expr]
    +! mkSegment (Ast.AstOperator $ Ast.Operator "]") []

syntax_funcDed :: Macro
syntax_funcDed = mkMacro
                 (Ast.AstVar      $ Ast.Var      "def") [ NonSpacedExpr
                                                        , ManyNonSpacedExpr]
    +! mkSegment (Ast.AstOperator $ Ast.Operator ":")   [Expr]



hardcodePredefinedMacros :: State.Monad Registry m => m ()
hardcodePredefinedMacros = mapM_ registerSection
    [ syntax_if_then_else
    , syntax_group
    , syntax_list
    , syntax_funcDed
    ]
