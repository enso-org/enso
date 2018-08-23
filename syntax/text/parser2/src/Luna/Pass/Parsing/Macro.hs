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
notReserved = \ma -> ma >>= \a -> a <$ failIfReserved a
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

-- | Macro patterns are used to define non-standard macro structures like
--   'if ... then ... else ...' or 'def foo a b: ...'. They work in a similar
--   fashion to Lisp macros.
--
--   Macro consist of one or more segments. Each segment starts with a special
--   token match (like special keyword) and consist of many chunks. Chunk is
--   one of pre-defined symbol group defined by the 'ChunkParser'.


-- === Definition === --

data ChunkParser
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
    { _ast    :: Ast.Ast
    , _chunks :: [ChunkParser]
    } deriving (Show)
makeLenses ''Segment

data Macro = Macro
    { _headSegment  :: Segment
    , _tailSegments :: SegmentList
    }
    deriving (Show)
makeLenses ''Macro



-- === Smart constructors === --

macro :: Ast.Ast -> [ChunkParser] -> Macro
macro = \ast pat -> Macro (Segment ast pat) SegmentListNull
{-# INLINE macro #-}

segment :: Ast.Ast -> [ChunkParser] -> Segment
segment = Segment
{-# INLINE segment #-}

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













syntax_if_then_else = macro
              (Ast.AstVar $ Ast.Var "if")   [Expr]
   +! segment (Ast.AstVar $ Ast.Var "then") [Expr]
   +? segment (Ast.AstVar $ Ast.Var "else") [Expr]

syntax_group = macro
              (Ast.AstOperator $ Ast.Operator "(") [Expr]
   +! segment (Ast.AstOperator $ Ast.Operator ")") []

syntax_list = macro
              (Ast.AstOperator $ Ast.Operator "[") [Expr]
   +! segment (Ast.AstOperator $ Ast.Operator "]") []

syntax_funcDed = macro
              (Ast.AstVar      $ Ast.Var      "def") [NonSpacedExpr, ManyNonSpacedExpr]
   +! segment (Ast.AstOperator $ Ast.Operator ":")   [Expr]





parseChunk :: ChunkParser -> Parser Ast
parseChunk = \chunk -> case chunk of
    Expr              -> parseExpr
    NonSpacedExpr     -> parseNonSpacedExpr
    ManyNonSpacedExpr -> parseManyNonSpacedExpr

parseChunks :: [ChunkParser] -> Parser [Ast]
parseChunks = mapM parseChunk
{-# NOINLINE parseChunks #-}

parseSegment :: Segment -> Parser [Ast]
parseSegment = parseChunks . view chunks
{-# INLINE parseSegment #-}

parseExpr :: Parser Ast
parseExpr = buildExpr =<< many1 go where
    go = do
        tok <- anyTokenNotReserved
        lookupSection (Ast.unspan tok) >>= \case
            Nothing   -> pure tok
            Just sect -> parseSection tok sect
{-# INLINE parseExpr #-}


parseManyNonSpacedExpr :: Parser Ast
parseManyNonSpacedExpr = Ast.list <$> many parseNonSpacedExpr
{-# INLINE parseManyNonSpacedExpr #-}

parseNonSpacedExpr :: Parser Ast
parseNonSpacedExpr = buildExpr =<< go where
    go = do
        tok  <- anyTokenNotReserved
        head <- lookupSection (Ast.unspan tok) >>= \case
            Nothing   -> pure tok
            Just sect -> parseSection tok sect
        let loop = nextTokenNotLeftSpaced >> go
        (head:) <$> (loop <|> pure [])
{-# INLINE parseNonSpacedExpr #-}

nextTokenNotLeftSpaced :: Parser ()
nextTokenNotLeftSpaced = do
    tok <- peekToken
    when_ (checkLeftSpacing tok) $ fail "spaced"
{-# INLINE nextTokenNotLeftSpaced #-}


parseSegmentList :: Name -> SegmentList -> Parser (Name, [Spanned [Ast]])
parseSegmentList = go where
    go name lst = (goTok name lst =<< peekToken) <|> pure (name, mempty)
    goTok name lst = \tok -> case lst of
        SegmentListNull -> pure (name, mempty)
        SegmentListCons tp (Segment seg chunks) lst' ->
            if Ast.unspan tok == seg
                then acceptSegment  name lst' tok chunks
                else discardSegment name lst' seg tp

    acceptSegment name lst tok chunks = do
        dropToken
        outs <- withNextSegmentReserved lst (parseChunks chunks)
        (Ast.Spanned (tok ^. Ast.span) outs:) <<$>> parseSegmentList (name <> "_" <> showSection (tok ^. Ast.ast)) lst

    -- FIXME: Should discardSegment call parseSegmentList ?
    discardSegment name lst seg = \case
        Optional -> pure (name, mempty)
        Required -> (Ast.Spanned mempty [Ast.invalid Invalid.MissingSection]:)
              <<$>> parseSegmentList (name <> "_" <> showSection seg) lst

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

withNextSegmentReserved :: SegmentList -> Parser a -> Parser a
withNextSegmentReserved = \case
    SegmentListCons _ (Segment seg _) _ -> withReserved seg
    SegmentListNull -> id

parseSection :: Ast -> Macro -> Parser Ast
parseSection = \(Ast.Spanned span tok) (Macro seg lst) -> do
    psegs            <- withNextSegmentReserved lst $ parseSegment seg
    (name, spanLst)  <- parseSegmentList (showSection tok) lst
    let (tailSpan, slst) = mergeSpannedLists spanLst
    let header = Ast.Spanned span $ Ast.var' name
        group  = Ast.apps header  $ psegs <> slst
        out    = group & Ast.span %~ (<> tailSpan)
    pure out



showSection :: Ast.Ast -> Name
showSection = \case
    Ast.AstVar      (Ast.Var      n) -> n
    Ast.AstCons     (Ast.Cons     n) -> n
    Ast.AstOperator (Ast.Operator n) -> n
    x -> error $ ppShow x















type Parser = State.StatesT '[Registry, Reserved Ast.Ast, Scope.Scope] (Parsec.Parser Ast)




runP :: [Ast] -> Parser a -> Either String a
runP = \stream p
    -> flip Parsec.parseOnly (Vector.fromList stream)
     $ State.evalDefT  @Scope.Scope
     $ State.evalDefT  @(Reserved Ast.Ast)
     $ State.evalDefT  @Registry
     $ do
        mapM_ registerSection
            [ syntax_if_then_else
            , syntax_group
            , syntax_list
            , syntax_funcDed
            ]
        p






-- dropToken :: State.Monad Stream m => m ()
-- dropToken = State.modify_ @Stream $ \s -> case unwrap s of
--     []     -> s
--     (_:as) -> wrap as
-- {-# INLINE dropToken #-}















-- parseNonSpacedExpr :: SegmentBuilder m => m Ast
-- parseNonSpacedExpr = buildExpr =<< go1 where
--     go1 = tokenNotReserved >>= \case
--         Nothing  -> pure mempty
--         Just tok -> do
--             head <- lookupSection (Ast.unspan tok) >>= \case
--                 Nothing   -> pure tok
--                 Just sect -> parseSection tok sect
--             (head:) <$> go2
--     go2 = peekToken >>= \case
--         Nothing  -> pure mempty
--         Just tok -> if checkLeftSpacing tok
--             then pure mempty
--             else go1
-- {-# INLINE parseNonSpacedExpr #-}

-- data Macro = Macro Segment SegmentList
--     deriving (Show)

-- data SegmentList
--     = Required Segment SegmentList
--     | Optional Segment SegmentList
--     | SegmentListNull
--     deriving (Show)

-- data Segment = Segment
--     { _name   :: Ast.Ast
--     , _chunks :: [ChunkParser]
--     } deriving (Show)
-- makeLenses ''Segment


-- importDef
--     = macro "import" [Expr]















-- data Stream       = OpStreamStart Name (Ast -> Ast) ElStream
--                   | ElStreamStart ElStream
--                   | NullStream

-- data OpStream     = OpStream      Name OpStreamType
-- data OpStreamType = InfixOp (Ast -> Ast -> Ast) ElStream
--                   | EndOp   (Ast -> Ast)

-- data ElStream     = ElStream      Ast ElStreamType
-- data ElStreamType = InfixEl OpStream
--                   | EndEl



-- abc
-- +*

-- a b + c

-- (a#b)c
-- +


-- a +b c

-- a


-- a
-- -

-- inheritCodeSpanLst :: BuilderMonad m
--     => ([IR.Term a] -> m (IR.Term b))
--     -> ([IR.Term a] -> m (IR.Term b))
-- inheritCodeSpanLst = \f ts -> do
--     cs <- IR.readLayer @CodeSpan <$$> ts
--     ir <- f ts
--     IR.writeLayer @CodeSpan ir (mconcat cs)
--     pure ir
-- {-# INLINE inheritCodeSpanLst #-}


-- partitionTokStream :: BuilderMonad m => Int -> [IR.SomeTerm] -> m [IR.SomeTerm]
-- partitionTokStream = \ind toks -> partitionTokStream__ ind toks mempty mempty
-- {-# INLINE partitionTokStream #-}

-- partitionTokStream__ :: BuilderMonad m
--     => Int -> [IR.SomeTerm] -> [IR.SomeTerm] -> [IR.SomeTerm] -> m [IR.SomeTerm]
-- partitionTokStream__ = \ind -> let
--     go toks expr defs = let
--         submitDefs = (: defs) <$> inheritCodeSpanLst IR.tokens' expr
--         in case toks of
--             []     -> if null expr then pure defs else submitDefs
--             (t:ts) -> IR.model t >>= \case
--                 Uni.LineBreak i -> case compare ind i of
--                     EQ -> do
--                         IR.delete t
--                         go ts mempty =<< submitDefs
--                     LT -> go ts (t:expr) defs
--                     GT -> error "TODO: wrong indentation"
--                 _ -> go ts (t:expr) defs
--     in go




-- | The pass algorithm is shown below.
--
-- ## STAGE 1
--
-- In this stage we produce AST from token stream. In order to correctly
-- assemble the expressions, we need to consider the following topics.
--
--
-- ### Operator precedences
--
-- There is no restriction where precedence relations are defined across the
-- file. The multi-pass parsing allows us to correctly handle every situation.
--
--
-- ### Custom parsing rules
--
-- Custom parsing rules allow altering the macro completely. In barebone parser
-- everything is just an expression containing identifiers and operators
-- separated by spaces. Any more complex construction like `type Vector a ...`
-- is defined as custom parsing rule and is translated to appriopriate macro
-- call.
--
-- All custom parsing rules need to be defined in a separate compilation unit,
-- let it be a file or other module singularRequiredSegmentList to be imported. Otherwise after
-- parsing a macro call and evaluating it, it might result in a custom parsing
-- rule definition which would alter how the original macro call was parsed.
--
--
-- ### Mixfix operators.
--
-- While assembling final expression from tokens we need to know all possible
-- mixfix operators like `if_then_else`. Unlike custom parsing rules. mixfix
-- operators could be discovered in a multi-pass parsing process. However, it is
-- not obvious how to efficiently parse mixfix declarations as deep patterns. It
-- is not a real problem for us, because we do not allow for custom mixfix
-- definitions at all currently, but it's worth noting as possible problem in
-- the future.
--
--
-- #### Mixfixes as deep patterns
--
-- Definition of custom mixfixes as deep pattern is very tricky. Consider:
--
-- ```haskell
-- foo1 bar1_bar2 x foo2 = ...  -- (1)
-- bar1 foo1_foo2 x bar2 = ...  -- (2)
-- ```
--
-- If we assume that `foo1_foo2` IS NOT mixfix in scope then the (1) CREATES
-- `bar1_bar2` mixfix, which if applied to (2) USES mixfix `foo1_foo2` as a
-- variable from scope and applies `x` to it.

-- If we assume that `foo1_foo2` IS mixfix in scope then (1) uses `bar1_bar2` as
-- scope variable and aplies `x` to it. Then (2) has also to be mixfix because its
-- in scope, which means that we use `foo1_foo2` as variable from the scope here.

-- The above code is very tricky because it has some looping rules and the only
-- situation to make them correct is to assume that both `foo1_foo2` and
-- `bar1_bar2` are in the scope while parsing and we do NOT override them here.
--



-- The implementation progress uses the following legend:
--
--     [ ] - to be done
--     [x] - already implemented
--     [-] - postponed
--     [.] - partial, possibly hardcoded implementation
--
--
-- handle imports and populate
-- scope with information about names and precedences.
--
-- [x] 1. Partition token stream into sub-streams based on indentation,
--        discover invalid indentations.
--     2. Iterate over sub-streams and discover special types:
-- [ ]      1. Imports
-- [-]      2. Mixfix definitions (add them to scope)
-- [ ]      3. Precedence definitions (add them to scope)
-- [-] 3. Evaluating imports.
--        ...
--     4. For each expression:
-- [ ]      1. Run expr builder (partial AST builder)
-- [ ]      2. Discover all inner token stream blocks
-- [ ]      3. Iterate whole process for each such block
--
--
-- ## STAGE 2
-- In this stage we run macros and generate final AST
--
-- [ ] 5. Iterate over the AST and discover all macro function calls, in
-- [ ]    particular all module-level calls like `type ...`
-- [ ] 6. Evaluate the calls and replace them with final AST



-- run :: BuilderMonad m => [IR.SomeTerm] -> m ()
-- run = \toks -> do
--     print "!!!!"

--                 -- if null expr then pure defs
--     -- let (e:es) = exprs
--     -- x <- IR.model e
--     -- case x of
--     --     Uni.Lam {} -> print "ouch"
--     out <- partitionTokStream 0 toks
--     print =<< (mapM IR.model out)
--     pure ()

--     -- putLnFmtd =<< showM out
