{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Pass.Parsing.ExprBuilder where

import Prologue hiding (optional)

import qualified Control.Monad.State.Layered               as State
import qualified Data.Graph.Component.Node.Destruction     as Component
import qualified Data.Map                                  as Map
import qualified Data.Set                                  as Set
import qualified Data.Text.Span                            as Span
import qualified Language.Symbol.Operator.Assoc            as Assoc
import qualified Language.Symbol.Operator.Prec             as Prec
import qualified Luna.IR                                   as IR
import qualified Luna.IR.Aliases                           as Uni
import qualified Luna.IR.Term.Ast.Invalid                  as Invalid
import qualified Luna.Pass                                 as Pass
import qualified Luna.Syntax.Text.Parser.Data.CodeSpan     as CodeSpan
import qualified Luna.Syntax.Text.Parser.Data.Name.Special as Name
import qualified Luna.Syntax.Text.Parser.IR.Ast            as Ast

import Data.Map                              (Map)
import Data.Set                              (Set)
import Data.Text.Position                    (Delta (Delta))
import Luna.Syntax.Text.Parser.Data.CodeSpan (CodeSpan)
import Luna.Syntax.Text.Parser.Data.Result   (Result)
import Luna.Syntax.Text.Parser.IR.Ast        (Spanned (Spanned))
import Luna.Syntax.Text.Parser.IR.Term       (Ast)
import Luna.Syntax.Text.Source               (Source)
import OCI.Data.Name                         (Name)

-- import Data.Parser hiding (Result)


import Data.Attoparsec.List ()







data ExprBuilder
-- FIXME: Contraints like `Component.Delete` should not be so explicit
type ExprBuilderPass m = (Pass.Interface ExprBuilder m, Component.Delete m)
type instance Pass.Spec ExprBuilder t = Spec t
type family   Spec  t where
    Spec (Pass.In  Pass.Attrs) = '[Source, Result]
    Spec (Pass.In  IR.Terms)   = CodeSpan
                              ': Pass.BasicPassSpec (Pass.In IR.Terms)
    Spec (Pass.Out t)          = Spec (Pass.In t)
    Spec t                     = Pass.BasicPassSpec t



type BuilderMonad m =
    ( MonadIO m
    , Pass.Interface ExprBuilder m
    -- , Component.Delete m
    )



buildGraph :: forall m. BuilderMonad m => Ast -> m IR.SomeTerm
buildGraph = \(Spanned cs ast) -> addCodeSpan cs =<< case ast of
    Ast.Cons      name -> IR.cons'  name []
    Ast.Var       name -> IR.var'   name
    Ast.Operator  name -> IR.var'   name
    Ast.Wildcard       -> IR.blank'
    Ast.LineBreak ind  -> IR.lineBreak' (unwrap ind)
    Ast.Invalid   inv  -> IR.invalid' inv
    Ast.Tokens (t:ts)  -> buildGraph t -- FIXME
    Ast.Missing        -> IR.missing'
    Ast.App f a        -> do
        f' <- buildGraph f
        a' <- buildGraph a
        IR.app' f' a'
    x -> error $ "TODO: " <> show x
    where addCodeSpan cs ir = ir <$ IR.writeLayer @CodeSpan ir cs
{-# INLINE buildGraph #-}




--------------------------------
-- === TokenStream Layout === --
--------------------------------

-- | Divide token stream to spaced and unspaced substreams.

-- === Definition === --

data Layout
    = SpacedLayout
    | UnspacedLayout
    deriving (Show)

data Layouted = Layouted
    { _layout :: Layout
    , _stream :: [Ast]
    } deriving (Show)
makeLenses ''Layouted


-- === API === --

discoverLayouts :: [Ast] -> [Layouted]
discoverLayouts = \case
    []     -> []
    (a:as) -> discoverLayouts__ [a] as []
{-# INLINE discoverLayouts #-}

discoverLayouts__ :: [Ast] -> [Ast] -> [Layouted] -> [Layouted]
discoverLayouts__ = \buff stream out -> case stream of
    [] -> reverse $ spaced buff out
    (tok:toks) -> if checkLeftSpacing tok
        then discoverLayouts__ (tok:buff) toks out
        else discoverLayouts__ mempty rest out' where
            (rest,nonSpaced) = (tok:) <$> takeWhileUnspaced toks
            out' = case buff of
                (firstNonSpaced:buff')
                   -> unspaced (firstNonSpaced : nonSpaced) (spaced buff' out)
                [] -> unspaced nonSpaced out
    where submitIfNotNull f s = if null s then id else ((:) . f) s
          spaced   = submitIfNotNull (Layouted SpacedLayout . reverse)
          unspaced = submitIfNotNull (Layouted UnspacedLayout)

takeWhileUnspaced :: [Ast] -> ([Ast], [Ast])
takeWhileUnspaced = go where
    go = \stream -> case stream of
        [] -> (mempty, mempty)
        (a:as) -> if checkLeftSpacing a
            then (stream, mempty)
            else (a:) <$> go as
{-# INLINE takeWhileUnspaced #-}


-- === Utils === --

checkLeftSpacing :: Ast -> Bool
checkLeftSpacing = (> 0) . view (Ast.span . CodeSpan.viewSpan . Span.offset)
{-# INLINE checkLeftSpacing #-}



----------------------------------
-- === Assignment partition === --
----------------------------------

-- | Divide token stream into MacroParser and Expr.
--
--   For the first sight it could seem strange that we convert '[Layouted]'
--   to 'Statement' just to convert it back to '[Layouted]' using the
--   'flattenStatement' in the very next step, however the design is
--   future proof. We would like to first parse some section of the file, then
--   discover all new mixfix declarations, update scope information and then
--   parse their bodies, so in the future tere should be some more steps between
--   the conversion 'Statement' to '[Layouted]'.

-- === Definition === --

data Statement
    = ExpressionStatement [Layouted]
    | AssignmentStatement [Layouted] Ast [Layouted]
    deriving (Show)


-- === API === --

buildStatement :: [Layouted] -> Statement
buildStatement = buildStatement__ id
{-# INLINE buildStatement #-}

-- | Note: see description above to learn more why it's useful.
buildFlatStatement :: [Layouted] -> [Layouted]
buildFlatStatement = flattenStatement . buildStatement
{-# INLINE buildFlatStatement #-}

buildStatement__ ::
    ([Layouted] -> [Layouted]) -> [Layouted] -> Statement
buildStatement__ = \f stream -> case stream of
    []         -> ExpressionStatement $! f mempty
    (tok:toks) -> let
        Layouted layout ss = tok
        continue = buildStatement__ (f . (tok:)) toks
        in case layout of
            UnspacedLayout -> continue
            SpacedLayout   -> case breakOnAssignment ss of
                Left {} -> continue
                Right (l, x, r)  -> let
                    mod a = if null a then id else (Layouted SpacedLayout a :)
                    in AssignmentStatement (mod l $ f mempty) x (mod r toks)
{-# NOINLINE buildStatement__ #-}

breakOnAssignment :: [Ast] -> Either [Ast] ([Ast], Ast, [Ast])
breakOnAssignment = breakOnAssignment__ id
{-# INLINE breakOnAssignment #-}

breakOnAssignment__
    :: ([Ast] -> [Ast]) -> [Ast] -> Either [Ast] ([Ast], Ast, [Ast])
breakOnAssignment__ = \f stream -> case stream of
    []         -> Left (f stream)
    (tok:toks) -> let
        continue = breakOnAssignment__ (f . (tok:)) toks
        in case Ast.unspan tok of
            Ast.Operator name ->
                if name == Name.unify
                    then Right (f mempty, tok, toks)
                    else continue
            _ -> continue
{-# NOINLINE breakOnAssignment__ #-}

flattenStatement :: Statement -> [Layouted]
flattenStatement = \case
    ExpressionStatement  s -> s
    AssignmentStatement p x s -> let
        op' = Ast.Operator Name.assign
        op  = x & Ast.ast .~ op'
        sop = Layouted SpacedLayout [op]
        in p <> (sop : s)
{-# INLINE flattenStatement #-}



-------------------------------
-- === Expr token stream === --
-------------------------------

-- | Type safe definition of AST token stream. We assume that there is always an
--   operator between two non-operators. It is true if we assume that the white
--   space is a special case of the '#app#' operator. The following 'Stream'
--   definition has all guarantees buil-in regarding the structure of the stream
--   including operator sections on the beginning and end of the stream.


-- === Definition === --

-- | The following structures are optimized for fast elem access times. In order
--   to understand how they work, refer to their simplifier, logically
--   equivalent form below:

--   > data ElStream
--   >     = InfixElStream Name OpStream
--   >     | EndElStream   Name
--   >
--   > data OpStream
--   >     = InfixOpStream Name (Ast -> Ast -> Ast) ElStream
--   >     | EndOpStream   Name (Ast -> Ast)
--

data Stream       = OpStreamStart Name (Ast -> Ast) ElStream
                  | ElStreamStart ElStream
                  | NullStream

data OpStream     = OpStream Name OpStreamType
data OpStreamType = InfixOp  (Ast -> Ast -> Ast) ElStream
                  | EndOp    (Ast -> Ast)

pattern InfixOpStream name f stream = OpStream name (InfixOp f stream)
pattern EndOpStream   name f        = OpStream name (EndOp f)
pattern InfixOp_      f ast stream  = InfixOp f (ElStream ast stream)

data ElStream     = ElStream Ast ElStreamType
data ElStreamType = InfixEl  OpStream
                  | EndEl

pattern InfixElStream ast  stream = ElStream ast (InfixEl stream)
pattern EndElStream   ast         = ElStream ast EndEl
pattern InfixEl_      name stream = InfixEl (OpStream name stream)

deriving instance Show Stream
deriving instance Show OpStream
deriving instance Show OpStreamType
deriving instance Show ElStream
deriving instance Show ElStreamType


-- === Smart constructors === --

singularElStream :: Ast -> Stream
singularElStream = ElStreamStart . EndElStream
{-# INLINE singularElStream #-}


-- === Construction === --

-- | Stream builder. Note that its implementation works on reversed token
--   stream in order to get tail-recursive optimization here.

buildStream :: [Ast] -> Stream
buildStream = \(reverse -> stream) -> case stream of
    []         -> NullStream
    (tok:toks) -> case Ast.unspan tok of
        Ast.Operator name
          -> buildElStream (EndOpStream name (rightSection tok)) toks
        _ -> buildOpStream (EndElStream tok) toks
{-# INLINE buildStream #-}

buildOpStream :: ElStream -> [Ast] -> Stream
buildOpStream = \result stream -> case stream of
    [] -> ElStreamStart result
    _  -> case takeOperators stream of
        ([(name,op)], []  ) -> OpStreamStart name (leftSection op) result
        ([]         , _   ) -> go Name.app     (Ast.app)      result stream
        ([(name,op)], toks) -> go name         (Ast.app2 op)  result toks
        ((p:ps)     , toks) -> go Name.invalid (Ast.app2 inv) result toks
            where inv = Ast.computeCodeSpanList1__ (snd <$> (p :| ps))
                      $ Ast.Invalid Invalid.AdjacentOperators
                      -- FIXME: register ops in invalid
    where go name f stream = buildElStream (InfixOpStream name f stream)
{-# NOINLINE buildOpStream #-}

buildElStream :: OpStream -> [Ast] -> Stream
buildElStream = \(result@(OpStream name stp)) -> \case
    [] -> case stp of
        EndOp   f   -> singularElStream $ f Ast.missing
        InfixOp f s -> OpStreamStart name (f Ast.missing) s
    (tok:toks) -> case Ast.unspan tok of
        Ast.AstOperator {} -> undefined
        _                  -> buildOpStream (InfixElStream tok result) toks
{-# NOINLINE buildElStream #-}


-- === Utils === --

leftSection :: Ast -> Ast -> Ast
leftSection = \a -> Ast.app2 a Ast.missing
{-# INLINE leftSection #-}

rightSection :: Ast -> Ast -> Ast
rightSection = \a -> flip (Ast.app2 a) Ast.missing
{-# INLINE rightSection #-}

takeOperators :: [Ast] -> ([(Name,Ast)], [Ast])
takeOperators = takeOperators__ mempty
{-# INLINE takeOperators #-}

takeOperators__ :: [(Name,Ast)] -> [Ast] -> ([(Name,Ast)], [Ast])
takeOperators__ = \result stream -> case stream of
    [] -> (result,[])
    (tok:toks) -> case Ast.unspan tok of
        Ast.Operator name
          -> takeOperators__ ((name,tok) : result) toks
        _ -> (result, stream)
{-# NOINLINE takeOperators__ #-}



--------------------------------
-- === Expression builder === --
--------------------------------

-- | The following code is a Shunting-yard algorithm modified to support
--   operator sectioning and automatic error recovery based on a type secure
--   token stream implementation. For more information please refer to the
--   Wikipedia entry: https://en.wikipedia.org/wiki/Shunting-yard_algorithm


-- === API === --

type ExprBuilderMonad m = (Assoc.Reader Name m, Prec.RelReader Name m)

buildExpr :: ExprBuilderMonad m => [Ast] -> m Ast
buildExpr = \stream -> do
    let layouted  = discoverLayouts stream
        statement = buildFlatStatement layouted
    stream' <- buildUnspacedExprs statement
    buildExprList stream'
{-# INLINE buildExpr #-}


-- === Utils === --

buildUnspacedExprs :: ExprBuilderMonad m => [Layouted] -> m [Ast]
buildUnspacedExprs = \stream -> let
    go = \(Layouted layout s) -> case layout of
        SpacedLayout   -> pure s
        UnspacedLayout -> pure <$> buildExprList s
    in concat <$> mapM go stream
{-# INLINE buildUnspacedExprs #-}

buildExprList :: ExprBuilderMonad m => [Ast] -> m Ast
buildExprList = buildExprStream . buildStream
{-# INLINE buildExprList #-}

buildExprStream :: ExprBuilderMonad m => Stream -> m Ast
buildExprStream = \case
    NullStream -> pure $ Ast.invalid Invalid.EmptyExpression
    OpStreamStart opName opFunc (ElStream el stp) -> case stp of
        EndEl     -> pure (opFunc el)
        InfixEl s -> buildExprOp__ s
                   $ InfixElStream el (EndOpStream opName opFunc)
    ElStreamStart (ElStream el stp) -> case stp of
        EndEl     -> pure el
        InfixEl s -> buildExprOp__ s (EndElStream el)
{-# INLINE buildExprStream #-}

buildExprOp__ :: ∀ m. ExprBuilderMonad m => OpStream -> ElStream -> m Ast
buildExprOp__ = \stream stack -> let
    OpStream streamOp streamTp = stream
    ElStream stackEl  stackTp  = stack

    go :: ElStreamType -> ElStream -> m Ast
    go streamTp newStack = case streamTp of
        EndEl           -> foldExprStack__ newStack
        InfixEl stream' -> buildExprOp__ stream' newStack

    submitToStack :: m Ast
    submitToStack = case streamTp of
        EndOp opF -> foldExprStackWithOp__ streamOp opF stackEl stackTp
        InfixOp_ f streamEl streamTp' -> go streamTp' newStack where
            newStack = InfixElStream streamEl (InfixOpStream streamOp f stack)
    {-# INLINE submitToStack #-}

    reduceStack :: OpStreamType -> m Ast
    reduceStack = \case
        EndOp f -> buildExprOp__ stream newStack where
            newStack = EndElStream (f stackEl)
        InfixOp_ f stackEl2 s -> buildExprOp__ stream newStack where
            newStack = ElStream (f stackEl2 stackEl) s
    {-# INLINE reduceStack #-}

    in case stackTp of
        EndEl -> submitToStack
        InfixEl_ stackOp stack' ->
            Prec.readRel stackOp streamOp >>= \case
                -- FIXME Just
                Just LT -> submitToStack
                Just GT -> reduceStack stack'
                Just EQ -> do
                    assoc  <- Assoc.read stackOp
                    assoc' <- Assoc.read streamOp
                    if (assoc == assoc' && assoc == Assoc.Left)
                        then reduceStack stack'
                        else submitToStack
                x -> error $ ppShow (stackOp, streamOp, x)
{-# NOINLINE buildExprOp__ #-}

foldExprStack__ :: ExprBuilderMonad m => ElStream -> m Ast
foldExprStack__ = \(ElStream el stp) -> case stp of
    EndEl -> pure el
    InfixEl_ op stp2 -> case stp2 of
        EndOp opF             -> pure $ opF el
        InfixOp_ opF el2 stp3 -> foldExprStackWithOp__ op (flip opF el) el2 stp3
{-# NOINLINE foldExprStack__ #-}

foldExprStackWithOp__ :: ExprBuilderMonad m
    => Name -> (Ast -> Ast) -> Ast -> ElStreamType -> m Ast
foldExprStackWithOp__ = \op opF el stream -> case stream of
    EndEl -> pure $ opF el
    InfixEl s@(OpStream op2 stream2) -> checkCorrectOpRel op op2 >>= \case
        True  -> foldExprStack__ $ InfixElStream (opF el) s
        False -> let tailOps = [el, opF Ast.missing] in case stream2 of
            EndOp op2F -> pure . assocConflict $ op2F Ast.missing :| tailOps
            InfixOp_ op2F el2 stream3 -> case stream3 of
                EndEl     -> pure inv
                InfixEl s -> foldExprStack__ (InfixElStream inv s)
                where inv = assocConflict $ el2 :| (fillMissing op2F : tailOps)
{-# NOINLINE foldExprStackWithOp__ #-}

checkCorrectOpRel :: ExprBuilderMonad m => Name -> Name -> m Bool
checkCorrectOpRel = \op1 op2 -> do
    -- FIXME Just
    Just prec   <- Prec.readRel op1 op2
    assoc  <- Assoc.read op1
    assoc' <- Assoc.read op2
    pure . not $ (prec == EQ) && (assoc /= assoc' || assoc == Assoc.None)
{-# INLINE checkCorrectOpRel #-}

assocConflict :: NonEmpty Ast -> Ast
assocConflict = \elems
    -> Ast.computeCodeSpanList1__ elems $ Ast.Invalid Invalid.AssocConflict
{-# INLINE assocConflict #-}

fillMissing :: (Ast -> Ast -> Ast) -> Ast
fillMissing = \f -> f Ast.missing Ast.missing
{-# INLINE fillMissing #-}




-- data Section = Section Segment MacroSegments
--     deriving (Show)

-- data MacroSegments
--     = Required Segment MacroSegments
--     | Optional Segment MacroSegments
--     | SectionListNull
--     deriving (Show)

-- data Segment = Segment
--     { _name   :: Ast.Ast
--     , _chunks :: [MacroParser]
--     } deriving (Show)
-- makeLenses ''Segment


-- importDef
--     = syntax "import" [Expr]















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
-- Custom parsing rules allow altering the syntax completely. In barebone parser
-- everything is just an expression containing identifiers and operators
-- separated by spaces. Any more complex construction like `type Vector a ...`
-- is defined as custom parsing rule and is translated to appriopriate macro
-- call.
--
-- All custom parsing rules need to be defined in a separate compilation unit,
-- let it be a file or other module required to be imported. Otherwise after
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
