{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Pass.Parsing.ExprBuilder where

import Prologue

import qualified Data.Graph.Component.Node.Destruction     as Component
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

import Data.Text.Position                    (Delta (Delta))
import Luna.Syntax.Text.Parser.Data.CodeSpan (CodeSpan)
import Luna.Syntax.Text.Parser.Data.Invalid  (Invalids)
import Luna.Syntax.Text.Parser.Data.Result   (Result)
import Luna.Syntax.Text.Parser.IR.Ast        (Spanned (Spanned))
import Luna.Syntax.Text.Parser.IR.Term       (Ast)
import Luna.Syntax.Text.Source               (Source)
import OCI.Data.Name                         (Name)

import Data.Parser hiding (Result)


import Data.Attoparsec.List ()

(<$$>) :: Monad m => (a -> m b) -> [a] -> m [b]
(<$$>) = mapM
{-# INLINE (<$$>) #-}



data ExprBuilder
-- FIXME: Contraints like `Component.Delete` should not be so explicit
type ExprBuilderPass m = (Pass.Interface ExprBuilder m, Component.Delete m)
type instance Pass.Spec ExprBuilder t = Spec t
type family   Spec  t where
    Spec (Pass.In  Pass.Attrs) = '[Source, Result, Invalids]
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
    Ast.AstCons      (Ast.Cons      name) -> IR.cons'  name []
    Ast.AstVar       (Ast.Var       name) -> IR.var'   name
    Ast.AstOperator  (Ast.Operator  name) -> IR.var'   name
    Ast.AstWildcard  {}                   -> IR.blank'
    Ast.AstLineBreak (Ast.LineBreak ind)  -> IR.lineBreak' (unwrap ind)
    Ast.AstInvalid   (Ast.Invalid   inv)  -> IR.invalid' inv
    Ast.AstTokens    (Ast.Tokens (t:ts))  -> buildGraph t -- FIXME
    Ast.AstMissing   (Ast.Missing)        -> IR.missing'
    Ast.AstApp       (Ast.App f a)        -> do
        f' <- buildGraph f
        a' <- buildGraph a
        IR.app' f' a'
    x                                     -> error $ "TODO: " <> show x
    where addCodeSpan cs ir = ir <$ IR.writeLayer @CodeSpan ir cs
{-# INLINE buildGraph #-}


-- data Spacing = L | R | LR deriving (Show)

data Placement
    = Section (Ast -> Ast)
    | Normal  (Ast -> Ast -> Ast)
    deriving (Show)

data Operator = Operator
    { _name :: Name
    , _func :: Placement
    } deriving (Show)

makeLenses ''Operator

comparePrec :: (Monad m, Prec.RelReader Name m)
    => Operator -> Operator -> m Ordering
comparePrec = \op1 op2 -> do
    let name1 = op1 ^. name
        name2 = op2 ^. name
    Prec.readRel name1 name2

checkLeftSpacing :: Ast -> Bool
checkLeftSpacing = (> 0) . view (Ast.span . CodeSpan.viewSpan . Span.offset)
{-# INLINE checkLeftSpacing #-}



------------------------------------------------
-- === Spaced / Unspaced stream partition === --
------------------------------------------------

data SpaceStream
    = SpacedStream   [Ast]
    | UnspacedStream [Ast]
    deriving (Show)

-- | Divide token stream to spaced and unspaced substreams.
subStreams :: [Ast] -> [SpaceStream]
subStreams = \case
    []     -> []
    (a:as) -> subStreams' [a] as []
{-# INLINE subStreams #-}

subStreams' :: [Ast] -> [Ast] -> [SpaceStream] -> [SpaceStream]
subStreams' = \buff stream out -> case stream of
    [] -> reverse $ spaced buff out
    (tok:toks) -> if checkLeftSpacing tok
        then subStreams' (tok:buff) toks out
        else let
            (rest,nonSpaced) = (tok:) <$> subNonSpacedStream toks
            out' = case buff of
                []                     -> unspaced nonSpaced out
                (firstNonSpaced:buff') -> unspaced (firstNonSpaced : nonSpaced)
                                        $ spaced buff' out
            in subStreams' [] rest out'
    where submitIfNotNull f s = if null s then id else ((:) . f) s
          spaced   = submitIfNotNull (SpacedStream . reverse)
          unspaced = submitIfNotNull (UnspacedStream)

subNonSpacedStream :: [Ast] -> ([Ast], [Ast])
subNonSpacedStream = go where
    go = \stream -> case stream of
        [] -> ([], [])
        (a:as) -> if checkLeftSpacing a
            then (stream, [])
            else (a:) <$> go as
{-# INLINE subNonSpacedStream #-}



-------------------------------
-- === Expr token stream === --
-------------------------------

-- === Definition === --

data Stream       = StreamOpStart Name (Ast -> Ast) ElStream
                  | StreamElStart ElStream
                  | NullStream

data OpStream     = OpStream      Name OpStreamType
data OpStreamType = InfixOpStream (Ast -> Ast -> Ast) ElStream
                  | EndOpStream   (Ast -> Ast)

data ElStream     = ElStream      Ast ElStreamType
data ElStreamType = InfixElStream OpStream
                  | EndElStream

deriving instance Show Stream
deriving instance Show OpStream
deriving instance Show OpStreamType
deriving instance Show ElStream
deriving instance Show ElStreamType


-- === Smart constructors === --

infixOpStream :: Name -> (Ast -> Ast -> Ast) -> ElStream -> OpStream
infixOpStream = \name f stream -> OpStream name $ InfixOpStream f stream
{-# INLINE infixOpStream #-}

endOpStream :: Name -> (Ast -> Ast) -> OpStream
endOpStream = \name f -> OpStream name $ EndOpStream f
{-# INLINE endOpStream #-}

infixElStream :: Ast -> OpStream -> ElStream
infixElStream = \ast stream -> ElStream ast $ InfixElStream stream
{-# INLINE infixElStream #-}

endElStream :: Ast -> ElStream
endElStream = \ast -> ElStream ast EndElStream
{-# INLINE endElStream #-}

singularElStream :: Ast -> Stream
singularElStream = StreamElStart . endElStream
{-# INLINE singularElStream #-}


-- === Construction === --

-- | Stream builder. Note that the stream builder works on reversed token
--   stream in order to get tail-recursive utils here.

buildStream :: [Ast] -> Stream
buildStream = \(reverse -> stream) -> case stream of
    []         -> NullStream
    (tok:toks) -> case Ast.unspan tok of
        Ast.AstOperator (Ast.Operator name)
          -> buildStreamEl (endOpStream name (rightSection tok)) toks
        _ -> buildStreamOp (endElStream tok) toks
{-# INLINE buildStream #-}

buildStreamOp :: ElStream -> [Ast] -> Stream
buildStreamOp = \result stream -> case stream of
    [] -> StreamElStart result
    _  -> case takeOperators stream of
        ([(name,op)], []  ) -> StreamOpStart name (leftSection op) result
        ([]         , _   ) -> go Name.app     (Ast.app)      result stream
        ([(name,op)], toks) -> go name         (Ast.app2 op)  result toks
        ((p:ps)     , toks) -> go Name.invalid (Ast.app2 inv) result toks
            where inv = Ast.inheritCodeSpanList' (snd <$> (p :| ps))
                      $ Ast.invalid' Invalid.AdjacentOperators
                      -- FIXME: register ops in invalid
    where go name f stream = buildStreamEl (infixOpStream name f stream)
{-# NOINLINE buildStreamOp #-}

buildStreamEl :: OpStream -> [Ast] -> Stream
buildStreamEl = \(result@(OpStream name stp)) -> \case
    [] -> case stp of
        EndOpStream   f   -> singularElStream $ f Ast.missing
        InfixOpStream f s -> StreamOpStart name (f Ast.missing) s
    (tok:toks) -> case Ast.unspan tok of
        Ast.AstOperator {} -> undefined
        _                  -> buildStreamOp (infixElStream tok result) toks
{-# NOINLINE buildStreamEl #-}


-- === Utils === --

leftSection :: Ast -> Ast -> Ast
leftSection = \a -> Ast.app2 a Ast.missing
{-# INLINE leftSection #-}

rightSection :: Ast -> Ast -> Ast
rightSection = \a -> flip (Ast.app2 a) Ast.missing
{-# INLINE rightSection #-}

takeOperators :: [Ast] -> ([(Name,Ast)], [Ast])
takeOperators = takeOperators' mempty
{-# INLINE takeOperators #-}

takeOperators' :: [(Name,Ast)] -> [Ast] -> ([(Name,Ast)], [Ast])
takeOperators' = \result stream -> case stream of
    [] -> (result,[])
    (tok:toks) -> case Ast.unspan tok of
        Ast.AstOperator (Ast.Operator name) -> takeOperators' ((name,tok) : result) toks
        _                  -> (result, stream)
{-# NOINLINE takeOperators' #-}




--------------------------------
-- === Expression builder === --
--------------------------------

buildExpr:: (Monad m, Assoc.Reader Name m, Prec.RelReader Name m)
    => [Ast] -> m Ast
buildExpr = \tokStream -> buildExpr' True False tokStream [] []

buildExpr' :: (Monad m, Assoc.Reader Name m, Prec.RelReader Name m)
    => Bool -> Bool -> [Ast] -> [Operator] -> [Ast] -> m Ast
buildExpr' = \isFirst lastWasEl tokStream opStack elStack -> let
    go        = buildExpr' False
    goAfterEl = go True
    goAfterOp = go False
    lastWasOp = not lastWasEl
    in case tokStream of
        [] -> pure $ finishExpr opStack elStack
        (tok:toks) -> case Ast.unspan tok of
            Ast.AstOperator (Ast.Operator name) -> do
                let isLast = null toks
                    opFunc = Ast.app2 tok
                    newOp  = Operator name fixity
                    fixity = if
                           | isFirst   -> Section (opFunc Ast.missing)
                           | isLast    -> Section (flip opFunc Ast.missing)
                           | otherwise -> Normal opFunc
                Assoc.read name -- FIXME
                case opStack of
                    []       -> goAfterOp toks (newOp:opStack) elStack
                    (op:ops) -> do
                        comparePrec op newOp >>= \case
                            LT -> goAfterOp toks (newOp:opStack) elStack
                            _  -> goAfterOp tokStream ops
                                $ applyOpToStack op elStack
            _ -> let
                appOperator = Operator Name.app (Normal Ast.app)
                opStackMod  = if lastWasEl then (appOperator:) else id
                in goAfterEl toks (opStackMod opStack) (tok:elStack)



buildExpr2 :: (Monad m, Assoc.Reader Name m, Prec.RelReader Name m)
    => Stream -> m Ast
buildExpr2 = \case
    NullStream -> error "TODO"
    StreamOpStart opName opFunc (ElStream el stp) -> case stp of
        EndElStream     -> error "TODO"
        InfixElStream s -> buildExprOp s (infixElStream el (endOpStream opName opFunc))
    x -> error $ ppShow x


buildExprOp :: (Monad m, Assoc.Reader Name m, Prec.RelReader Name m)
    => OpStream -> ElStream -> m Ast
buildExprOp = \stream stack -> let
    OpStream streamOp streamTp = stream
    ElStream stackEl  stackTp  = stack

    in case stackTp of
        EndElStream     -> error "TODO"
        InfixElStream (OpStream stackOp stackTp') -> do
            comparePrec2 stackOp streamOp >>= \case
                LT -> case streamTp of
                    InfixOpStream f (ElStream streamEl streamTp') -> case streamTp' of
                        EndElStream -> let
                            stack' = infixElStream streamEl
                                   $ infixOpStream streamOp f stack
                            in pure $ finishExprEl stack'
                        x -> error $ ppShow x
                    x -> error $ ppShow x
                x -> error $ ppShow x

        x -> error $ ppShow x

    -- go        = buildExpr' False
    -- goAfterEl = go True
    -- goAfterOp = go False
    -- lastWasOp = not lastWasEl
    -- OpStream op stream' = stream
    -- in case streamTp of
    --     InfixOpStream f (ElStream el' streamTp') -> case stackTp of
    --         InfixElStream (OpStream name stream') -> case stream' of
    --             EndOpStream f -> do

    --             x -> error $ ppShow x
    --                     -- stack' = infixElStream el' (infixOpStream name f stack)
    --                     -- in pure $ finishExprEl stack'
    --         -- InfixElStream stream' -> case stackTp of
    --         --     EndElStream -> let
    --         --         stack' = infixElStream el' (infixOpStream name f stack)
    --         --         in buildExprOp stream' stack'
    --         x -> error $ ppShow x
    --     x -> error $ ppShow x
    --     -- EndOpStream ->

    --     [] -> pure $ finishExpr opStack elStack
    --     (tok:toks) -> case Ast.unspan tok of
    --         Ast.AstOperator (Ast.Operator name) -> do

    --             -- if lastWasOp

    --             let isLast = null toks
    --                 opFunc = Ast.app2 tok
    --                 newOp  = Operator name fixity
    --                 fixity = if
    --                        | isFirst   -> Section (opFunc Ast.missing)
    --                        | isLast    -> Section (flip opFunc Ast.missing)
    --                        | otherwise -> Normal opFunc
    --             Assoc.read name -- FIXME
    --             case opStack of
    --                 []       -> goAfterOp toks (newOp:opStack) elStack
    --                 (op:ops) -> do
    --                     comparePrec op newOp >>= \case
    --                         LT -> goAfterOp toks (newOp:opStack) elStack
    --                         _  -> goAfterOp tokStream ops
    --                             $ applyOpToStack op elStack
    --         _ -> let
    --             appOperator = Operator Name.app (Normal Ast.app)
    --             opStackMod  = if lastWasEl then (appOperator:) else id
    --             in goAfterEl toks (opStackMod opStack) (tok:elStack)


-- data Stream       = StreamOpStart Name (Ast -> Ast) ElStream
--                   | StreamElStart ElStream
--                   | NullStream

-- data OpStream     = OpStream      Name OpStreamType
-- data OpStreamType = InfixOpStream (Ast -> Ast -> Ast) ElStream
--                   | EndOpStream   (Ast -> Ast)

-- data ElStream     = ElStream      Ast ElStreamType
-- data ElStreamType = InfixElStream OpStream
--                   | EndElStream


finishExprEl :: ElStream -> Ast
finishExprEl = \(ElStream a stp) -> case stp of
    EndElStream -> a
    InfixElStream (OpStream name stp) -> case stp of
        EndOpStream   f                   -> f a
        InfixOpStream f (ElStream b stp') -> case stp' of
            EndElStream     -> f b a
            InfixElStream s -> finishExprEl $ infixElStream (f b a) s
{-# NOINLINE finishExprEl #-}



finishExpr :: [Operator] -> [Ast] -> Ast
finishExpr = \opStack elStack ->
    case opStack of
        (op:ops) -> finishExpr ops (applyOpToStack op elStack)
        [] -> case elStack of
            [a] -> a
            x   -> error $ ppShow x
{-# INLINE finishExpr #-}

applyOpToStack :: Operator -> [Ast] -> [Ast]
applyOpToStack = \op elStack -> case op ^. func of
    Normal f -> case elStack of
        (a:b:elStack') -> f b a : elStack'
        -- FIXME
    Section f -> case elStack of
        (a:elStack') -> f a : elStack'
        -- FIXME
{-# INLINE applyOpToStack #-}

comparePrec2 :: (Monad m, Prec.RelReader Name m)
    => Name -> Name -> m Ordering
comparePrec2 = Prec.readRel


infixl 2 **<
(**<) = (+)

infixr 5 **>
(**>) = (+)

foo :: Int
foo = 5 **< 10 **> 12


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
