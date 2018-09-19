{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Text.Parser.Parser.ExprBuilder where

import Prologue hiding (optional)

import qualified Control.Monad.State.Layered           as State
import qualified Data.Graph.Component.Node.Destruction as Component
import qualified Data.Map                              as Map
import qualified Data.Set                              as Set
import qualified Data.Text.Span                        as Span
import qualified Language.Symbol.Operator.Assoc        as Assoc
import qualified Language.Symbol.Operator.Prec         as Prec
import qualified Luna.IR                               as IR
import qualified Luna.IR.Aliases                       as Uni
import qualified Luna.IR.Term.Ast.Invalid              as Invalid
import qualified Luna.Pass                             as Pass
import qualified Luna.Syntax.Text.Parser.Ast           as Ast
import qualified Luna.Syntax.Text.Parser.Ast.CodeSpan  as CodeSpan
import qualified Luna.Syntax.Text.Parser.Lexer.Names   as Name

import Data.Map                             (Map)
import Data.Set                             (Set)
import Data.Text.Position                   (Delta (Delta))
import Luna.Syntax.Text.Parser.Ast          (Spanned (Spanned))
import Luna.Syntax.Text.Parser.Ast.CodeSpan (CodeSpan)
import Luna.Syntax.Text.Parser.Lexer        (Token)
import Luna.Syntax.Text.Source              (Source)
import OCI.Data.Name                        (Name)

-- import Data.Parser hiding (Result)


import Data.Attoparsec.List ()






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
    , _stream :: [Token]
    } deriving (Show)
makeLenses ''Layouted


-- === API === --

discoverLayouts :: [Token] -> [Layouted]
discoverLayouts = \case
    []     -> []
    (a:as) -> discoverLayouts__ [a] as []
{-# INLINE discoverLayouts #-}

discoverLayouts__ :: [Token] -> [Token] -> [Layouted] -> [Layouted]
discoverLayouts__ = \buff stream out -> case stream of
    [] -> reverse $ spaced buff out
    (tok:toks) -> if checkLeftSpacing' tok buff
        then discoverLayouts__ (tok:buff) toks out
        else discoverLayouts__ mempty rest out' where
            (rest,nonSpaced) = (tok:) <$> takeWhileUnspaced toks
            out' = case buff of
                (firstNonSpaced:buff')
                   -> unspaced (firstNonSpaced : nonSpaced) (spaced buff' out)
                [] -> unspaced nonSpaced out
    where make f s = if null s then id else ((:) . f . discoverOpAccessors) s
          spaced   = make (Layouted SpacedLayout)   . reverse
          unspaced = make (Layouted UnspacedLayout) . discoverUMinus

takeWhileUnspaced :: [Token] -> ([Token], [Token])
takeWhileUnspaced = go where
    go = \stream -> case stream of
        [] -> (mempty, mempty)
        (a:as) -> if checkLeftSpacing a
            then (stream, mempty)
            else (a:) <$> go as
{-# INLINE takeWhileUnspaced #-}


-- | Unary minus is the only prefix operator and needs to be handled correctly.
--   We scan heads of all unspaced layouts to discover it.
discoverUMinus :: [Token] -> [Token]
discoverUMinus = \case
    []     -> mempty
    (a:as) -> a' : as where
        a' = case Ast.unspan a of
            Ast.Operator "-" -> a & Ast.ast .~ Ast.Operator Name.uminus
            _                -> a
{-# INLINE discoverUMinus #-}

-- | Any operator following the accessor operator should be treated as var. We
--   can discover such cases only after layouts are build in order to properly
--   handle all sections.
discoverOpAccessors :: [Token] -> [Token]
discoverOpAccessors = \p -> case p of
    []     -> []
    (a:as) -> case Ast.unspan a of
        Ast.Operator "." -> case as of
            []     -> p
            (t:ts) -> case Ast.unspan t of
                Ast.Operator op -> a : (t & Ast.ast .~ Ast.Var op)
                                     : discoverOpAccessors ts
                _               -> a : t : discoverOpAccessors ts
        _ -> a : discoverOpAccessors as



-- === Utils === --

-- FIXME
-- This is a hack. We assume that ':' is always left-spaced so it will not
-- create a section in expressions like `foo = a: b: a + b`. The problem is
-- that this makes this operator a very special one. There is no such problem
-- in the new syntax so as soon as we switch to it, we should remove this hack.
checkLeftSpacing :: Token -> Bool
checkLeftSpacing = \a -> normal a || special a where
    normal  = (> 0) . view (Ast.span . CodeSpan.viewSpan . Span.offset)
    special = (== Ast.Operator ":") . view Ast.ast
{-# INLINE checkLeftSpacing #-}

checkLeftSpacing' :: Token -> [Token] -> Bool
checkLeftSpacing' = \a buff -> checkLeftSpacing a || checkLamBuff buff where
    checkLamBuff = \case
        (tok:toks) -> Ast.unspan tok == Ast.Operator ":"
        _ -> False
{-# INLINE checkLeftSpacing' #-}



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
    = ExpressionStatement [Token]
    | AssignmentStatement [Token] Token [Token]
    deriving (Show)


-- === API === --

buildStatement :: [Token] -> Statement
buildStatement = either ExpressionStatement (uncurry AssignmentStatement) . breakOnAssignment
{-# INLINE buildStatement #-}

breakOnAssignment :: [Token] -> Either [Token] ([Token], Token, [Token])
breakOnAssignment = breakOnAssignment__ id
{-# INLINE breakOnAssignment #-}

breakOnAssignment__
    :: ([Token] -> [Token]) -> [Token] -> Either [Token] ([Token], Token, [Token])
breakOnAssignment__ = \f stream -> case stream of
    []         -> Left (f stream)
    (tok:toks) -> let
        continue = breakOnAssignment__ (f . (tok:)) toks
        in case Ast.unspan tok of
            Ast.Operator name ->
                if name == Name.rawAssign
                    then let
                        op = tok & Ast.ast .~ Ast.Operator Name.assign
                        in Right (f mempty, op, toks)
                    else continue
            _ -> continue
{-# NOINLINE breakOnAssignment__ #-}



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
--   >     = InfixOpStream Name (Token -> Token -> Token) ElStream
--   >     | EndOpStream   Name (Token -> Token)
--

data Stream       = OpStreamStart Name Token ElStream
                  | ElStreamStart ElStream
                  | NullStream

data OpStream     = OpStream Name OpStreamType
data OpStreamType = InfixOp  (Maybe Token) ElStream
                  | EndOp    Token

pattern InfixOpStream name mop stream = OpStream name (InfixOp mop stream)
pattern EndOpStream   name op         = OpStream name (EndOp op)
pattern InfixOp_      mop ast stream  = InfixOp  mop  (ElStream ast stream)

data ElStream     = ElStream Token ElStreamType
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

singularElStream :: Token -> Stream
singularElStream = ElStreamStart . EndElStream
{-# INLINE singularElStream #-}


-- === Construction === --

-- | Stream builder. Note that its implementation works on reversed token
--   stream in order to get tail-recursive optimization here.

buildStream :: [Token] -> Stream
buildStream = \(reverse -> stream) -> case stream of
    []         -> NullStream
    (tok:toks) -> case discoverOps stream of
        Single name op    -> goEl name op toks
        NotFound          -> goOp tok toks
        Invalid inv toks' -> goEl Name.invalid inv toks'
    where goOp      = buildOpStream . EndElStream
          goEl name = buildElStream . EndOpStream name
{-# INLINE buildStream #-}

buildOpStream :: ElStream -> [Token] -> Stream
buildOpStream = \result stream -> case stream of
    [] -> ElStreamStart result
    (tok:toks) -> case discoverOps stream of
        NotFound          -> go Name.app     Nothing    result stream
        Invalid inv toks' -> go Name.invalid (Just inv) result toks'
        Single name op    -> go name         (Just op)  result toks
    where go = buildElStream .:. InfixOpStream
{-# NOINLINE buildOpStream #-}

buildElStream :: OpStream -> [Token] -> Stream
buildElStream = \(result@(OpStream name stp)) -> \case
    [] -> case stp of
        EndOp   op    -> singularElStream op
        InfixOp mop s -> let
            Just op = mop -- FIXME: this is impossible, but how to prove it?
            in OpStreamStart name op s
    (tok:toks) -> buildOpStream (InfixElStream tok result) toks
{-# NOINLINE buildElStream #-}



-- === Operator discovery === --

data OperatorDiscovery
    = Single Name Token
    | Invalid Token [Token]
    | NotFound
    deriving (Show)

discoverOps :: [Token] -> OperatorDiscovery
discoverOps stream = case takeOperators stream of
    ([(name,op)], _)     -> Single name op
    ([]         , _)     -> NotFound
    ((p:ps)     , toks') -> Invalid inv toks'
        where inv = Ast.computeCodeSpanList1__ (snd <$> (p :| ps))
                  $ Ast.Invalid Invalid.AdjacentOperators
{-# INLINE discoverOps #-}

takeOperators :: [Token] -> ([(Name,Token)], [Token])
takeOperators = takeOperators__ mempty
{-# INLINE takeOperators #-}

takeOperators__ :: [(Name,Token)] -> [Token] -> ([(Name,Token)], [Token])
takeOperators__ = \result stream -> case stream of
    [] -> (result,[])
    (tok:toks) -> let
        go name = takeOperators__ ((name,tok) : result) toks
        in case Ast.unspan tok of
            Ast.Operator name -> go name
            Ast.Modifier name -> go name
            _                 -> (result, stream)
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

buildExpr :: ExprBuilderMonad m => [Token] -> m Token
buildExpr = \toks -> case buildStatement toks of
    ExpressionStatement expr -> buildExprSegment expr
    AssignmentStatement pat eq expr -> do
        pat'  <- buildExprSegment pat
        expr' <- buildExprSegment expr
        pure $ Ast.infixApp pat' eq expr'
{-# INLINE buildExpr #-}


-- === Utils === --

buildExprSegment :: ExprBuilderMonad m => [Token] -> m Token
buildExprSegment = buildExprList <=< buildUnspacedExprs
{-# INLINE buildExprSegment #-}

buildUnspacedExprs :: ExprBuilderMonad m => [Token] -> m [Token]
buildUnspacedExprs = buildUnspacedLayouts . discoverLayouts
{-# INLINE buildUnspacedExprs #-}

buildUnspacedLayouts :: ExprBuilderMonad m => [Layouted] -> m [Token]
buildUnspacedLayouts = \stream -> let
    go = \(Layouted layout s) -> case layout of
        SpacedLayout   -> pure s
        UnspacedLayout -> pure <$> buildExprList s
    in concat <$> mapM go stream
{-# INLINE buildUnspacedLayouts #-}

buildExprList :: ExprBuilderMonad m => [Token] -> m Token
buildExprList = buildExprStream . buildStream
{-# INLINE buildExprList #-}

buildExprStream :: ExprBuilderMonad m => Stream -> m Token
buildExprStream = \case
    NullStream -> pure $ Ast.invalid Invalid.EmptyExpression
    OpStreamStart opName op (ElStream el stp) -> case stp of
        EndEl     -> pure (Ast.sectionRight op el)
        InfixEl s -> buildExprOp__ s
                   $ InfixElStream el (EndOpStream opName op)
    ElStreamStart (ElStream el stp) -> case stp of
        EndEl     -> pure el
        InfixEl s -> buildExprOp__ s (EndElStream el)
{-# INLINE buildExprStream #-}

buildExprOp__ :: ∀ m. ExprBuilderMonad m => OpStream -> ElStream -> m Token
buildExprOp__ = \stream stack -> let
    OpStream streamOp streamTp = stream
    ElStream stackEl  stackTp  = stack

    submitToStack :: m Token
    submitToStack = case streamTp of
        EndOp op -> foldExprStackStep__ (Ast.sectionLeft stackEl op) stackTp
        InfixOp_ op streamEl streamTp' -> let
            newOpStream = InfixOpStream streamOp op stack
            newStack    = InfixElStream streamEl newOpStream
            in case streamTp' of
                EndEl           -> foldExprStack__ streamEl newOpStream
                InfixEl stream' -> buildExprOp__ stream' newStack
    {-# INLINE submitToStack #-}

    reduceStack :: OpStreamType -> m Token
    reduceStack = \case
        EndOp op -> buildExprOp__ stream newStack where
            newStack = EndElStream (Ast.sectionRight op stackEl)
        InfixOp_ mop stackEl2 s -> buildExprOp__ stream newStack where
            newStack = ElStream (appInfix mop stackEl2 stackEl) s
    {-# INLINE reduceStack #-}

    markStreamOpInvalid :: Invalid.Symbol -> m Token
    markStreamOpInvalid invSym = let
        inv op    = Ast.Spanned (op ^. Ast.span) (Ast.Invalid invSym)
        newStream = OpStream Name.invalid $ case streamTp of
            EndOp   op          -> EndOp   (inv op)
            InfixOp (Just op) s -> InfixOp (Just (inv op)) s
        in buildExprOp__ newStream stack

    in case stackTp of
        EndEl                   -> submitToStack
        InfixEl_ stackOp stack' -> readRel stackOp streamOp >>= \case
            Nothing -> markStreamOpInvalid Invalid.MissingRelation
            Just LT -> submitToStack
            Just GT -> reduceStack stack'
            Just EQ -> do
                assoc  <- Assoc.read stackOp
                assoc' <- Assoc.read streamOp
                if assoc == assoc'
                    then case assoc of
                        Assoc.Left  -> reduceStack stack'
                        Assoc.Right -> submitToStack
                        Assoc.None  -> markStreamOpInvalid Invalid.NoAssoc
                    else markStreamOpInvalid Invalid.AssocConflict
{-# NOINLINE buildExprOp__ #-}

-- | Relation checking. All operators already discovered to be invalid has
--   special precedence rules like 'lowest'. In such case. however, there will
--   be few operators of lowest precedence and they need to be comparable.
--   Possible solution involves declaring many groups of operators like the
--   'standard' group and lower and higher groups, so the 'invalid' operator
--   would be in a group between standard operators and a group containing
--   'assignment' one.
readRel :: Prec.RelReader Name m => Name -> Name -> m (Maybe Ordering)
readRel = \l r -> if
    | l == Name.assign  -> pure $ Just LT
    | r == Name.assign  -> pure $ Just GT
    | l == Name.invalid -> pure $ Just LT
    | r == Name.invalid -> pure $ Just GT
    | otherwise         -> Prec.readRel l r
{-# INLINE readRel #-}

foldExprStack__ :: ExprBuilderMonad m => Token -> OpStream -> m Token
foldExprStack__ = \el (OpStream op stp2) -> case stp2 of
        EndOp    op           -> pure $ Ast.sectionRight el op
        InfixOp_ mop el2 stp3 -> foldExprStackStep__ (appInfix mop el2 el) stp3
{-# NOINLINE foldExprStack__ #-}

appInfix :: Maybe Token -> (Token -> Token -> Token)
appInfix top ta tb = case top of
    Nothing -> Ast.app ta tb
    Just op -> case Ast.unspan op of
        Ast.Operator "." -> injectApp (hackApp $ flip Ast.infixApp op ta) tb
        _                -> flip Ast.infixApp op ta tb
{-# INLINE appInfix #-}

injectApp :: (Token -> Token) -> Token -> Token
injectApp f t = case Ast.unspan t of
    Ast.App base arg -> Ast.app (injectApp f base) arg
    _                -> f t

-- | TODO: TO BE DELETED!
--
--   This function introduces purposely an incorret behavior that was present
--   in old parser implementation. Unfortunately fixing the old codebase and
--   the GUI is not very easy so we needed to introduce the bug in the new
--   parser as well. Consider the following code:
--
--   > var = foo . bar.baz 4
--
--   According to our operator rules this code should not work, because the
--   accessor operator without spaces has higher precedence than spaced one and
--   thus the code should be parsed as
--
--   > var = foo . (bar.baz 4)
--
--   The problem with fixing it can be seen in GUI when user creates two nodes
--   `foo` and `bar.baz 4` and connects them together. Without proper
--   reformatting or adding parentheses this will result in improper code.
--
--   This behavior is still questionable. We should re-visit this topic as soon
--   as the new parser is shipped.
--
hackApp :: (Token -> Token) -> (Token -> Token)
hackApp f t = case Ast.unspan t of
    Ast.InfixApp l op r -> case Ast.unspan op of
        Ast.Operator "." -> Ast.infixApp (f l) op r
        _                -> f t
    _ -> f t


foldExprStackStep__ :: ExprBuilderMonad m => Token -> ElStreamType -> m Token
foldExprStackStep__ = \a -> \case
    EndEl     -> pure a
    InfixEl s -> foldExprStack__ a s
{-# NOINLINE foldExprStackStep__ #-}















-- data Stream       = OpStreamStart Name (Token -> Token) ElStream
--                   | ElStreamStart ElStream
--                   | NullStream

-- data OpStream     = OpStream      Name OpStreamType
-- data OpStreamType = InfixOp (Token -> Token -> Token) ElStream
--                   | EndOp   (Token -> Token)

-- data ElStream     = ElStream      Token ElStreamType
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
