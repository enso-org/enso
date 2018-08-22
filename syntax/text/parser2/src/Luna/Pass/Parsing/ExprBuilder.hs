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
import Luna.Syntax.Text.Parser.Data.Invalid  (Invalids)
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




------------------------------------------------
-- === Spaced / Unspaced stream partition === --
------------------------------------------------

-- | Divide token stream to spaced and unspaced substreams.

-- === Definition === --

data SpaceStream
    = SpacedStream   [Ast]
    | UnspacedStream [Ast]
    deriving (Show)


-- === API === --

subStreams :: [Ast] -> [SpaceStream]
subStreams = \case
    []     -> []
    (a:as) -> subStreams__ [a] as []
{-# INLINE subStreams #-}

subStreams__ :: [Ast] -> [Ast] -> [SpaceStream] -> [SpaceStream]
subStreams__ = \buff stream out -> case stream of
    [] -> reverse $ spaced buff out
    (tok:toks) -> if checkLeftSpacing tok
        then subStreams__ (tok:buff) toks out
        else subStreams__ [] rest out' where
            (rest,nonSpaced) = (tok:) <$> subNonSpacedStream toks
            out' = case buff of
                (firstNonSpaced:buff')
                   -> unspaced (firstNonSpaced : nonSpaced) (spaced buff' out)
                [] -> unspaced nonSpaced out
    where submitIfNotNull f s = if null s then id else ((:) . f) s
          spaced   = submitIfNotNull (SpacedStream . reverse)
          unspaced = submitIfNotNull (UnspacedStream)

subNonSpacedStream :: [Ast] -> ([Ast], [Ast])
subNonSpacedStream = go where
    go = \stream -> case stream of
        [] -> (mempty, mempty)
        (a:as) -> if checkLeftSpacing a
            then (stream, mempty)
            else (a:) <$> go as
{-# INLINE subNonSpacedStream #-}


-- === Utils === --

checkLeftSpacing :: Ast -> Bool
checkLeftSpacing = (> 0) . view (Ast.span . CodeSpan.viewSpan . Span.offset)
{-# INLINE checkLeftSpacing #-}



----------------------------------
-- === Assignment partition === --
----------------------------------

-- | Divide token stream into Chunk and Expr.
--
--   For the first sight it could seem strange that we convert '[SpaceStream]'
--   to 'ExpressionStream' just to convert it back to '[SpaceStream]' using the
--   'flattenExpressionStream' in the very next step, however the design is
--   future proof. We would like to first parse some section of the file, then
--   discover all new mixfix declarations, update scope information and then
--   parse their bodies, so in the future tere should be some more steps between
--   the conversion 'ExpressionStream' to '[SpaceStream]'.

-- === Definition === --

data ExpressionStream
    = ExpressionStream [SpaceStream]
    | AssignmentStream [SpaceStream] Ast [SpaceStream]
    deriving (Show)


-- === API === --

expressionStream :: [SpaceStream] -> ExpressionStream
expressionStream = expressionStream__ id
{-# INLINE expressionStream #-}

expressionStream__ ::
    ([SpaceStream] -> [SpaceStream]) -> [SpaceStream] -> ExpressionStream
expressionStream__ = \f stream -> case stream of
    []         -> ExpressionStream $! f mempty
    (tok:toks) -> let
        continue = expressionStream__ (f . (tok:)) toks
        in case tok of
            UnspacedStream {} -> continue
            SpacedStream   ss -> case breakOnEq ss of
                Left {} -> continue
                Right (l, x, r)  -> let
                    mod a = if null a then id else (SpacedStream a :)
                    in AssignmentStream (mod l $ f mempty) x (mod r toks)
{-# NOINLINE expressionStream__ #-}

breakOnEq :: [Ast] -> Either [Ast] ([Ast], Ast, [Ast])
breakOnEq = breakOnEq__ id
{-# INLINE breakOnEq #-}

breakOnEq__ :: ([Ast] -> [Ast]) -> [Ast] -> Either [Ast] ([Ast], Ast, [Ast])
breakOnEq__ = \f stream -> case stream of
    []         -> Left (f stream)
    (tok:toks) -> let
        continue = breakOnEq__ (f . (tok:)) toks
        in case Ast.unspan tok of
            Ast.AstOperator (Ast.Operator name) ->
                if name == Name.unify
                    then Right (f mempty, tok, toks)
                    else continue
            _ -> continue
{-# NOINLINE breakOnEq__ #-}

flattenExpressionStream :: ExpressionStream -> [SpaceStream]
flattenExpressionStream = \case
    ExpressionStream s     -> s
    AssignmentStream p x s -> let
        op' = Ast.AstOperator $ Ast.Operator Name.assign
        op  = x & Ast.ast .~ op'
        sop = SpacedStream [op]
        in p <> (sop : s)
{-# INLINE flattenExpressionStream #-}


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
        Ast.AstOperator (Ast.Operator name)
          -> buildStreamEl (EndOpStream name (rightSection tok)) toks
        _ -> buildStreamOp (EndElStream tok) toks
{-# INLINE buildStream #-}

buildStreamOp :: ElStream -> [Ast] -> Stream
buildStreamOp = \result stream -> case stream of
    [] -> ElStreamStart result
    _  -> case takeOperators stream of
        ([(name,op)], []  ) -> OpStreamStart name (leftSection op) result
        ([]         , _   ) -> go Name.app     (Ast.app)      result stream
        ([(name,op)], toks) -> go name         (Ast.app2 op)  result toks
        ((p:ps)     , toks) -> go Name.invalid (Ast.app2 inv) result toks
            where inv = Ast.computeCodeSpanList1__ (snd <$> (p :| ps))
                      $ Ast.invalid' Invalid.AdjacentOperators
                      -- FIXME: register ops in invalid
    where go name f stream = buildStreamEl (InfixOpStream name f stream)
{-# NOINLINE buildStreamOp #-}

buildStreamEl :: OpStream -> [Ast] -> Stream
buildStreamEl = \(result@(OpStream name stp)) -> \case
    [] -> case stp of
        EndOp   f   -> singularElStream $ f Ast.missing
        InfixOp f s -> OpStreamStart name (f Ast.missing) s
    (tok:toks) -> case Ast.unspan tok of
        Ast.AstOperator {} -> undefined
        _                  -> buildStreamOp (InfixElStream tok result) toks
{-# NOINLINE buildStreamEl #-}


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
        Ast.AstOperator (Ast.Operator name)
          -> takeOperators__ ((name,tok) : result) toks
        _ -> (result, stream)
{-# NOINLINE takeOperators__ #-}



-- data ExpressionStream
--     = ExpressionStream [SpaceStream]
--     | AssignmentStream [SpaceStream] [SpaceStream]
--     deriving (Show)

-- data SpaceStream
--     = SpacedStream   [Ast]
--     | UnspacedStream [Ast]
--     deriving (Show)



--------------------------------
-- === Expression builder === --
--------------------------------

-- | The following code is a Shunting-yard algorithm modified to support
--   operator sectioning and automatic error recovery based on a type secure
--   token stream implementation. For more information please refer to the
--   Wikipedia entry: https://en.wikipedia.org/wiki/Shunting-yard_algorithm


-- === API === --

type ExprBuilderMonad m = (Assoc.Reader Name m, Prec.RelReader Name m)


buildExprx :: ExprBuilderMonad m => [Ast] -> m Ast
buildExprx = \stream -> do
    let sstream = buildExpry stream
    stream' <- buildExprSS sstream
    buildExpr_tt stream'
{-# INLINE buildExprx #-}

buildExpry :: [Ast] -> [SpaceStream]
buildExpry = \ast -> let
    stream1 = subStreams ast
    stream2 = expressionStream stream1
    stream3 = flattenExpressionStream stream2
    in stream3
{-# INLINE buildExpry #-}

buildExprSS :: ExprBuilderMonad m => [SpaceStream] -> m [Ast]
buildExprSS = \stream -> let
    go = \case
        SpacedStream   s -> pure s
        UnspacedStream s -> pure <$> buildExpr_tt s
    in concat <$> mapM go stream
{-# INLINE buildExprSS #-}

buildExpr_tt :: ExprBuilderMonad m => [Ast] -> m Ast
buildExpr_tt = buildExpr . buildStream
{-# INLINE buildExpr_tt #-}

buildExpr :: ExprBuilderMonad m => Stream -> m Ast
buildExpr = \case
    NullStream -> pure $ Ast.invalid Invalid.EmptyExpression
    OpStreamStart opName opFunc (ElStream el stp) -> case stp of
        EndEl     -> pure (opFunc el)
        InfixEl s -> buildExprOp__ s
                   $ InfixElStream el (EndOpStream opName opFunc)
    ElStreamStart (ElStream el stp) -> case stp of
        EndEl     -> pure el
        InfixEl s -> buildExprOp__ s (EndElStream el)
{-# INLINE buildExpr #-}


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
    -> Ast.computeCodeSpanList1__ elems $ Ast.invalid' Invalid.AssocConflict
{-# INLINE assocConflict #-}

fillMissing :: (Ast -> Ast -> Ast) -> Ast
fillMissing = \f -> f Ast.missing Ast.missing
{-# INLINE fillMissing #-}
















-----------------------------
-- === Section patterns === --
-----------------------------

-- | Section patterns are used to define non-standard syntax structures like
--   'if ... then ... else ...' or 'def foo a b: ...'. They work in a similar
--   fashion to Lisp macros.


data Chunk
    = Expr
    | ManyExpr
    | ExprBlock
    deriving (Show)


-- === Definition === --

data SegmentList
    = Required Segment SegmentList
    | Optional Segment SegmentList
    | SectionListNull
    deriving (Show)

data Segment = Segment
    { _ast    :: Ast.Ast
    , _chunks :: [Chunk]
    } deriving (Show)
makeLenses ''Segment

data Section = Section
    { _name         :: Name
    , _headSegment  :: Segment
    , _tailSegments :: SegmentList
    }
    deriving (Show)
makeLenses ''Section



-- === Smart constructors === --

syntax :: Name -> Ast.Ast -> [Chunk] -> Section
syntax = \name ast pat -> Section name (Segment ast pat) SectionListNull
{-# INLINE syntax #-}

segment :: Ast.Ast -> [Chunk] -> Segment
segment = Segment
{-# INLINE segment #-}

optional :: Segment -> SegmentList
optional = \s -> Optional s SectionListNull
{-# INLINE optional #-}

required :: Segment -> SegmentList
required = \s -> Required s SectionListNull
{-# INLINE required #-}

infixl 6 +?
infixl 6 +!
(+?), (+!) :: Section -> Segment -> Section
(+?) = \l r -> appendSegment l (optional r)
(+!) = \l r -> appendSegment l (required r)
{-# INLINE (+?) #-}
{-# INLINE (+!) #-}


-- === Utils === --

concatSegmentList :: SegmentList -> SegmentList -> SegmentList
concatSegmentList = \l r -> case l of
    Required sect t -> Required sect (concatSegmentList t r)
    Optional sect t -> Optional sect (concatSegmentList t r)
    SectionListNull -> r

appendSegment :: Section -> SegmentList -> Section
appendSegment = \sect r -> sect & tailSegments %~ flip concatSegmentList r
{-# INLINE appendSegment #-}



------------------------

-- data SegmentList1 = SegmentList1 Segment SegmentList
--     deriving (Show)

-- data SectionBuilder = SectionBuilder
--     { _consumed :: [Ast]
--     , _left     :: SegmentList1
--     } deriving (Show)

-- makeLenses ''SectionBuilder


newtype PossibleSections = PossibleSections (Map Ast.Ast Section)
    deriving (Default, Show)
makeLenses ''PossibleSections

registerSection :: State.Monad PossibleSections m => Section -> m ()
registerSection = \section -> State.modify_ @PossibleSections
    $ wrapped %~ Map.insert (section ^. headSegment . ast) section
{-# INLINE registerSection #-}

lookupSection :: State.Getter PossibleSections m => Ast.Ast -> m (Maybe Section)
lookupSection = \ast -> Map.lookup ast . unwrap <$> State.get @PossibleSections
{-# INLINE lookupSection #-}


newtype Reserved = Reserved (Set Ast.Ast)
    deriving (Default, Show)
makeLenses ''Reserved




withReserved :: State.Monad Reserved m => Ast.Ast -> m a -> m a
withReserved = \a -> State.withModified @Reserved $ wrapped %~ Set.insert a
{-# INLINE withReserved #-}

withReservedMany :: State.Monad Reserved m => [Ast.Ast] -> m a -> m a
withReservedMany = \a -> State.withModified @Reserved
                       $ wrapped %~ (Set.fromList a <>)
{-# INLINE withReservedMany #-}

checkReserved :: State.Getter Reserved m => Ast.Ast -> m Bool
checkReserved = \a -> Set.member a . unwrap <$> State.get @Reserved
{-# INLINE checkReserved #-}


newtype Streamx = Streamx [Ast] deriving (Show)
makeLenses ''Streamx

type SegmentBuilder m =
    ( State.Monad Streamx m
    , State.Monad Reserved m
    , State.Monad PossibleSections m
    , ExprBuilderMonad m
    )


syntax_if_then_else = syntax "if_then_else"
              (Ast.AstVar $ Ast.Var "if")   [Expr]
   +! segment (Ast.AstVar $ Ast.Var "then") [Expr]
   +! segment (Ast.AstVar $ Ast.Var "else") [Expr]

syntax_group = syntax "(_)"
              (Ast.AstOperator $ Ast.Operator "(") [Expr]
   +! segment (Ast.AstOperator $ Ast.Operator ")") []

syntax_list = syntax "[_]"
              (Ast.AstOperator $ Ast.Operator "[") [Expr]
   +! segment (Ast.AstOperator $ Ast.Operator "]") []

runSegmentBuilderT :: Monad m => [Ast] -> State.StatesT '[Streamx, PossibleSections, Reserved] m a -> m (a, Streamx)
runSegmentBuilderT = \stream p
    -> State.evalDefT  @Reserved
     $ State.evalDefT @PossibleSections
     $ flip (State.runT @Streamx) (wrap stream)
     $ do
        mapM_ registerSection
            [ syntax_if_then_else
            , syntax_group
            , syntax_list
            ]
        p
-- buildSegments :: [Ast] -> [Ast]
-- buildSegments = State.evalDef @ReservedStack . buildSegments__
-- {-# INLINE buildSegments #-}

-- buildSegments__ :: State.Monad ReservedStack m => [Ast] -> m [Ast]
-- buildSegments__ = \case
--     [] -> pure []
--     (tok:toks) ->


token :: State.Monad Streamx m => m (Maybe Ast)
token = peekToken <* dropToken
{-# INLINE token #-}

tokenNotReserved :: (State.Monad Streamx m, State.Getter Reserved m)
    => m (Maybe Ast)
tokenNotReserved = mapM (<$ dropToken) =<< peekTokenNotReserved
{-# INLINE tokenNotReserved #-}

peekToken :: State.Getter Streamx m => m (Maybe Ast)
peekToken = head . unwrap <$> State.get @Streamx
{-# INLINE peekToken #-}

peekTokenNotReserved :: (State.Getter Streamx m, State.Getter Reserved m)
    => m (Maybe Ast)
peekTokenNotReserved = peekToken >>= \case
    Just tok -> checkReserved (Ast.unspan tok) >>= \case
        True  -> pure Nothing
        False -> pure $ Just tok
    Nothing -> pure Nothing
{-# INLINE peekTokenNotReserved #-}

dropToken :: State.Monad Streamx m => m ()
dropToken = State.modify_ @Streamx $ \s -> case unwrap s of
    []     -> s
    (_:as) -> wrap as
{-# INLINE dropToken #-}



funcDed = syntax "def"
              (Ast.AstVar      $ Ast.Var      "def") [Expr, ManyExpr]
   +! segment (Ast.AstOperator $ Ast.Operator ":")   [ExprBlock]


parseChunk :: SegmentBuilder m => Chunk -> m Ast
parseChunk = \chunk -> case chunk of
    Expr -> parseExpr

parseExpr :: SegmentBuilder m => m Ast
parseExpr = buildExprx =<< go where
    go = tokenNotReserved >>= \case
        Nothing  -> pure mempty
        Just tok -> do
            head <- lookupSection (Ast.unspan tok) >>= \case
                Nothing   -> pure tok
                Just sect -> parseSection tok sect
            (head:) <$> go
{-# INLINE parseExpr #-}

parseChunks :: SegmentBuilder m => [Chunk] -> m [Ast]
parseChunks = go where
    go = \case
        []     -> pure mempty
        (c:cs) -> (:) <$> parseChunk c <*> go cs
{-# NOINLINE parseChunks #-}

parseSegment :: SegmentBuilder m => Segment -> m [Ast]
parseSegment = parseChunks . view chunks
{-# INLINE parseSegment #-}

parseSegmentList :: SegmentBuilder m => SegmentList -> m [Spanned [Ast]]
parseSegmentList = \lst -> peekToken >>= \case
    Nothing  -> pure mempty
    Just tok -> case lst of
        SectionListNull -> pure mempty
        Required (Segment seg chunks) lst' -> if Ast.unspan tok == seg
            then do
                dropToken
                outs <- withNextSegmentReserved lst'
                      $ parseChunks chunks
                (Ast.Spanned (tok ^. Ast.span) outs:) <$> parseSegmentList lst'
            else undefined
        x -> error $ ppShow x

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

withNextSegmentReserved :: SegmentBuilder m => SegmentList -> m a -> m a
withNextSegmentReserved = \case
    Required (Segment seg _) _ -> withReserved seg
    Optional (Segment seg _) _ -> withReserved seg
    SectionListNull            -> id

parseSection :: SegmentBuilder m => Ast -> Section -> m Ast
parseSection = \(Ast.Spanned span _) (Section name seg lst) -> do
    psegs            <- withNextSegmentReserved lst $ parseSegment seg
    (tailSpan, slst) <- mergeSpannedLists <$> parseSegmentList lst
    let header = Ast.Spanned span $ Ast.var' name
        group  = Ast.apps header  $ psegs <> slst
        out    = group & Ast.span %~ (<> tailSpan)
    pure out

-- data Section = Section Segment SegmentList
--     deriving (Show)

-- data SegmentList
--     = Required Segment SegmentList
--     | Optional Segment SegmentList
--     | SectionListNull
--     deriving (Show)

-- data Segment = Segment
--     { _name   :: Ast.Ast
--     , _chunks :: [Chunk]
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
