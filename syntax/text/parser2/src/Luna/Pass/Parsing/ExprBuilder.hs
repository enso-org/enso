{-# LANGUAGE UndecidableInstances #-}

module Luna.Pass.Parsing.ExprBuilder where

import Prologue

import qualified Data.Graph.Component.Node.Destruction as Component
import qualified Luna.IR                               as IR
import qualified Luna.IR.Aliases                       as Uni
import qualified Luna.Pass                             as Pass
import qualified Luna.Syntax.Text.Parser.IR.Ast        as Ast

import Data.Text.Position                    (Delta (Delta))
import Luna.Syntax.Text.Parser.Data.CodeSpan (CodeSpan)
import Luna.Syntax.Text.Parser.Data.Invalid  (Invalids)
import Luna.Syntax.Text.Parser.Data.Result   (Result)
import Luna.Syntax.Text.Parser.IR.Ast        (Spanned (Spanned))
import Luna.Syntax.Text.Parser.IR.Term       (Ast)
import Luna.Syntax.Text.Source               (Source)

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
    , Component.Delete m
    )



buildGraph :: forall m. BuilderMonad m => Ast -> m IR.SomeTerm
buildGraph = \(Spanned cs ast) -> addCodeSpan cs =<< case ast of
    Ast.AstCons      (Ast.Cons      name) -> IR.cons'  name []
    Ast.AstVar       (Ast.Var       name) -> IR.var'   name
    Ast.AstOperator  (Ast.Operator  name) -> IR.var'   name
    Ast.AstWildcard  {}                   -> IR.blank'
    Ast.AstLineBreak (Ast.LineBreak ind)  -> IR.lineBreak' (unwrap ind)
    x                                     -> error $ "TODO: " <> show x
    where addCodeSpan cs ir = ir <$ IR.writeLayer @CodeSpan ir cs
{-# INLINE buildGraph #-}



inheritCodeSpanLst :: BuilderMonad m
    => ([IR.Term a] -> m (IR.Term b))
    -> ([IR.Term a] -> m (IR.Term b))
inheritCodeSpanLst = \f ts -> do
    cs <- IR.readLayer @CodeSpan <$$> ts
    ir <- f ts
    IR.writeLayer @CodeSpan ir (mconcat cs)
    pure ir
{-# INLINE inheritCodeSpanLst #-}


partitionTokStream :: BuilderMonad m => Int -> [IR.SomeTerm] -> m [IR.SomeTerm]
partitionTokStream = \ind toks -> partitionTokStream__ ind toks mempty mempty
{-# INLINE partitionTokStream #-}

partitionTokStream__ :: BuilderMonad m
    => Int -> [IR.SomeTerm] -> [IR.SomeTerm] -> [IR.SomeTerm] -> m [IR.SomeTerm]
partitionTokStream__ = \ind -> let
    go toks expr defs = let
        submitDefs = (: defs) <$> inheritCodeSpanLst IR.tokens' expr
        in case toks of
            []     -> if null expr then pure defs else submitDefs
            (t:ts) -> IR.model t >>= \case
                Uni.LineBreak i -> case compare ind i of
                    EQ -> do
                        IR.delete t
                        go ts mempty =<< submitDefs
                    LT -> go ts (t:expr) defs
                    GT -> error "TODO: wrong indentation"
                _ -> go ts (t:expr) defs
    in go




-- | The pass algorithm is shown below.
--
-- The implementation progress uses the following legend:
--
--     [ ] - to be done
--     [x] - already implemented
--     [-] - postponed
--     [.] - partial, possibly hardcoded implementation
--
-- ## STAGE 1
-- In this tage we produce AST from token stream. In order to correctly parse
-- expressions, we need to know:
--
--     - Custom parsing rules.
--
--       They allow altering the syntax completely. In barebone parser
--       everything is just an expression containing identifiers and operators
--       separated by spaces. Any more complex construction like `type Vector a
--       ...` is defined as custom parsing rule and is translated during parsing
--       to appriopriate macro calls.
--
--       All custom parsing rules need to be defined in a separate compilation
--       unit, let it be a file or other module required to be imported.
--       Otherwise after parsing a macro call and evaluating it, it might result
--       in a custom parsing rule definition which would alter how the original
--       macro call was parsed.

--
--
--       - Mixfix operators. While assembling final expression from tokens we
--         need to know all possible mixfix operators like `if_then_else`.
--
--       - Operator precedences.
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



run :: BuilderMonad m => [IR.SomeTerm] -> m ()
run = \toks -> do
    print "!!!!"

                -- if null expr then pure defs
    -- let (e:es) = exprs
    -- x <- IR.model e
    -- case x of
    --     Uni.Lam {} -> print "ouch"
    out <- partitionTokStream 0 toks
    print =<< (mapM IR.model out)
    pure ()

    -- putLnFmtd =<< showM out
