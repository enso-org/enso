{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Pass.Parsing.Parser where

import Prologue


import qualified Control.Monad.State.Layered               as State
import qualified Data.Graph.Component.Node.Destruction     as Component
import qualified Data.Graph.Data.Graph.Class               as Graph
import qualified Data.Graph.Data.Layer.Class               as Layer
import qualified Data.Map                                  as Map
import qualified Data.Mutable.Class                        as Mutable
import qualified Data.Set                                  as Set
import qualified Data.Text.Span                            as Span
import qualified Language.Symbol.Operator.Assoc            as Assoc
import qualified Language.Symbol.Operator.Prec             as Prec
import qualified Luna.IR                                   as IR
import qualified Luna.IR.Aliases                           as Uni
import qualified Luna.IR.Term.Ast.Invalid                  as Invalid
import qualified Luna.Pass                                 as Pass
import qualified Luna.Pass.Attr                            as Attr
import qualified Luna.Pass.Parsing.Macro                   as Macro
import qualified Luna.Pass.Parsing.Parserx                 as Stage1
import qualified Luna.Pass.Scheduler                       as Scheduler
import qualified Luna.Syntax.Text.Parser.Data.CodeSpan     as CodeSpan
import qualified Luna.Syntax.Text.Parser.Data.CodeSpan     as CodeSpan
import qualified Luna.Syntax.Text.Parser.Data.Name.Special as Name
import qualified Luna.Syntax.Text.Parser.IR.Ast            as Ast
import qualified Luna.Syntax.Text.Parser.State.Marker      as Marker

import Data.Map                              (Map)
import Data.Set                              (Set)
import Data.Text.Position                    (Delta (Delta))
import Data.Text32                           (Text32)
import Luna.Pass                             (Pass)
import Luna.Syntax.Text.Parser.Data.CodeSpan (CodeSpan)
import Luna.Syntax.Text.Parser.Data.Invalid  (Invalids)
import Luna.Syntax.Text.Parser.Data.Result   (Result)
import Luna.Syntax.Text.Parser.Data.Result   (Result (Result))
import Luna.Syntax.Text.Parser.IR.Ast        (Spanned (Spanned))
import Luna.Syntax.Text.Parser.IR.Term       (Ast)
import Luna.Syntax.Text.Source               (Source)
import OCI.Data.Name                         (Name)



data Parser

type instance Graph.Components      Parser          = '[IR.Terms, IR.Links]
type instance Graph.ComponentLayers Parser IR.Links = '[IR.Target, IR.Source]
type instance Graph.ComponentLayers Parser IR.Terms
    = '[IR.Users, IR.Model, IR.Type, CodeSpan]


type ParserPass m = Pass.Interface Parser m
type instance Pass.Spec Parser t = Spec t
type family   Spec  t where
    Spec (Pass.In  Pass.Attrs) = '[Source, Result]
    Spec (Pass.In  IR.Terms)   = CodeSpan
                              ': Pass.BasicPassSpec (Pass.In IR.Terms)
    Spec (Pass.Out t)          = Spec (Pass.In t)
    Spec t                     = Pass.BasicPassSpec t



type BuilderMonad m =
    ( MonadIO m
    , Pass.Interface Parser m
    )



-- -------------------------
-- -- === Parser pass === --
-- -------------------------

-- -- === Definition === --

instance ParserPass (Pass stage Parser)
      => Pass.Definition stage Parser where
    definition = do
        src             <- Attr.get @Source
        (unit, markers) <- run (convert src)
        Attr.put $ Result unit


-- === API === --

-- registerStatic :: Registry.Monad m => m ()
-- registerStatic = do
--     Registry.registerPrimLayer @IR.Terms @CodeSpan

registerDynamic :: ∀ stage m.
    ( ParserPass (Pass stage Parser)
    , Scheduler.PassRegister stage Parser m
    , Scheduler.Monad m
    ) => m ()
registerDynamic = do
    Scheduler.registerAttr     @Invalids
    Scheduler.enableAttrByType @Invalids
    Scheduler.registerAttr     @Source
    Scheduler.enableAttrByType @Source
    Scheduler.registerAttr     @Result
    Scheduler.enableAttrByType @Result
    Scheduler.registerPass     @stage @Parser

run :: ParserPass (Pass stage Parser)
    => Text32 -> Pass stage Parser (IR.SomeTerm, Marker.TermMap)
run = runWith Macro.unit

runWith :: ParserPass (Pass stage Parser)
    => Macro.Parser Ast -> Text32 -> Pass stage Parser (IR.SomeTerm, Marker.TermMap)
runWith p src = runMeDebug $ Stage1.runWith p src
{-# INLINE runWith #-}

runMeDebug :: ParserPass (Pass stage Parser)
    => Ast -> Pass stage Parser (IR.SomeTerm, Marker.TermMap)
runMeDebug ast = do
    ((ref, unmarked), gidMap) <- State.runDefT @Marker.TermMap
                               $ State.runDefT @Marker.TermOrphanList
                               $ buildGraph ast
    pure (ref, gidMap)
{-# INLINE runMeDebug #-}

    -- let tokens = Lexer.evalDefLexer src
        -- parser = Parsing.stx *> p <* Parsing.etx
    -- runParserContext__ parser tokens >>= \case
    --     Left e -> error ("Parser error: " <> parseErrorPretty e <> "\ntokens:\n"
    --            <> show (view Symbol.symbol <$> tokens))
    --     Right irbs -> do
    --         ((ref, unmarked), gidMap) <- State.runDefT @Marker.TermMap
    --                                    $ State.runDefT @Marker.TermOrphanList
    --                                    $ fromIRB $ fromIRBS irbs
    --         pure (ref, gidMap)

type instance Item (NonEmpty a) = a

buildGraph :: forall m. BuilderMonad m => Ast -> m IR.SomeTerm
buildGraph = go
{-# INLINE buildGraph #-}

strGo :: forall m. BuilderMonad m => Ast.Spanned (Ast.StrChunk Ast.Ast) -> m IR.SomeTerm
strGo = \(Spanned cs a) -> addCodeSpan cs =<< case a of
    Ast.StrPlain t -> IR.rawString' =<< Mutable.fromList (toString t)
    _              -> IR.invalid' Invalid.ParserError
    where addCodeSpan cs ir = ir <$ IR.writeLayer @CodeSpan ir cs

go :: forall m. BuilderMonad m => Ast -> m IR.SomeTerm
go = \(Spanned cs ast) -> addCodeSpan cs =<< case ast of

    -- Literals
    Ast.Number     num -> do
        intPart <- Mutable.fromList (toList num)
        empty   <- Mutable.new
        IR.number' 10 intPart empty
    Ast.Str       strs -> do
        [str] <- strGo <$$> strs
        pure str

    -- Identifiers
    Ast.Var       name -> IR.var'   name
    Ast.Cons      name -> IR.cons'  name []
    Ast.Operator  name -> IR.var'   name
    Ast.Modifier  name -> error "TODO" -- we need to discuss handling it in IR
    Ast.Wildcard       -> IR.blank'

    -- Layouting
    Ast.Block b -> do
        foo :| foos <- go <$$> b
        foldlM IR.seq' foo foos
    Ast.Marker m -> IR.marker' $ fromIntegral m
    Ast.LineBreak ind  -> impossible -- All line breaks handled in parser

    -- Docs
    Ast.Comment a -> error "TODO" -- we need to handle non attached comments

    -- Errors
    Ast.Invalid   inv  -> IR.invalid' inv

    -- Expressions
    Ast.Unit      ls   -> do
        (ih      :: IR.SomeTerm) <- IR.importHub' []
        (unitCls :: IR.SomeTerm) <- IR.record' False "" [] [] =<< (go <$$> ls)
        IR.unit' ih [] unitCls
    Ast.InfixApp l f r -> do
        let tok = Ast.unspan f
            specialOp = case tok of
                Ast.Operator op -> if
                    | op == Name.assign -> Just IR.unify'
                    | op == Name.lam    -> Just IR.lam'
                    | op == Name.acc    -> Just IR.acc'
                    | otherwise         -> Nothing
                _ -> Nothing

        case specialOp of
            Just op -> do
                l' <- go l
                r' <- go $! Ast.prependAsOffset f r
                op l' r'
            Nothing -> do
                f' <- go f
                l' <- go l
                r' <- go r
                (lf :: IR.SomeTerm) <- IR.app' f' l'
                IR.app' lf r'

    Ast.SectionRight f r -> do
        f' <- go f
        r' <- go r
        -- TODO
        -- The naming IR.sectionLeft and IR.sectionRight need to be swapped!
        IR.sectionLeft' f' r'
    Ast.SectionLeft l f -> do
        l' <- go l
        f' <- go f
        -- TODO
        -- We should change the ordering in IR to reflect real code placement.
        -- Right now both sections have function link on the left side in IR.
        IR.sectionRight' f' l'
    Ast.App f a        -> do
        let (tok, arg :| args) = collectApps (pure a) f
            continue = appTailArgs =<< appHeadArg =<< go tok

            appHeadArg :: IR.SomeTerm -> m IR.SomeTerm
            appHeadArg = \t -> do
                arg' <- go arg
                IR.app' t arg'

            appTailArgs :: IR.SomeTerm -> m IR.SomeTerm
            appTailArgs = \t -> do
                args' <- go <$$> args
                foldlM IR.app' t args'

        case Ast.unspan tok of
            Ast.Var var -> if
                | var == "(_)" -> appTailArgs =<< case Ast.unspan arg of
                    Ast.List []  -> IR.tuple' []
                    Ast.List [a] -> IR.grouped' =<< go a
                    Ast.List as  -> IR.tuple'   =<< (go <$$> as)
                    _            -> parseError
                | var == "[_]"   -> appTailArgs =<< case Ast.unspan arg of
                    Ast.List as -> IR.list' =<< (go <$$> as)
                    _           -> parseError
                | var == "def_:" -> case args of
                    [paramLst, body] -> do
                        case Ast.unspan paramLst of
                            Ast.List params -> do
                                name'   <- go $ Ast.prependAsOffset tok arg
                                params' <- go <$$> params
                                body'   <- go body
                                IR.function' name' params' body'
                            _ -> parseError
                    _ -> parseError
                | otherwise -> continue
            -- Ast.Comment c -> do
            --     a :| as <- go <$$> arg :| args
            --     case as of
            --         [a2] -> do
            --             doc <- Mutable.fromList (toString c)
            --             IR.documented' doc a2
            --         _    -> parseError
            Ast.Marker c -> do
                a :| as <- go <$$> (arg :| args)
                case as of
                    [] -> flip IR.marked' a =<< go tok
                    _  -> parseError
            _ -> continue
    Ast.Comment c -> parseError

    x -> error $ "TODO: " <> show x
    where addCodeSpan cs ir = do
            --   putStrLn "\n\n"
            --   print . IR.showTag =<< Layer.read @IR.Model ir
            --   print cs
              ir <$ IR.writeLayer @CodeSpan ir cs
          parseError = IR.invalid' Invalid.ParserError
{-# NOINLINE go #-}

-- foldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a

isOperator n = (== Ast.Operator n)

(<$$>) :: (Traversable t, Monad m) => (a -> m b) -> t a -> m (t b)
(<$$>) = mapM
{-# INLINE (<$$>) #-}

collectApps :: NonEmpty Ast -> Ast -> (Ast, NonEmpty Ast)
collectApps = \apps tok -> case Ast.unspan tok of
    Ast.App f a -> collectApps (a <| apps) f
    _           -> (tok, apps)

collectSpan :: NonEmpty Ast -> CodeSpan
collectSpan = \lst -> let
    s :| ss = view Ast.span <$> lst
    in foldl' (<>) s ss
{-# INLINE collectSpan #-}

t <| (a :| as) = t :| (a : as)


-- App (App : a) b
