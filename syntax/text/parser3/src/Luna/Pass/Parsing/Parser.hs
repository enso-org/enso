{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Pass.Parsing.Parser where

import Prologue


import qualified Control.Monad.State.Layered           as State
import qualified Data.Graph.Component.Node.Destruction as Component
import qualified Data.Graph.Data.Graph.Class           as Graph
import qualified Data.Graph.Data.Layer.Class           as Layer
import qualified Data.Map                              as Map
import qualified Data.Mutable.Class                    as Mutable
import qualified Data.Set                              as Set
import qualified Data.Text.Span                        as Span
import qualified Language.Symbol.Operator.Assoc        as Assoc
import qualified Language.Symbol.Operator.Prec         as Prec
import qualified Luna.IR                               as IR
import qualified Luna.IR.Aliases                       as Uni
import qualified Luna.IR.Term.Ast.Invalid              as Invalid
import qualified Luna.Pass                             as Pass
import qualified Luna.Pass.Attr                        as Attr
import qualified Luna.Pass.Scheduler                   as Scheduler
import qualified Luna.Syntax.Text.Parser.Ast           as Ast
import qualified Luna.Syntax.Text.Parser.Ast.Class     as Atom
import qualified Luna.Syntax.Text.Parser.Ast.CodeSpan  as CodeSpan
import qualified Luna.Syntax.Text.Parser.Ast.CodeSpan  as CodeSpan
import qualified Luna.Syntax.Text.Parser.Lexer         as Lexer
import qualified Luna.Syntax.Text.Parser.Lexer.Names   as Name
import qualified Luna.Syntax.Text.Parser.Parser        as Parser
import qualified Luna.Syntax.Text.Parser.State.Marker  as Marker

import Data.Map                              (Map)
import Data.Set                              (Set)
import Data.Text.Position                    (Delta (Delta))
import Data.Text32                           (Text32)
import Luna.Pass                             (Pass)
import Luna.Syntax.Text.Parser.Ast           (Spanned (Spanned))
import Luna.Syntax.Text.Parser.Ast.CodeSpan  (CodeSpan)
import Luna.Syntax.Text.Parser.State.Invalid (Invalids)
import Luna.Syntax.Text.Parser.State.Result  (Result)
import Luna.Syntax.Text.Parser.State.Result  (Result (Result))
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
    , State.Getter Marker.TermMap m
    , State.Setter Marker.TermMap m
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
run = runWith Parser.unit

runWith :: ParserPass (Pass stage Parser)
    => Parser.Parser Lexer.Token -> Text32 -> Pass stage Parser (IR.SomeTerm, Marker.TermMap)
runWith p src = runMeDebug $ Parser.evalWith p src
{-# INLINE runWith #-}

runMeDebug :: ParserPass (Pass stage Parser)
    => Lexer.Token -> Pass stage Parser (IR.SomeTerm, Marker.TermMap)
runMeDebug ast = do
    ((ref, unmarked), gidMap) <- State.runDefT @Marker.TermMap
                               $ State.runDefT @Marker.TermOrphanList
                               $ buildGraph ast
    pure (ref, gidMap)
{-# INLINE runMeDebug #-}


type instance Item (NonEmpty a) = a

buildGraph :: forall m. BuilderMonad m => Lexer.Token -> m IR.SomeTerm
buildGraph = buildIR
{-# INLINE buildGraph #-}

strGo :: forall m. BuilderMonad m => Ast.Spanned (Atom.StrChunk Ast.Ast) -> m IR.SomeTerm
strGo = \(Spanned cs a) -> addCodeSpan cs =<< case a of
    Atom.StrPlain t -> IR.rawString' =<< Mutable.fromList (toString t)
    _               -> IR.invalid' Invalid.ParserError
    where addCodeSpan cs ir = ir <$ IR.writeLayer @CodeSpan ir cs




-- === Utils === --

addCodeSpan :: BuilderMonad m => CodeSpan -> IR.SomeTerm -> m IR.SomeTerm
addCodeSpan = \cs ir -> do
    -- putStrLn "\n\n"
    print . IR.showTag =<< Layer.read @IR.Model ir
    print cs
    ir <$ IR.writeLayer @CodeSpan ir cs
{-# INLINE addCodeSpan #-}

parseError :: BuilderMonad m => m IR.SomeTerm
parseError = IR.invalid' Invalid.ParserError
{-# INLINE parseError #-}


-- === Imports === --

discoverImportLines :: [Lexer.Token] -> ([Lexer.Token], [Lexer.Token])
discoverImportLines = discoverImportLines__ mempty mempty
{-# INLINE discoverImportLines #-}

discoverImportLines__ :: [Lexer.Token] -> [Lexer.Token] -> [Lexer.Token] -> ([Lexer.Token], [Lexer.Token])
discoverImportLines__ = \results others stream -> case stream of
    []           -> (reverse others, reverse results)
    (line:lines) -> let
        break = (stream, results)
        in case Ast.unspan line of
            Ast.App f a -> let (tok, arg :| args) = collectApps (pure a) f
                in case Ast.unspan tok of
                    Ast.Var "import" -> let
                        results' = line : results
                        in discoverImportLines__ results' others lines
                    _ -> discoverImportLines__ results (line:others) lines
            _ -> discoverImportLines__ results (line:others) lines

getImportName :: Lexer.Token -> Maybe IR.Qualified
getImportName = \t -> case Ast.unspan t of
    Ast.InfixApp l f r -> case Ast.unspan f of
        Ast.Operator op -> if
            | op == Name.acc -> do
                l' <- getImportName l
                r' <- getImportName r
                pure $ l' <> "." <> r'
            | otherwise -> Nothing
        _ -> Nothing
    Ast.Cons v -> pure $ convert v

buildImportIR :: forall m. BuilderMonad m => Lexer.Token -> [Lexer.Token] -> m IR.SomeTerm
buildImportIR = \a@(Spanned cs ast) args -> addCodeSpan cs =<< if not (null args)
    then parseError
    else case ast of
        Ast.Cons v -> do
            isrc <- IR.importSource $ IR.Absolute (convert v)
            IR.imp' isrc IR.Everything
        Ast.InfixApp l f r -> case getImportName a of
            Just name -> do
                isrc <- IR.importSource $ IR.Absolute name
                IR.imp' isrc IR.Everything
            _ -> parseError
        _ -> parseError


-- === IR === --

buildUnit :: BuilderMonad m => [Lexer.Token] -> m [IR.SomeTerm]
buildUnit = \case
    [] -> pure []
    (a:as) -> do
        a' <- buildIR a
        case Ast.unspan a of
            Ast.Comment c -> case as of
                (aa:aas) -> do
                    txt <- Mutable.fromList $ convertTo @[Char] c
                    aa' <- buildIR aa
                    dd  <- IR.documented' txt aa'
                    addCodeSpan (view Ast.span a <> view Ast.span aa) dd
                    (dd:) <$> buildUnit aas
                _ -> (a':) <$> buildUnit as
            _ -> (a':) <$> buildUnit as

buildIR :: forall m. BuilderMonad m => Lexer.Token -> m IR.SomeTerm
buildIR = \(Spanned cs ast) -> putStrLn "\n---" {- >> print cs -} >> (addCodeSpan cs =<< case ast of

    -- Literals
    Ast.Number     num -> do
        intPart <- Mutable.fromList (toList num)
        empty   <- Mutable.new
        IR.number' 10 intPart empty

    Ast.Str       strs -> do
        [str] <- strGo <$$> strs
        pure str

    -- Identifiers
    Ast.Var       name -> print ("var " <> show name) >> IR.var'   name
    Ast.Cons      name -> IR.cons'  name []
    Ast.Operator  name -> print ("op " <> show name) >> IR.var'   name
    -- Ast.Modifier  name -> error "TODO" -- we need to discuss handling it in IR
    Ast.Wildcard       -> IR.blank'

    -- Layouting
    Ast.Block b -> do
        foo :| foos <- buildIR <$$> b
        let f acc new = do
                csAcc <- IR.readLayer @CodeSpan acc
                csNew <- IR.readLayer @CodeSpan new
                ir <- IR.seq' acc new
                IR.writeLayer @CodeSpan ir (csAcc <> csNew)
                pure ir
        foldlM f foo foos
    Ast.Marker m -> IR.marker' $ fromIntegral m
    Ast.LineBreak ind  -> impossible -- All line breaks handled in parser

    -- Docs
    -- Ast.Comment a -> error "TODO" -- we need to handle non attached comments

    -- Errors
    Ast.Invalid inv  -> IR.invalid' inv

    -- Expressions
    Ast.Unit block -> case Ast.unspan block of
        Ast.Block (l:|ls) -> do
            let (ls', imps) = discoverImportLines (l:ls)
                impsSpan    = collectSpan imps
                record'     = IR.record' False "" [] []
            (unitCls :: IR.SomeTerm) <- record'       =<< buildUnit ls'
            (ih      :: IR.SomeTerm) <- IR.importHub' =<< buildIR <$$> imps
            IR.writeLayer @CodeSpan ih impsSpan
            IR.unit' ih [] unitCls
        _ -> parseError

    Ast.InfixApp l f r -> do
        let tok = Ast.unspan f
            specialOp = case tok of
                Ast.Operator op -> if
                    | op == Name.assign -> Just IR.unify'
                    | op == Name.lam    -> Just IR.lam'
                    | op == Name.acc    -> Just IR.acc'
                    | otherwise         -> Nothing
                _ -> Nothing
            realNumber =
                let dot = tok == Ast.Operator Name.acc
                    isNumber ast = case Ast.unspan ast of
                        Ast.Number _ -> True
                        _            -> False
                    lIsNum = isNumber l
                    rIsNum = isNumber r
                in dot && lIsNum && rIsNum

        case specialOp of
            Just op -> do
                if realNumber then do
                    buildRealIR l r
                else do
                    l' <- buildIR l
                    r' <- buildIR $! Ast.prependAsOffset f r
                    op l' r'
            Nothing -> do
                f' <- buildIR f
                l' <- buildIR l
                r' <- buildIR r
                (lf :: IR.SomeTerm) <- IR.app' f' l'
                IR.app' lf r'

    Ast.SectionRight f r -> do
        f' <- buildIR f
        r' <- buildIR r
        -- TODO
        -- The naming IR.sectionLeft and IR.sectionRight need to be swapped!
        IR.sectionLeft' f' r'

    Ast.SectionLeft l f -> do
        l' <- buildIR l
        f' <- buildIR f
        -- TODO
        -- We should change the ordering in IR to reflect real code placement.
        -- Right now both sections have function link on the left side in IR.
        IR.sectionRight' f' l'

    Ast.App f a -> do
        let (tok, arg :| args) = collectApps (pure a) f
            continue       = builAppsIR args =<< builAppIR arg =<< buildIR tok
            assertNoArgs f = if not (null args) then parseError else f
            mfixArg        = Ast.prependAsOffset tok arg

        case Ast.unspan tok of
            Ast.Var var -> if
                | var == "(_)"     -> buildTupleIR    mfixArg args
                | var == "[_]"     -> buildListIR     mfixArg args
                | var == "case_of" -> buildCaseOfIR   mfixArg args
                | var == "def_:"   -> buildFunctionIR mfixArg args
                | var == "class_:" -> buildClassIR    mfixArg args
                | var == "import"  -> buildImportIR   mfixArg args
                | otherwise        -> continue

            Ast.Marker c -> assertNoArgs $ do
                expr   <- buildIR arg
                marker <- buildIR tok
                marked <- flip IR.marked' expr marker
                let mid = fromIntegral c
                State.modify_ @Marker.TermMap $ wrapped %~ Map.insert mid marked
                pure marked

            _ -> continue

    Ast.Comment c -> parseError
    Ast.Missing   -> do
        print ">> missing"
        print cs
        IR.missing'

    x -> error $ "TODO: " <> show x
    )
{-# NOINLINE buildIR #-}

-- foldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a

(<?>) :: Lexer.Token -> Invalid.Symbol -> Lexer.Token
(<?>) = flip specInvalid
{-# INLINE (<?>) #-}


assertSingleArg args f = case args of
    [a] -> f a
    _   -> parseError



builAppIR :: BuilderMonad m => Lexer.Token -> IR.SomeTerm -> m IR.SomeTerm
builAppIR = \arg t -> do
    arg' <- buildIR arg
    IR.app' t arg'

builAppsIR :: BuilderMonad m => [Lexer.Token] -> IR.SomeTerm -> m IR.SomeTerm
builAppsIR = \args t -> do
    args' <- buildIR <$$> args
    foldlM IR.app' t args'

type MixFixBuilder = forall m. BuilderMonad m => Lexer.Token -> [Lexer.Token] -> m IR.SomeTerm

buildClassIR :: MixFixBuilder
buildClassIR arg args = case Ast.unspan arg of
    Ast.Cons n -> case args of
        [ps, clss] -> case Ast.unspan ps of
            Ast.List params -> case Ast.unspan clss of
                Ast.List recs -> do
                    conss <- mapM buildClassConsIR recs
                    params' <- buildIR <$$> params
                    IR.record' False n params' conss []
                _ -> parseError
            _ -> parseError
        _ -> parseError
    _ -> parseError

buildClassConsIR :: BuilderMonad m => Lexer.Token -> m IR.SomeTerm
buildClassConsIR = \tok -> case Ast.unspan tok of
    Ast.App nameTok fieldToks -> case Ast.unspan nameTok of
        Ast.Cons name -> case Ast.unspan fieldToks of
            Ast.List fields -> IR.recordCons' name =<< mapM buildClassField fields
            _ -> parseError
        _ -> parseError
    _ -> parseError

buildClassField :: BuilderMonad m => Lexer.Token -> m IR.SomeTerm
buildClassField = \tok -> case Ast.unspan tok of
    Ast.App t tp -> case Ast.unspan t of
        Ast.App ff ns -> case Ast.unspan ff of
            Ast.Var "#fields#" -> case Ast.unspan ns of
                Ast.List names -> do
                    names' <- Mutable.fromList (getFieldName <$> names)
                    IR.recordFields' names' =<< buildIR tp
                _ -> parseError
            _ -> parseError
        _ -> parseError
    _ -> parseError

getFieldName :: Lexer.Token -> Name
getFieldName = \tok -> case Ast.unspan tok of
    Ast.Var n -> n
    _         -> error "FIXME"



-- App
--   (App (App (Var "class_:") (Cons "Foox")) (List []))
--   (List
--      [ App
--          (Cons "Vector")
--          (List
--             [ App
--                 (App (Var "#fields#") (List [ Var "x" , Var "y" , Var "z" ]))
--                 (Cons "Int")
--             , App
--                 (App (Var "#fields#") (List [ Var "r" , Var "t" , Var "y" ]))
--                 (Cons "String")
--             ])
--      ])

buildCaseOfIR :: MixFixBuilder
buildCaseOfIR arg args = assertSingleArg args $ \a -> let
    match t = let
        notFunc = t & Ast.ast .~ Ast.Invalid Invalid.CaseWayNotFunction
        in buildIR $ case Ast.unspan t of
            Ast.InfixApp l op r
              -> if Ast.unspan op == Ast.Operator ":" then t else notFunc
            Ast.Invalid {} -> t
            _              -> notFunc
    in do
        base <- buildIR arg
        case Ast.unspan a of
            Ast.Block lines -> IR.match' base . convert1' =<< match <$$> lines
            _               -> IR.match' base . pure      =<< match a

buildListIR :: MixFixBuilder
buildListIR arg args = builAppsIR args =<< case Ast.unspan arg of
    Ast.List as -> IR.list' =<< buildIR <$$> as
    _           -> parseError

buildTupleIR :: MixFixBuilder
buildTupleIR = \arg args -> do
    print "TUPLE"
    pprint arg
    builAppsIR args =<< case Ast.unspan arg of
        Ast.List []  -> IR.tuple' []
        Ast.List [a] -> IR.grouped' =<< buildIR a
        Ast.List as  -> IR.tuple'   =<< buildIR <$$> as
        _            -> parseError

buildRealIR :: BuilderMonad m => Lexer.Token -> Lexer.Token -> m IR.SomeTerm
buildRealIR (Spanned _ (Ast.Number integral)) (Spanned _ (Ast.Number fractional)) = do
    intPart  <- Mutable.fromList (toList integral)
    fracPart <- Mutable.fromList (toList fractional)
    IR.number' 10 intPart fracPart

buildFunctionIR :: MixFixBuilder
buildFunctionIR arg args = case args of
    [paramLst, body] -> do
        case Ast.unspan paramLst of
            Ast.List params -> do
                let name = checkFunctionName arg
                name'   <- buildIR name
                params' <- buildIR <$$> params
                body'   <- buildIR body
                IR.function' name' params' body'
            _ -> parseError

checkFunctionName :: Lexer.Token -> Lexer.Token
checkFunctionName = fmap handle where
    handle = \case
        Ast.Var      a -> Ast.Var      a
        Ast.Operator a -> Ast.Operator a
        Ast.Invalid Invalid.EmptyExpression
           -> Ast.Invalid Invalid.MissingFunctionName
        _  -> Ast.Invalid Invalid.InvalidFunctionName
{-# INLINE checkFunctionName #-}

specInvalid :: Invalid.Symbol -> Lexer.Token -> Lexer.Token
specInvalid inv = fmap handle where
    handle = \case
        Ast.Invalid {} -> Ast.Invalid inv
        x -> x
{-# INLINE specInvalid #-}



isOperator n = (== Ast.Operator n)

infixl 4 <$$>
(<$$>) :: (Traversable t, Monad m) => (a -> m b) -> t a -> m (t b)
(<$$>) = mapM
{-# INLINE (<$$>) #-}

collectApps :: NonEmpty Lexer.Token -> Lexer.Token -> (Lexer.Token, NonEmpty Lexer.Token)
collectApps = \apps tok -> case Ast.unspan tok of
    Ast.App f a -> collectApps (a <| apps) f
    _           -> (tok, apps)

collectSpan :: [Lexer.Token] -> CodeSpan
collectSpan = \lst -> case lst of
    []       -> mempty
    (s : ss) -> view Ast.span s <> collectSpan ss
{-# INLINE collectSpan #-}

t <| (a :| as) = t :| (a : as)


-- App (App : a) b





-- TODO: refactor
instance Convertible1 NonEmpty [] where
    convert1 = \(a:|as) -> a:as
    {-# INLINE convert1 #-}
