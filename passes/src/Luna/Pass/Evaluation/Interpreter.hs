{-# LANGUAGE NoStrict             #-}
{-# LANGUAGE NoStrictData         #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Pass.Evaluation.Interpreter where

import Prologue

import qualified Control.Monad.State.Layered           as State
import qualified Data.Graph.Data.Component.Vector      as ComponentVector
import qualified Data.Map                              as Map
import qualified Data.Mutable.Storable.SmallAutoVector as SmallVector
import qualified Luna.IR                               as IR
import qualified Luna.IR.Aliases                       as Uni
import qualified Luna.IR.Layer                         as Layer
import qualified Luna.Pass                             as Pass
import qualified Luna.Pass.Attr                        as Attr
import qualified Luna.Pass.Data.Stage                  as TC
import qualified Luna.Pass.Data.Error                  as Error
import qualified Luna.Pass.Evaluation.Data.Scope       as Scope
import qualified Luna.Pass.Scheduler                   as Scheduler
import qualified Luna.Runtime                          as Runtime

import Data.IORef                      (newIORef, readIORef)
import Data.Map                        (Map)
import Luna.Pass.Data.Root             (Root (..))
import Luna.Pass.Evaluation.Data.Scope (LocalScope, MonadScope)


------------------------------
-- === Literal Creation === --
------------------------------

mkInt :: Runtime.Units -> Integer -> Runtime.Data
mkInt = Runtime.toData

mkDouble :: Runtime.Units -> Double -> Runtime.Data
mkDouble = Runtime.toData

mkString :: Runtime.Units -> Text -> Runtime.Data
mkString = Runtime.toData

mkNothing :: Runtime.Units -> Runtime.Data
mkNothing imps = Runtime.toData imps ()

----------------------------
-- === Scheduler Flow === --
----------------------------

runInterpreter :: IR.SomeTerm -> Runtime.Units -> TC.Monad Runtime.Value
runInterpreter root units = do
    Scheduler.registerAttr @Runtime.Units
    Scheduler.registerAttr @Root
    Scheduler.registerAttr @ResultValue

    Scheduler.setAttr $ Root root
    Scheduler.setAttr units
    Scheduler.enableAttrByType @ResultValue

    Scheduler.registerPass @TC.Stage @(Interpreter ResultValue)
    Scheduler.runPassByType @(Interpreter ResultValue)

    ResultValue res <- Scheduler.getAttr
    return res

execInterpreter :: IR.SomeTerm -> Runtime.Units -> TC.Monad (IO LocalScope)
execInterpreter root units = do
    Scheduler.registerAttr @Runtime.Units
    Scheduler.registerAttr @Root
    Scheduler.registerAttr @ResultScope

    Scheduler.setAttr $ Root root
    Scheduler.setAttr units
    Scheduler.enableAttrByType @ResultScope

    Scheduler.registerPass @TC.Stage @(Interpreter ResultScope)
    Scheduler.runPassByType @(Interpreter ResultScope)

    ResultScope res <- Scheduler.getAttr
    return res

------------------
-- === Pass === --
------------------

data Interpreter res

data ResultValue = ResultValue Runtime.Value
type instance Attr.Type ResultValue = Attr.Atomic
instance Default ResultValue where
    def = ResultValue $ Runtime.throw "Uninitialized interpreter"

data ResultScope = ResultScope (IO LocalScope)
type instance Attr.Type ResultScope = Attr.Atomic
instance Default ResultScope where
    def = ResultScope $ pure def

type instance Pass.Spec (Interpreter res) t = InterpreterSpec res t
type family InterpreterSpec res t where
    InterpreterSpec _ (Pass.In  Pass.Attrs) = '[Root, Runtime.Units]
    InterpreterSpec r (Pass.Out Pass.Attrs) = '[r]
    InterpreterSpec _ t = Pass.BasicPassSpec t

instance Pass.Definition TC.Stage (Interpreter ResultValue) where
    definition = do
        Root root <- Attr.get
        units  <- Attr.get @Runtime.Units
        result <- interpret units root
        Attr.put $ ResultValue $ Runtime.force
            $ State.evalT result (def :: LocalScope)

instance Pass.Definition TC.Stage (Interpreter ResultScope) where
    definition = do
        Root root <- Attr.get
        units  <- Attr.get @Runtime.Units
        result <- interpret units root
        Attr.put $ ResultScope $ do
            scopeRef <- newIORef def
            Runtime.runIO $ Runtime.runError $ Scope.evalWithRef result scopeRef
            readIORef scopeRef

interpret :: (m ~ t Runtime.Eff, MonadScope m, MonadTrans t)
          => Runtime.Units -> IR.SomeTerm
          -> TC.Pass (Interpreter r) (m Runtime.Data)
interpret glob expr = do
    err <- Error.getError expr
    case err of
        Just e  -> pure $ pure $ Runtime.Error (e ^. Error.contents)
        Nothing -> interpret' glob expr

interpret' :: (m ~ t Runtime.Eff, MonadScope m, MonadTrans t)
           => Runtime.Units -> IR.SomeTerm
           -> TC.Pass (Interpreter r) (m Runtime.Data)
interpret' glob expr = Layer.read @IR.Model expr >>= \case
    Uni.RawString s -> do
        str <- SmallVector.toList s
        let lunaStr = mkString glob (convert str)
        return $ return lunaStr
    Uni.FmtString s -> do
        parts <- ComponentVector.toList s
        case parts of
            []  -> return $ return $ mkString glob ""
            [a] -> interpret glob =<< IR.source a
            _   -> return $ lift $
                Runtime.throw "Interpolated strings not supported yet."
    IR.UniTermNumber n -> do
        isInt <- IR.isInteger n
        num   <- if isInt then mkInt glob    <$> IR.toInteger n
                          else mkDouble glob <$> IR.toDouble n
        return $ return num
    Uni.Var name -> return $ do
        val <- State.gets @LocalScope (Scope.localLookup expr)
        case val of
            Just v  -> return v
            Nothing -> lift $ Runtime.throw $ "Variable not found: "
                                            <> convert name
    Uni.ResolvedDef mod name -> do
        lift . Runtime.force <$> Runtime.lookupSymbol glob mod name
    Uni.Lam i' o' -> do
        bodyVal <- interpret glob =<< IR.source o'
        inputMatcher <- irrefutableMatcher =<< IR.source i'
        return $ do
            env <- State.get @LocalScope
            return $ Runtime.Function $ \d -> do
                newBinds <- inputMatcher $ Runtime.mkThunk d
                State.evalT bodyVal $ Scope.merge newBinds env
    Uni.App f a -> do
        fun <- interpret glob =<< IR.source f
        arg <- interpret glob =<< IR.source a
        return $ do
            env <- State.get @LocalScope
            let fun' = State.evalT fun env
                arg' = State.evalT arg env
            lift $ Runtime.force $ Runtime.applyFun fun' arg'
    Uni.Acc a' n' -> do
        a <- interpret glob =<< IR.source a'
        name <- IR.source n' >>= Layer.read @IR.Model >>= \case
            Uni.Var n -> return n
            _         -> error "interpret: method name is not Var"
        return $ do
            arg <- a
            lift $ Runtime.force $ Runtime.dispatchMethod name arg
    Uni.Unify l' r' -> do
        rhs <- interpret glob =<< IR.source r'
        l   <- IR.source l'
        pat <- irrefutableMatcher l
        return $ do
            env <- State.get @LocalScope
            let rhs' = State.evalT rhs (Scope.localInsert l rhsT env)
                rhsT = Runtime.mkThunk rhs'
            rhsV <- lift $ Runtime.runError $ Runtime.force rhs'
            case rhsV of
                Left e -> do
                    State.modify_ @LocalScope $ Scope.localInsert l
                        $ Runtime.Error $ unwrap e
                    lift $ Runtime.throw $ unwrap e
                Right v -> do
                    State.modify_ @LocalScope . Scope.merge =<< lift (pat v)
                    return v
    Uni.Function n' as' b' -> do
        n <- IR.source n'
        asList <- ComponentVector.toList as'
        as <- traverse (irrefutableMatcher <=< IR.source) asList
        rhs <- interpret glob =<< IR.source b'
        let makeFuns []       e = State.evalT rhs e
            makeFuns (m : ms) e = return $ Runtime.Function $ \d -> do
                newBinds <- m $ Runtime.mkThunk d
                makeFuns ms (Scope.merge newBinds e)
        return $ do
            env <- State.get @LocalScope
            let rhs' = makeFuns as (Scope.localInsert n rhst env)
                rhst = Runtime.mkThunk rhs'
            State.modify_ @LocalScope $ Scope.localInsert n rhst
            return $ Runtime.mkSusp rhs'
    Uni.Seq l' r' -> do
        lhs <- interpret glob =<< IR.source l'
        rhs <- interpret glob =<< IR.source r'
        return $ do
            lV <- lhs
            lift $ Runtime.forceThunks' lV
            rhs
    Uni.ResolvedCons mod cls name fs -> do
        fsList <- ComponentVector.toList fs
        fields <- mapM (interpret glob <=< IR.source) fsList
        let mets = Runtime.getObjectMethodMap glob mod cls
        return $ do
            fs <- sequence fields
            let cons = Runtime.Constructor name fs
            return $ Runtime.Cons $ Runtime.Object mod cls cons mets
    Uni.Match t' cls' -> do
        target <- interpret glob =<< IR.source t'
        cls    <- traverse IR.source =<< ComponentVector.toList cls'
        clauses <- for cls $ \clause -> Layer.read @IR.Model clause >>= \case
            Uni.Lam pat res -> (,) <$> (matcher =<< IR.source pat)
                               <*> (interpret glob =<< IR.source res)
        return $ do
            env <- State.get @LocalScope
            let tgt' = State.evalT target env
            tgt <- lift $ Runtime.force tgt'
            (scope, cl) <- lift $ runMatch clauses tgt
            lift $ State.evalT cl (Scope.merge scope env)
    Uni.Marked _ b' -> do
        b <- IR.source b'
        interpretHere <- Layer.read @IR.Model b >>= \case
            Uni.Unify{}    -> return False
            Uni.Function{} -> return False
            _              -> return True
        e <- interpret glob b
        return $ if not interpretHere then e else do
            e' <- e
            State.modify_ @LocalScope $ Scope.localInsert b e'
            return e'
    s -> return $ lift $ Runtime.throw
             $ "Unexpected (report this as a bug): " <> convert (show s)

------------------------------
-- === Pattern Matching === --
------------------------------

type MatchRes  = (Maybe (Map IR.SomeTerm Runtime.Data), Runtime.Data)
type MatchResM = Runtime.Eff MatchRes
type Matcher   = Runtime.Data -> MatchResM

runMatch :: [(Matcher, a)]
         -> Runtime.Data
         -> Runtime.Eff (Map IR.SomeTerm Runtime.Data, a)
runMatch [] _ = Runtime.throw "Inexhaustive pattern match."
runMatch ((m, p) : ms) d = do
    (res, nextObj) <- m d
    case res of
        Just newScope -> return (newScope, p)
        Nothing       -> runMatch ms nextObj

tryConsMatch :: IR.Name -> [Matcher] -> Matcher
tryConsMatch name fieldMatchers d' = Runtime.force' d' >>= \case
    d@(Runtime.Cons (Runtime.Object mod cls (Runtime.Constructor n fs) ms)) ->
        if n == name then matchFields else return (Nothing, d) where
            matchFields :: MatchResM
            matchFields = do
                results <- zipWithM ($) fieldMatchers fs
                let binds = fmap Map.unions $ sequence $ fst <$> results
                    newObj = Runtime.Cons
                               (Runtime.Object mod cls
                                  (Runtime.Constructor n (snd <$> results)) ms)
                return (binds, newObj)
    d -> return (Nothing, d)

tryBoxedMatch :: (Eq a, Runtime.IsNative a) => a -> Matcher
tryBoxedMatch i d' = Runtime.force' d' >>= \case
    d@(Runtime.Native o) -> return (matchRes, d) where
        matchRes = if Runtime.fromNative o == i then Just def else Nothing
    d -> return (Nothing, d)

matcher :: IR.SomeTerm -> TC.Pass (Interpreter t) Matcher
matcher expr = Layer.read @IR.Model expr >>= \case
    Uni.Var _ -> return $ \d -> return (Just $ Map.fromList [(expr, d)], d)
    Uni.ResolvedCons mod cls n as -> do
        asList <- ComponentVector.toList as
        argMatchers <- traverse (matcher <=< IR.source) asList
        return $ tryConsMatch n argMatchers
    IR.UniTermNumber n -> do
        isInt <- IR.isInteger n
        case isInt of
            True -> do
                num <- IR.toInteger n
                return $ tryBoxedMatch num
            _ -> do
                num <- IR.toDouble n
                return $ tryBoxedMatch num
    Uni.RawString s -> do
        s <- SmallVector.toList s
        return $ tryBoxedMatch $ (convert s :: Text)
    Uni.Blank -> return $ \d -> return (Just def, d)
    s -> return $ const $
        Runtime.throw $ convert $ "Unexpected pattern: " <> show s

irrefutableMatcher :: IR.SomeTerm
                   -> TC.Pass (Interpreter t) (Runtime.Data
                          -> Runtime.Eff (Map IR.SomeTerm Runtime.Data))
irrefutableMatcher expr = do
    m <- matcher expr
    return $ \d -> do
        res <- fst <$> m d
        maybe (Runtime.throw "Irrefutable pattern match failed.") return res

