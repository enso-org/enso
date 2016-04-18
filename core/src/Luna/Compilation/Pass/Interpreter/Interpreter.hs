{-# LANGUAGE CPP                       #-}
{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Luna.Compilation.Pass.Interpreter.Interpreter where

import           Prelude.Luna                                    hiding (pre, succ)

import           Control.Monad                                   (forM_)
import           Control.Monad.Event                             (Dispatcher)
import           Control.Monad.Except                            (runExceptT, ExceptT (..), throwError)
import           Control.Monad.Trans.Identity
import           Control.Monad.Trans.State
-- import qualified Control.Monad.State                             as State
import           Data.Maybe                                      (isNothing, isJust, catMaybes)
import           Data.Either                                     (rights)

import           Data.Construction
import           Data.Graph
import qualified Data.Graph.Backend.NEC                          as NEC
import           Data.Graph.Builder                              hiding (get)
import qualified Data.Graph.Builder                              as GraphBuilder
import qualified Data.IntSet                                     as IntSet
import           Data.Prop
import           Data.Record                                     hiding (cons, Value)
import           Development.Placeholders

import           Luna.Compilation.Pass.Interpreter.Class         (InterpreterMonad, InterpreterT, runInterpreterT, evalInterpreterT)
import           Luna.Compilation.Pass.Interpreter.Env           (Env)
import qualified Luna.Compilation.Pass.Interpreter.Env           as Env
import           Luna.Compilation.Pass.Interpreter.Layer         (InterpreterData (..), InterpreterLayer, EvalMonad, evalMonad, ValueErr(..))
import qualified Luna.Compilation.Pass.Interpreter.Layer         as Layer
import           Luna.Compilation.Pass.Interpreter.NativeCalls   (lookupNative)

import           Luna.Runtime.Dynamics                           (Dynamic, Static)
import           Old.Luna.Syntax.Term.Class                           (Lam (..), Acc (..), App (..), Native (..), Blank (..), Unify (..), Var (..), Cons (..), Curry (..))
import           Luna.Syntax.Model.Network.Builder               (redirect, replacement, readSuccs, tcErrors)
import           Luna.Syntax.Model.Layer
import           Luna.Syntax.Model.Network.Builder.Node          (NodeInferable, TermNode)
import           Luna.Syntax.Model.Network.Builder.Node.Inferred
import           Luna.Syntax.Model.Network.Term
import qualified Old.Luna.Syntax.Term.Expr.Lit                            as Lit

import           Type.Inference

-- import qualified Luna.Library.StdLib                             as StdLib

import           Luna.Syntax.Term.Function                       (Arg, Function (..), Signature (..))
import qualified Luna.Syntax.Term.Function                       as Function
import qualified Luna.Syntax.Term.Function.Argument              as Arg

--import qualified Luna.Evaluation.Session                         as Session

import           GHC.Prim                                        (Any)

import           Control.Monad.Catch                             (MonadCatch, handleAll, handleJust)
import           Control.Monad.Ghc                               (GhcT)
import           Control.Exception                               (SomeException(..), AsyncException(..), throwIO)
import           Language.Haskell.Session                        (GhcMonad)
import qualified Language.Haskell.Session                        as HS
import qualified Language.Haskell.Session.Hint.Eval as HEval
import           Unsafe.Coerce   -- TODO: move to another module


import           Data.Digits                                     (unDigits, digits)
import           Data.Ratio
-- import           Luna.Syntax.Model.Network.Builder.Term.Class    (NetLayers)
import           Luna.Syntax.Model.Network.Builder.Term.Class    (NetGraph, NetLayers, NetCluster, runNetworkBuilderT, TermBuilder_OLD, NetworkBuilderT, NetRawNode, NetRawCluster)

import           Data.String.Utils                               (replace)

import           System.Clock

import qualified GHC.Paths              as Paths
import qualified Control.Monad.Ghc      as MGHC
import qualified GHC
import           Control.Concurrent     (threadDelay)
import           GHC.Exception          (fromException, Exception)
import           Control.DeepSeq        (deepseq, force)
import qualified DynFlags as GHC
import           Data.Layer_OLD.Cover_OLD

-- TODO: move to another module and encapsulate

findSymbol :: HS.SessionMonad m => String -> String -> m Any
findSymbol name tpe = unsafeCoerce <$> HEval.interpretTyped name tpe

appArg :: Any -> Any -> Any
appArg = unsafeCoerce

unsafeCast :: Any -> a
unsafeCast = unsafeCoerce

toAny :: a -> Any
toAny = unsafeCoerce

convertBase :: Integral a => a -> a -> a
convertBase radix = unDigits radix . digits 10

getCPUTime :: IO Integer
getCPUTime = do
    timeNano <- timeSpecAsNanoSecs <$> getTime ThreadCPUTime
    return $ timeNano `div` 1000

convertRationalBase :: Integer -> Rational -> Rational
convertRationalBase radix rational = nom % den where
    nom = convertBase radix (numerator   rational)
    den = convertBase radix (denominator rational)

toMonadAny :: a -> EvalMonad Any
toMonadAny = return . toAny

numberToAny :: Lit.Number -> EvalMonad Any
numberToAny (Lit.Number radix (Lit.Rational r)) = toMonadAny $ convertRationalBase (toInteger radix) r
numberToAny (Lit.Number radix (Lit.Integer  i)) = toMonadAny $ convertBase         (toInteger radix) i
numberToAny (Lit.Number radix (Lit.Double   d)) = toMonadAny $ d


tryGetBool :: String -> Maybe Bool
tryGetBool boolStr
    | boolStr == "True"  = Just True
    | boolStr == "False" = Just False
    | otherwise          = Nothing

#define InterpreterCtx(m, ls, term) ( ls    ~ NetLayers                               \
                                    , term  ~ Draft Static                            \
                                    , node  ~ (ls :<: term)                           \
                                    , edge  ~ Link node                               \
                                    , n ~ NetRawNode                                  \
                                    , e ~ Link NetRawNode                             \
                                    , c ~ NetRawCluster                               \
                                    , graph ~ Hetero (NEC.Graph n e c)                \
                                    , clus  ~ NetCluster                              \
                                    , BiCastable e edge                               \
                                    , BiCastable n node                               \
                                    , BiCastable c clus                               \
                                    , MonadIO (m)                                     \
                                    , MonadBuilder graph (m)                          \
                                    , NodeInferable (m) node                          \
                                    , TermNode Lam  (m) node                          \
                                    , HasProp InterpreterData node                    \
                                    , Prop    InterpreterData node ~ InterpreterLayer \
                                    , InterpreterMonad (Env (Ref Node node)) (m)      \
                                    , ReferencedM Node graph (m) node                 \
                                    , ReferencedM Edge graph (m) edge                 \
                                    , Dispatcher ELEMENT (Ptr Node ('Known (NetLayers :<: Draft Static))) (m) \
                                    , Dispatcher CONNECTION (Ptr Edge ('Known (Arc (NetLayers :< Draft Static NetLayers) (NetLayers :< Draft Static NetLayers)))) (m) \
                                    )



#define PassCtx(m, ls, term)        ( ls    ~ NetLayers                               \
                                    , term  ~ Draft Static                            \
                                    , node  ~ (ls :<: term)                           \
                                    , edge  ~ Link node                               \
                                    , n ~ NetRawNode                                  \
                                    , e ~ Link NetRawNode                             \
                                    , c ~ NetRawCluster                               \
                                    , graph ~ Hetero (NEC.Graph n e c)                \
                                    , clus  ~ NetCluster                              \
                                    , BiCastable e edge                               \
                                    , BiCastable n node                               \
                                    , BiCastable c clus                               \
                                    , MonadIO (m)                                     \
                                    , MonadBuilder graph (m)                          \
                                    , NodeInferable (m) node                          \
                                    , TermNode Lam  (m) node                          \
                                    , MonadFix (m)                                    \
                                    , HasProp InterpreterData node                    \
                                    , Prop    InterpreterData node ~ InterpreterLayer \
                                    , ReferencedM Node graph (m) node                 \
                                    , ReferencedM Edge graph (m) edge                 \
                                    , Dispatcher ELEMENT (Ptr Node ('Known (NetLayers :<: Draft Static))) (m) \
                                    , Dispatcher CONNECTION (Ptr Edge ('Known (Arc (NetLayers :< Draft Static NetLayers) (NetLayers :< Draft Static NetLayers)))) (m) \
                                    )

pre :: InterpreterCtx(m, ls, term) => Ref Node (ls :<: term) -> m [Ref Node (ls :<: term)]
pre ref = do
    node <- read ref
    mapM (follow source) $ node # Inputs

succ :: InterpreterCtx(m, ls, term) => Ref Node (ls :<: term) -> m [Ref Node (ls :<: term)]
succ ref = do
    node <- read ref
    mapM (follow target) $ readSuccs node

isDirty :: (Prop InterpreterData n ~ InterpreterLayer, HasProp InterpreterData n) => n -> Bool
isDirty node = (node # InterpreterData) ^. Layer.dirty

isRequired :: (Prop InterpreterData n ~ InterpreterLayer, HasProp InterpreterData n) => n -> Bool
isRequired node = (node # InterpreterData) ^. Layer.required

markDirty :: InterpreterCtx(m, ls, term) => Ref Node (ls :<: term) -> Bool -> m ()
markDirty ref dirty = do
    node <- read ref
    write ref (node & prop InterpreterData . Layer.dirty .~ dirty)

setValue :: InterpreterCtx(m, ls, term) => ValueErr (EvalMonad Any) -> Ref Node (ls :<: term) -> Integer -> m ()
setValue value ref startTime = do
    endTime <- liftIO getCPUTime
    let !time = endTime - startTime
    node <- read ref
    let dirty = isLeft value
    write ref (node & prop InterpreterData . Layer.value .~ value
                    & prop InterpreterData . Layer.dirty .~ dirty
                    & prop InterpreterData . Layer.time  .~ time
              )

copyValue :: InterpreterCtx(m, ls, term) => Ref Node (ls :<: term) -> Ref Node (ls :<: term) -> Integer -> m ()
copyValue fromRef toRef startTime = do
    endTime <- liftIO getCPUTime
    let !time = endTime - startTime
    fromNode <- read fromRef
    toNode   <- read toRef
    let value = (fromNode # InterpreterData) ^. Layer.value
        debug = (fromNode # InterpreterData) ^. Layer.debug
    let dirty = isLeft value

    write toRef (toNode & prop InterpreterData . Layer.value .~ value
                        & prop InterpreterData . Layer.dirty .~ dirty
                        & prop InterpreterData . Layer.time  .~ time
                        & prop InterpreterData . Layer.debug .~ debug
                )

getValue :: InterpreterCtx(m, ls, term) => Ref Node (ls :<: term) -> m (ValueErr (EvalMonad Any))
getValue ref = do
    node <- read ref
    return $ (node # InterpreterData) ^. Layer.value

markSuccessors :: InterpreterCtx(m, ls, term) => Ref Node (ls :<: term) -> m ()
markSuccessors ref = do
    node <- read ref
    unless (isDirty node) $ do
        markDirty ref True
        when (isRequired node) $ do
            Env.addNodeToEval ref
            mapM_ markSuccessors =<< succ ref

nodesToExecute :: InterpreterCtx(m, ls, term) =>  m [Ref Node (ls :<: term)]
nodesToExecute = do
    mapM_ collectNodesToEval =<< Env.getNodesToEval
    Env.getNodesToEval

reset :: InterpreterMonad (Env node) m => m ()
reset = Env.clearNodesToEval

connect :: InterpreterCtx(m, ls, term) => Ref Node (ls :<: term) -> Ref Node (ls :<: term) -> m ()
connect prev next = do
    nd <- read prev
    isPrevDirty <- isDirty <$> read prev
    markSuccessors $ if isPrevDirty
        then prev
        else next

markModified :: InterpreterCtx(m, ls, term) => Ref Node (ls :<: term) -> m ()
markModified = markSuccessors


-- interpreter


unpackArguments :: InterpreterCtx(m, ls, term) => [Arg (Ref Edge (Link (ls :<: term)))] -> m [Ref Node (ls :<: term)]
unpackArguments args = mapM (follow source . Arg.__val_) args


-- TODO: handle exception
argumentValue :: InterpreterCtx(m, ls, term) => Ref Node (ls :<: term) -> m (ValueErr (EvalMonad Any))
argumentValue ref = do
    node <- read ref
    return $ (node # InterpreterData) ^. Layer.value

argumentsValues :: InterpreterCtx(m, ls, term) => [Ref Node (ls :<: term)] -> m (ValueErr [EvalMonad Any])
argumentsValues refs = do
    mayValuesList <- mapM argumentValue refs
    return $ sequence mayValuesList

collectNodesToEval :: InterpreterCtx(m, ls, term) => Ref Node (ls :<: term) -> m ()
collectNodesToEval ref = do
    Env.addNodeToEval ref
    prevs <- pre ref
    forM_ prevs $ \p -> do
        whenM (isDirty <$> read p) $
            collectNodesToEval p

exceptionHandler :: SomeException -> IO (ValueErr (EvalMonad Any))
exceptionHandler e = do
    let asyncExcMay = ((fromException e) :: Maybe AsyncException)
    putStrLn $ "Exception catched:\n" <> show e
    case asyncExcMay of
        Nothing  -> return $ Left [show e]
        Just exc -> do
            putStrLn "Async exception occurred"
            liftIO $ throwIO e

-- TCData TCErrors - non empty
evaluateNative :: (InterpreterCtx((ExceptT [String] m), ls, term), Monad m) => Ref Node (ls :<: term) -> [Ref Node (ls :<: term)] -> m (ValueErr (EvalMonad Any))
evaluateNative ref args = runExceptT $ do
    node <- read ref
    name <- caseTest (uncover node) $ do
       of' $ \(Native nameStr) -> do
           return $ unwrap' nameStr
       of' $ \ANY -> throwError ["Error: native cannot be evaluated"]

    markDirty ref False
    valuesE <- argumentsValues args
    values <- ExceptT $ return valuesE
    fun :: Any <- maybe (throwError ["Unknown native call: " <> name]) return $ lookupNative name
    res <- liftIO $ handleAll exceptionHandler $ do
        args <- liftIO $ sequence values
        let resA = foldl appArg fun args
        let resM = unsafeCast resA :: EvalMonad Any
        res <- liftIO $ resM
        return $ Right $ return res
    case res of
        Left errs -> throwError errs
        Right res -> return res

-- join $ foldl (\f a -> f <*> a) (pure f) values


type NodeRef = Ref Node (NetLayers :<: Draft Static)
type BindList = [(NodeRef, Any)]

-- TODO: optimize native haskell functions

interpretNoArgs :: NetGraph -> BindList -> NodeRef -> EvalMonad Any
interpretNoArgs g binds out = do
    v <- evalBuildInt g def $ do -- TODO: pass env (instead of def)
        forM_ binds $ \(r, v) -> do
            let (val :: ValueErr (EvalMonad Any)) = Right $ return v
            withRef r $ (prop InterpreterData . Layer.dirty .~ False)
                      . (prop InterpreterData . Layer.value .~ val)
        res <- evaluateNode out
        case res of
            Left err -> do
                print err
                $notImplemented
            Right val -> return val
    v

interpret' :: NetGraph -> BindList -> [NodeRef] -> NodeRef -> Any
interpret' g binds []         out = toAny $ interpretNoArgs g binds out
interpret' g binds (arg:args) out = toAny $ \v -> interpret' g ((arg, v) : binds) args out

interpret :: NetGraph -> [NodeRef] -> NodeRef -> EvalMonad Any
interpret g args out = return $ interpret' g [] args out

interpretFun :: NetGraph -> Signature NodeRef -> EvalMonad Any
interpretFun g sig = do
    let args = unlayer <$> sig ^. Function.args
        out  = sig ^. Function.out
    interpret g args out

-- IO (Any -> Any -> ... -> Any -> IO Any)

runBuild :: NetworkBuilderT NetGraph m (KnownTypeT ELEMENT NodeRef n) => NetGraph -> m a -> n (a, NetGraph)
runBuild g m = runInferenceT ELEMENT (Proxy :: Proxy (Ref Node (NetLayers :<: Draft Static)))
                                           $ runNetworkBuilderT g m

runBuildInt :: (Monad n, NetworkBuilderT NetGraph m (InterpreterT (Env NodeRef) (KnownTypeT ELEMENT NodeRef n)))
            => NetGraph -> Env (Ref Node (NetLayers :<: Draft Static)) -> m a -> n (a, NetGraph)
runBuildInt g env m = runInferenceT ELEMENT (Proxy :: Proxy (Ref Node (NetLayers :<: Draft Static)))
                                           $ flip evalInterpreterT env
                                           $ runNetworkBuilderT g m

    -- ((), env) <- flip runInterpreterT (def :: env) $ evaluateNodes reqRefs


evalBuild g m = fmap fst $ runBuild g m
evalBuildInt g env m = fmap fst $ runBuildInt g env m


execBuild g m = fmap snd $ runBuild g m


testG :: Hetero (NEC.Graph n e c)
testG = $notImplemented

testSig :: Signature (Ref Node (ls :<: term))
testSig = $notImplemented

testRef :: Ref Node (ls :<: term)
testRef = $notImplemented

evaluateFirstOrderFun :: (InterpreterCtx(m, ls, term)) => Ref Node (ls :<: term) -> m ()
evaluateFirstOrderFun ref = do
    startTime <- liftIO getCPUTime
    node <- read ref
    let clusterPtrMay = (node # TCData) ^. replacement
    case clusterPtrMay of
        Nothing -> return ()
        Just clusterPtr -> do
            let (cluster :: (Ref Cluster NetCluster)) = cast clusterPtr
            sigMay <- follow (prop Lambda) cluster
            case sigMay of
                Nothing -> return ()
                Just sig -> do
                    g <- GraphBuilder.get
                    let val = interpretFun g sig
                    setValue (Right $ val) ref startTime
                    return ()

-- FIXME[MK]: fix this context, by migrating WHOLE file to ExceptT. No time for this right now
evaluateNode :: (InterpreterCtx(m, ls, term), InterpreterCtx((ExceptT [String] m), ls, term)) => Ref Node (ls :<: term) -> m (ValueErr (EvalMonad Any))
evaluateNode ref = do
    startTime <- liftIO getCPUTime
    -- putStrLn $ "startTime " <> show startTime
    node <- read ref
    -- putStrLn $ "evaluating " <> show ref
    let tcData = node # TCData
    when (null $ tcData ^. tcErrors) $ do
        case tcData ^. redirect of
            Just redirect -> do
                redirRef <- (follow source) redirect
                -- putStrLn $ "redirecting to " <> show redirRef
                evaluateNode redirRef
                copyValue redirRef ref startTime
            Nothing -> do
                if isDirty node
                    then caseTest (uncover node) $ do
                        of' $ \(Unify l r)  -> return ()
                        of' $ \(Acc _ _)  -> evaluateFirstOrderFun ref
                        of' $ \(Curry _ _)  -> evaluateFirstOrderFun ref
                        of' $ \(App f args) -> do
                            funRef       <- follow source f
                            unpackedArgs <- unpackArguments args
                            mapM evaluateNode unpackedArgs
                            funNode      <- read funRef
                            nativeVal    <- caseTest (uncover funNode) $ do
                                of' $ \(Native _) -> evaluateNative funRef unpackedArgs
                                of' $ \ANY        -> return $ Left ["evaluating non native function"]
                            setValue nativeVal ref startTime
                        of' $ \(Lit.String str)                 -> do
                            setValue (Right $ toMonadAny str) ref startTime
                        of' $ \number@(Lit.Number radix system) -> do
                            setValue (Right $ numberToAny number) ref startTime
                        of' $ \(Cons (Lit.String s) args) -> do
                            case tryGetBool s of
                                Nothing   -> return ()
                                Just bool -> setValue (Right $ toMonadAny bool) ref startTime
                        of' $ \Blank -> return ()
                        of' $ \ANY   -> return ()
                    else return ()

    getValue ref


evaluateNodes :: (InterpreterCtx(m, ls, term), InterpreterCtx((ExceptT [String] m), ls, term)) => [Ref Node (ls :<: term)] -> m ()
evaluateNodes reqRefs = do
    mapM_ collectNodesToEval reqRefs
    mapM_ evaluateNode =<< Env.getNodesToEval



run :: forall env m ls term node edge graph clus n e c. (Monad m, PassCtx(InterpreterT env m, ls, term), InterpreterCtx((ExceptT [String] (InterpreterT env m)), ls, term), env ~ Env (Ref Node (ls :<: term)))
    => [Ref Node (ls :<: term)] -> m ()
run reqRefs = do
    ((), env) <- flip runInterpreterT (def :: env) $ evaluateNodes reqRefs
    return ()
