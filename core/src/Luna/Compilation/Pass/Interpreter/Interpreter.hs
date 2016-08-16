{-# LANGUAGE CPP                       #-}
{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Luna.Compilation.Pass.Interpreter.Interpreter where

import           Prelude.Luna                                    hiding (pre, succ)

import           Control.Monad                                   (forM_)
import           Control.Monad.Event                             (Dispatcher)
import           Control.Monad.Except                            (runExceptT, ExceptT (..), throwError, MonadError)
import           Control.Monad.Reader                            (runReaderT, ReaderT, MonadReader, ask, local)

import           Data.Construction
import           Data.Graph
import qualified Data.Graph.Backend.NEC                          as NEC
import           Data.Graph.Builder                              hiding (get)
import qualified Data.Graph.Builder                              as GraphBuilder
import           Data.Prop
import           Data.Record                                     hiding (cons, Value)
import           Development.Placeholders

import           Luna.Compilation.Pass.Interpreter.Class         (InterpreterMonad, InterpreterT, runInterpreterT, evalInterpreterT)
import           Luna.Compilation.Pass.Interpreter.Env           (Env, enrichScope, lookupVar)
import qualified Luna.Compilation.Pass.Interpreter.Env           as Env
import           Luna.Compilation.Pass.Interpreter.Layer         (InterpreterData (..), InterpreterLayer, EvalMonad, evalMonad, ValueErr(..))
import           Luna.Compilation.Pass.Interpreter.StdScope      (stdScope)
import qualified Luna.Compilation.Pass.Interpreter.Layer         as Layer
import           Luna.Compilation.Pass.Interpreter.Value

import           Luna.Runtime.Dynamics                           (Dynamic, Static)
import           Old.Luna.Syntax.Term.Class                      (Lam (..), Acc (..), App (..), Native (..), Blank (..), Unify (..), Var (..), Cons (..), Curry (..))
import           Luna.Syntax.Model.Network.Builder               (redirect, readSuccs, tcErrors)
import           Luna.Syntax.Model.Layer
import           Luna.Syntax.Model.Network.Builder.Node          (NodeInferable, TermNode)
import           Luna.Syntax.Model.Network.Builder.Node.Inferred
import           Luna.Syntax.Model.Network.Term
import qualified Old.Luna.Syntax.Term.Expr.Lit                   as Lit

import           Type.Inference

import           Luna.Syntax.Term.Function                       (Arg)

import           Control.Monad.Catch                             (MonadCatch, handleAll, handleJust)
import           Control.Exception                               (SomeException(..), AsyncException(..), throwIO)


import           Data.Digits                                     (unDigits, digits)
import           Data.Ratio
import           Luna.Syntax.Model.Network.Builder.Term.Class    (NetGraph, NetLayers, NetCluster, runNetworkBuilderT, TermBuilder_OLD, NetworkBuilderT, NetRawNode, NetRawCluster)

import           GHC.Exception          (fromException, Exception)
import           Data.Layer_OLD.Cover_OLD

convertBase :: Integral a => a -> a -> a
convertBase radix = unDigits radix . digits 10

convertRationalBase :: Integer -> Rational -> Rational
convertRationalBase radix rational = nom % den where
    nom = convertBase radix (numerator   rational)
    den = convertBase radix (denominator rational)

numberToValue :: Lit.Number -> Value
numberToValue (Lit.Number radix (Lit.Rational r)) = unsafeToValue $ convertRationalBase (toInteger radix) r
numberToValue (Lit.Number radix (Lit.Integer  i)) = unsafeToValue (fromIntegral $ convertBase (toInteger radix) i :: Int)
numberToValue (Lit.Number radix (Lit.Double   d)) = unsafeToValue $ d

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

markDirty :: InterpreterCtx(m, ls, term) => Bool -> Ref Node (ls :<: term) -> m ()
markDirty dirty ref = do
    node <- read ref
    write ref (node & prop InterpreterData . Layer.dirty .~ dirty)

setValue :: InterpreterCtx(m, ls, term) => ValueErr Value -> Ref Node (ls :<: term) -> m ()
setValue value ref = withRef ref $ (prop InterpreterData . Layer.value .~ value)

getValue :: InterpreterCtx(m, ls, term) => Ref Node (ls :<: term) -> m (ValueErr Value)
getValue ref = do
    node <- read ref
    return $ (node # InterpreterData) ^. Layer.value

markSuccessors :: InterpreterCtx(m, ls, term) => Ref Node (ls :<: term) -> m ()
markSuccessors ref = do
    node <- read ref
    unless (isDirty node) $ do
        markDirty True ref
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

unpackArguments :: InterpreterCtx(m, ls, term) => [Arg (Ref Edge (Link (ls :<: term)))] -> m [Ref Node (ls :<: term)]
unpackArguments args = mapM (follow source . unlayer) args

collectNodesToEval :: InterpreterCtx(m, ls, term) => Ref Node (ls :<: term) -> m ()
collectNodesToEval ref = do
    Env.addNodeToEval ref
    prevs <- pre ref
    forM_ prevs $ \p -> do
        whenM (isDirty <$> read p) $
            collectNodesToEval p

exceptionHandler :: SomeException -> IO (ValueErr a)
exceptionHandler e = case asyncExcMay of
        Nothing  -> return $ Left [show e]
        Just exc -> liftIO $ throwIO e
    where asyncExcMay = ((fromException e) :: Maybe AsyncException)

liftBinders :: [a] -> (Value -> Value) -> Value -> Value
liftBinders [] f v = f v
liftBinders (_ : as) f v = do
    Function func <- v
    unsafeToValue $ \(x :: Data) -> liftBinders as f (func x)

makeConst :: Value -> [a] -> Value
makeConst v []       = v
makeConst v (_ : as) = unsafeToValue $ \(x :: Data) -> makeConst v as

handleArgs :: [Ref Node n] -> Ref Node n -> Maybe Value
handleArgs [] _ = Nothing
handleArgs (a : as) r | r == a    = Just . unsafeToValue $ \x -> makeConst (Pure x) as
                      | otherwise = unsafeToValue . (const :: Value -> Data -> Value) <$> handleArgs as r

composeManyToMany :: [a] -> [Value] -> Value -> Value
composeManyToMany []       fs g = unsafeAppFun g fs
composeManyToMany (_ : as) fs g = unsafeToValue $ \(x :: Data) -> let appArg f = unsafeAppFun f [Pure x]
                                                                  in  composeManyToMany as (appArg <$> fs) (appArg g)

evaluateAST :: ( InterpreterCtx(m, ls, term)
               , MonadError [String] m
               , MonadReader [Ref Node (ls :<: term)] m
               ) => Ref Node (ls :<: term) -> m Value
evaluateAST ref = do
    node <- read ref
    args <- ask
    let tcData = node # TCData
    caseTest (uncover node) $ do
        of' $ \(Lit.String str) -> return . flip makeConst args . unsafeToValue $ str
        of' $ return . flip makeConst args . numberToValue
        of' $ \(Cons n _) -> do
            v <- case unwrap n of
                "True"  -> return $ unsafeToValue True
                "False" -> return $ unsafeToValue False
                _       -> throwError ["Undefined constructor"]
            return $ makeConst v args
        of' $ \(App f as) -> do
            funRef       <- follow source f
            funVal       <- evaluateNode funRef
            unpackedArgs <- unpackArguments as
            argVals      <- mapM evaluateNode unpackedArgs
            return $ case argVals of
                [] -> funVal
                _  -> composeManyToMany args argVals funVal
        of' $ \(Acc (Lit.String name) t) -> do
            targetRef <- follow source t
            targetVal <- evaluateNode targetRef
            return $ liftBinders args (unsafeGetProperty name) targetVal
        of' $ \(Var n) -> case tcData ^. redirect of
            Just e -> do
                ref         <- follow source e
                assignedVal <- evaluateNode ref
                return assignedVal
                -- TODO[MK]: Figure output repacking with lambdas (how to encode the transform without AST in place!)
                {-v <- liftIO $ handleAll exceptionHandler $ Right <$> toIO assignedVal-}
                {-case v of-}
                    {-Left errs -> do-}
                        {-let msgs = ("Runtime exception: " <>) <$> errs-}
                        {-setValue (Left msgs) ref-}
                        {-throwError msgs-}
                    {-Right v -> return $ Pure v-}
            Nothing -> do
                v <- lookupVar (unwrap n) <?!> ["Undefined variable"]
                return $ makeConst v args
        of' $ \(Lam as out) -> do
            unpackedArgs <- unpackArguments as
            outRef <- follow source out
            local (++ unpackedArgs) $ evaluateNode outRef
        of' $ \ANY -> throwError ["Unexpected node type"]

evaluateNode :: ( InterpreterCtx(m, ls, term)
                , MonadError [String] m
                , MonadReader [Ref Node (ls :<: term)] m
                ) => Ref Node (ls :<: term) -> m Value
evaluateNode ref = do
    args <- ask
    val  <- case handleArgs args ref of
        Just v -> return $ Right v
        _      -> do
            node <- read ref
            let tcData = node # TCData
            if isDirty node && null (tcData ^. tcErrors)
                then do
                    markDirty False ref
                    Right <$> evaluateAST ref
                else getValue ref
    setValue val ref
    case val of
        Left  e -> throwError e
        Right v -> return v

evaluateNodes :: (InterpreterCtx(m, ls, term), InterpreterCtx((ExceptT [String] (ReaderT [Ref Node (ls :<: term)] m)), ls, term)) => [Ref Node (ls :<: term)] -> m ()
evaluateNodes reqRefs = do
    mapM_ collectNodesToEval $ reverse reqRefs
    mapM_ (flip runReaderT [] . runExceptT . evaluateNode) =<< Env.getNodesToEval

run :: forall env m ls term node edge graph clus n e c. (Monad m, InterpreterCtx(InterpreterT env m, ls, term), InterpreterCtx((ExceptT [String] (ReaderT [Ref Node (ls :<: term)] (InterpreterT env m))), ls, term), env ~ Env (Ref Node (ls :<: term)))
    => [Ref Node (ls :<: term)] -> m ()
run reqRefs = do
    ((), env) <- flip runInterpreterT (def :: env) $ do
        enrichScope stdScope
        evaluateNodes reqRefs
    return ()
