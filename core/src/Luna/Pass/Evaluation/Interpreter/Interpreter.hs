{-# LANGUAGE CPP                       #-}
{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Luna.Pass.Evaluation.Interpreter.Interpreter where

import           Luna.Prelude                                    hiding (pre, succ)

import           Control.Monad                                   (forM_)
import           Old.Control.Monad.Event                             (Dispatcher)
import           Control.Monad.Except                            (runExceptT, ExceptT (..), throwError, MonadError)
import           Control.Monad.Reader                            (runReaderT, ReaderT, MonadReader, ask, local)

import           Data.IORef                                      (IORef, newIORef, readIORef, modifyIORef, writeIORef)

import           Data.Graph
import qualified Data.Graph.Backend.NEC                          as NEC
import           Data.Graph.Builder                              hiding (get)
import           Old.Data.Prop
import           Data.Record                                     hiding (cons, Value)
import           Data.Map                                        (Map)
import qualified Data.Map                                        as Map

import           Luna.Pass.Evaluation.Interpreter.Class         (InterpreterMonad, InterpreterT, runInterpreterT)
import           Luna.Pass.Evaluation.Interpreter.Env           (Env, enrichScope, lookupVar)
import qualified Luna.Pass.Evaluation.Interpreter.Env           as Env
import           Luna.Pass.Evaluation.Interpreter.Layer         (InterpreterData (..), InterpreterLayer, ValueErr)
import           Luna.Pass.Evaluation.Interpreter.StdScope      (stdScope)
import qualified Luna.Pass.Evaluation.Interpreter.Layer         as Layer
import           Luna.Pass.Evaluation.Interpreter.Value

import           Old.Luna.Runtime.Dynamics                           (Static)
import           Old.Luna.Syntax.Term.Class                      (Lam (..), Acc (..), App (..), Var (..), Cons (..))
import           Old.Luna.Syntax.Model.Network.Builder               (redirect, readSuccs, tcErrors)
import           Old.Luna.Syntax.Model.Layer
import           Old.Luna.Syntax.Model.Network.Builder.Node          (NodeInferable, TermNode)
import           Old.Luna.Syntax.Model.Network.Term
import qualified Old.Luna.Syntax.Term.Expr.Lit                   as Lit

import           Luna.IR.Function                       (Arg)

import           Control.Monad.Catch                             (handleAll)
import           Control.Exception                               (SomeException(..), AsyncException(..), throwIO)


import           Data.Digits                                     (unDigits, digits)
import           Data.Ratio
import           Old.Luna.Syntax.Model.Network.Builder.Term.Class    (NetLayers, NetCluster, NetRawNode, NetRawCluster)

import           GHC.Exception          (fromException)
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

exceptionHandler :: SomeException -> ExceptT String IO a
exceptionHandler e = case asyncExcMay of
        Nothing  -> throwError $ show e
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

data Stack = Stack { _global :: StackFrame
                   , _local  :: [StackFrame]
                   }

newtype StackFrame = StackFrame (IORef (Map Int Value))
makeWrapped ''StackFrame

makeStack :: MonadIO m => m Stack
makeStack = Stack <$> liftIO (StackFrame <$> newIORef Map.empty) <*> pure []

readStack :: MonadIO m => Stack -> Ref Node n -> m (Maybe Value)
readStack (Stack g l) r = readFrames g l where
    readFrames g []       = readFrame g
    readFrames g (f : fs) = do
        res <- readFrame g
        case res of
            Nothing -> readFrames g fs
            _ -> return res

    readFrame (StackFrame mref) = Map.lookup (unwrap r) <$> liftIO (readIORef mref)

putValue :: MonadIO m => Stack -> Ref Node n -> Value -> m ()
putValue (Stack g l) r v = putVal g l where
    putVal g []      = putFrame g
    putVal _ (f : _) = putFrame f

    putFrame (StackFrame mref) = liftIO $ modifyIORef mref (Map.insert (unwrap r) v)

setValue :: ( InterpreterCtx(m, ls, term)
            , MonadReader (Stack, [Ref Node (ls :<: term)]) m
            ) => ValueErr Value -> Ref Node (ls :<: term) -> m ()
setValue value ref = do
    (_, binds) <- ask
    node <- read ref
    when (node ^. prop InterpreterData . Layer.binders >= length binds) $
        withRef ref $ (prop InterpreterData . Layer.value   .~ value)
                    . (prop InterpreterData . Layer.binders .~ length binds)
getValue :: ( InterpreterCtx(m, ls, term)
            , MonadReader (Stack, [Ref Node (ls :<: term)]) m
            ) => Ref Node (ls :<: term) -> m (ValueErr Value)
getValue ref = do
    node  <- read ref
    (_, binds) <- ask
    let intData = node # InterpreterData
    let outstandingBinds = length binds - (intData ^. Layer.binders)
    return $ case intData ^. Layer.value of
        Left err -> Left err
        Right v  -> Right $ makeConst v $ take outstandingBinds $ repeat ()

allocFrame :: MonadIO m => Stack -> m Stack
allocFrame (Stack g l) = liftIO $ do
    newFrame <- StackFrame <$> newIORef Map.empty
    return $ Stack g (newFrame : l)

clearLocal :: MonadIO m => Stack -> m ()
clearLocal (Stack _ ((StackFrame l) : ls)) = liftIO $ writeIORef l Map.empty
clearLocal _ = return ()

evaluateAST :: ( InterpreterCtx(m, ls, term)
               , MonadError [String] m
               , MonadReader (Stack, [Ref Node (ls :<: term)]) m
               ) => Ref Node (ls :<: term) -> m Value
evaluateAST ref = do
    node <- read ref
    (stack, args) <- ask
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
                return $ flip (liftBinders args) assignedVal $ \val -> do
                    memo <- readStack stack ref
                    v <- case memo of
                        Just v  -> v
                        Nothing -> Monadic $ handleAll exceptionHandler $ toExceptIO val
                    putValue stack ref (Pure v)
                    return v
            Nothing -> do
                v <- lookupVar (unwrap n) <?!> ["Undefined variable"]
                return $ makeConst v args
        of' $ \(Lam as out) -> do
            unpackedArgs <- unpackArguments as
            outRef       <- follow source out
            newStack     <- allocFrame stack
            let newArgs  =  args ++ unpackedArgs
            funVal <- local (const (newStack, newArgs)) $ evaluateNode outRef
            return $ liftBinders newArgs (clearLocal newStack >>) funVal
        of' $ \ANY -> throwError ["Unexpected node type"]

evaluateNode :: ( InterpreterCtx(m, ls, term)
                , MonadError [String] m
                , MonadReader (Stack, [Ref Node (ls :<: term)]) m
                ) => Ref Node (ls :<: term) -> m Value
evaluateNode ref = do
    (_, args) <- ask
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

evaluateNodes :: (InterpreterCtx(m, ls, term), InterpreterCtx((ExceptT [String] (ReaderT (Stack, [Ref Node (ls :<: term)]) m)), ls, term)) => [Ref Node (ls :<: term)] -> m ()
evaluateNodes reqRefs = do
    stack <- makeStack
    mapM_ (flip runReaderT (stack, []) . runExceptT . evaluateNode) reqRefs

run :: forall env m ls term node edge graph clus n e c. (Monad m, InterpreterCtx(InterpreterT env m, ls, term), InterpreterCtx((ExceptT [String] (ReaderT (Stack, [Ref Node (ls :<: term)]) (InterpreterT env m))), ls, term), env ~ Env (Ref Node (ls :<: term)))
    => [Ref Node (ls :<: term)] -> m ()
run reqRefs = do
    ((), env) <- flip runInterpreterT (def :: env) $ do
        enrichScope stdScope
        evaluateNodes reqRefs
    return ()
