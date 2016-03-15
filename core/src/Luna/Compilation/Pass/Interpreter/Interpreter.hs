{-# LANGUAGE CPP                       #-}
{-# LANGUAGE BangPatterns              #-}

module Luna.Compilation.Pass.Interpreter.Interpreter where

import           Prologue                                        hiding (Getter, Setter, pre, read, succ, ( # ))

import           Control.Monad                                   (forM_)
import           Control.Monad.Event                             (Dispatcher)
import           Control.Monad.Trans.Identity
import           Control.Monad.Trans.State
import           Data.Maybe                                      (isNothing, isJust, catMaybes)

import           Data.Construction
import           Data.Graph
import           Data.Graph.Backend.VectorGraph                  hiding (source, target)
import           Data.Graph.Builder                              hiding (get)
import qualified Data.IntSet                                     as IntSet
import           Data.Prop
import           Data.Record                                     hiding (cons)
import           Development.Placeholders

import           Luna.Compilation.Pass.Interpreter.Class         (InterpreterMonad, InterpreterT, runInterpreterT)
import           Luna.Compilation.Pass.Interpreter.Env           (Env)
import qualified Luna.Compilation.Pass.Interpreter.Env           as Env
import           Luna.Compilation.Pass.Interpreter.Layer         (InterpreterData (..), InterpreterLayer, EvalMonad, evalMonad)
import qualified Luna.Compilation.Pass.Interpreter.Layer         as Layer

import           Luna.Evaluation.Runtime                         (Dynamic, Static)
import           Luna.Syntax.AST.Term                            (Lam (..), Acc (..), App (..), Native (..), Blank (..), Unify (..), Var (..), Cons (..))
import           Luna.Syntax.Model.Network.Builder               (redirect, readSuccs)
import           Luna.Syntax.Builder
import           Luna.Syntax.Model.Layer
import           Luna.Syntax.Model.Network.Builder.Node          (NodeInferable, TermNode)
import           Luna.Syntax.Model.Network.Builder.Node.Inferred
import           Luna.Syntax.Model.Network.Term
import qualified Luna.Syntax.AST.Term.Lit                        as Lit

import           Type.Inference

-- import qualified Luna.Library.StdLib                             as StdLib

import           Luna.Syntax.AST.Function                        (Arg)
import qualified Luna.Syntax.AST.Function.Argument               as Arg

import qualified Luna.Evaluation.Session                         as Session

import           GHC.Prim                                        (Any)

import           Control.Monad.Catch                             (MonadCatch, MonadMask, handleAll, handleJust)
import           Control.Monad.Ghc                               (GhcT)
import           Control.Exception                               (SomeException(..), AsyncException(..), throwIO)
import           Language.Haskell.Session                        (GhcMonad)
import qualified Language.Haskell.Session                        as HS

import           Data.Digits                                     (unDigits, digits)
import           Data.Ratio
import           Luna.Syntax.Model.Network.Builder.Term.Class    (NetLayers)

import           Data.String.Utils                               (replace)

import           System.Clock

import qualified GHC.Paths              as Paths
import qualified Control.Monad.Ghc      as MGHC
import qualified GHC
import           Control.Concurrent     (threadDelay)
import           GHC.Exception          (fromException, Exception)


-- libdir = "/home/adam/.cabal/lib/x86_64-linux-ghc-7.10.2/"

runGHC :: (MGHC.MonadIO m, MonadMask m, Functor m) => MGHC.GhcT m a -> m a
-- runGHC session = MGHC.runGhcT (Just libdir) $ initializeGHC >> session
runGHC session = MGHC.runGhcT (Just Paths.libdir) $ initializeGHC >> session

initializeGHC :: GhcMonad m => m ()
initializeGHC = do
    HS.setStrFlags ["-fno-ghci-sandbox"]
    flags <- GHC.getSessionDynFlags
    void  $  GHC.setSessionDynFlags flags
                { GHC.hscTarget     = GHC.HscInterpreted
                , GHC.ghcLink       = GHC.LinkInMemory
                , GHC.ctxtStkDepth  = 1000
                -- , GHC.verbosity     = 4
                }

defaultImports :: [HS.Import]
defaultImports = [ "Prelude"
                 , "Control.Applicative"
                 -- , "Prologue"
                 ]



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
toMonadAny = return . Session.toAny


numberToAny :: Lit.Number -> EvalMonad Any
numberToAny (Lit.Number radix (Lit.Rational r)) = toMonadAny $ convertRationalBase (toInteger radix) r
numberToAny (Lit.Number radix (Lit.Integer  i)) = toMonadAny $ convertBase         (toInteger radix) i
numberToAny (Lit.Number radix (Lit.Double   d)) = toMonadAny $ d

#define InterpreterCtx(m, ls, term) ( ls   ~ NetLayers                                         \
                                    , term ~ Draft Static                                      \
                                    , ne   ~ Link (ls :<: term)                                \
                                    , BiCastable e ne                                          \
                                    , BiCastable n (ls :<: term)                               \
                                    , MonadIO (m)                                              \
                                    , MonadBuilder (Hetero (VectorGraph n e c)) (m)            \
                                    , NodeInferable (m) (ls :<: term)                          \
                                    , TermNode Lam  (m) (ls :<: term)                          \
                                    , HasProp InterpreterData (ls :<: term)                    \
                                    , Prop    InterpreterData (ls :<: term) ~ InterpreterLayer \
                                    , InterpreterMonad (Env (Ref Node (ls :<: term))) (m)      \
                                    , MonadMask (m)                                            \
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

markDirty :: InterpreterCtx(m, ls, term) => Ref Node (ls :<: term) -> m ()
markDirty ref = do
    node <- read ref
    write ref (node & prop InterpreterData . Layer.dirty .~ True)

setValue :: InterpreterCtx(m, ls, term) => Maybe (EvalMonad Any) -> Ref Node (ls :<: term) -> Integer -> m ()
setValue value ref startTime = do
    endTime <- liftIO getCPUTime
    putStrLn $ "startTime " <> show startTime <> " endTime " <> show endTime
    let !time = endTime - startTime
    node <- read ref
    let dirty = isNothing value
    write ref (node & prop InterpreterData . Layer.value .~ value
                    & prop InterpreterData . Layer.dirty .~ dirty
                    & prop InterpreterData . Layer.time  .~ time
              )
    valueString <- getValueString ref
    displayValue ref
    updNode <- read ref
    write ref $ updNode & prop InterpreterData . Layer.debug .~ valueString

copyValue :: InterpreterCtx(m, ls, term) => Ref Node (ls :<: term) -> Ref Node (ls :<: term) -> Integer -> m ()
copyValue fromRef toRef startTime = do
    endTime <- liftIO getCPUTime
    putStrLn $ "startTime " <> show startTime <> " endTime " <> show endTime
    let !time = endTime - startTime
    fromNode <- read fromRef
    toNode   <- read toRef
    let value = (fromNode # InterpreterData) ^. Layer.value
        debug = (fromNode # InterpreterData) ^. Layer.debug
    let dirty = isNothing value

    write toRef (toNode & prop InterpreterData . Layer.value .~ value
                        & prop InterpreterData . Layer.dirty .~ dirty
                        & prop InterpreterData . Layer.time  .~ time
                        & prop InterpreterData . Layer.debug .~ debug
                )

getValue :: InterpreterCtx(m, ls, term) => Ref Node (ls :<: term) -> m (Maybe (EvalMonad Any))
getValue ref = do
    node <- read ref
    return $ (node # InterpreterData) ^. Layer.value

--- sandbox


markSuccessors :: InterpreterCtx(m, ls, term) => Ref Node (ls :<: term) -> m ()
markSuccessors ref = do
    node <- read ref
    -- putStrLn $         "markSuccessors " <> show ref
    unless (isDirty node) $ do
        -- putStrLn $     "marking dirty  " <> show ref
        markDirty ref
        when (isRequired node) $ do
            -- putStrLn $ "addReqNode     " <> show ref
            Env.addNodeToEval ref
            mapM_ markSuccessors =<< succ ref

-- handler

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
argumentValue :: InterpreterCtx(m, ls, term) => Ref Node (ls :<: term) -> m (Maybe (EvalMonad Any))
argumentValue ref = do
    node <- read ref
    return $ (node # InterpreterData) ^. Layer.value

argumentsValues :: InterpreterCtx(m, ls, term) => [Ref Node (ls :<: term)] -> m (Maybe [EvalMonad Any])
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

getValueString :: InterpreterCtx(m, ls, term) => Ref Node (ls :<: term) -> m String
getValueString ref = do
    typeName <- getTypeName ref
    value    <- getValue    ref
    case value of
                Nothing  -> return ""
                Just val -> do
                                -- val :: _
                                pureVal <- liftIO val
                                -- pureVal ::
                                if typeName == Just "String"
                                    then return $ show ((Session.unsafeCast pureVal) :: String)
                                    else (if typeName == Just "Int"
                                        then return $ show ((Session.unsafeCast pureVal) :: Integer)
                                        else return "unknown type")

displayValue :: InterpreterCtx(m, ls, term) => Ref Node (ls :<: term) -> m ()
displayValue ref = do
    typeName <- getTypeName ref
    valueString <- getValueString ref
    putStrLn $ "Type " <> show typeName <> " value " <> valueString


getTypeName :: InterpreterCtx(m, ls, term) => Ref Node (ls :<: term) -> m (Maybe String)
getTypeName ref = do
    node  <- read ref
    tpRef <- follow source $ node # Type
    getTypeNameForType tpRef

getListType :: InterpreterCtx(m, ls, term) => [Ref Node (ls :<: term)] -> m (Maybe String)
getListType (arg:rest) = do
    name <- getTypeNameForType arg
    return $ (\s -> "[" <> s <> "]") <$> name
    -- TODO: log error if args not empty
getListType _ = return Nothing

getArgsType :: InterpreterCtx(m, ls, term) => [Ref Node (ls :<: term)] -> m (Maybe String)
getArgsType args@(arg:rest) = do
    typeNameMays <- mapM getTypeNameForType args
    return $ (\typeNames -> "(" <> intercalate ") (" typeNames <> ")") <$> sequence typeNameMays
getArgsType _ = return Nothing

getTypeNameForType :: InterpreterCtx(m, ls, term) => Ref Node (ls :<: term) -> m (Maybe String)
getTypeNameForType tpRef = do
    tp    <- read tpRef
    caseTest (uncover tp) $ do
        of' $ \(Cons (Lit.String s) args) -> do
            let rawArgs  = unlayer <$> args
            sigElems     <- mapM (follow source) rawArgs
            if s == "List"
                then
                    getListType sigElems
                else do
                    argsTypeMay <- getArgsType sigElems
                    case argsTypeMay of
                        Nothing -> return $ Just s
                        Just argsType -> return . Just $ s <> " " <> argsType
        of' $ \(Var (Lit.String name)) -> return . Just $ replace "#" "_" name
        of' $ \ANY                        -> return Nothing -- error "Ambiguous node type"


-- run :: (MonadIO m, MonadMask m, Functor m) => GhcT m a -> m a

getNativeType :: (InterpreterCtx(m, ls, term), HS.SessionMonad (GhcT m)) => Ref Node (ls :<: term) -> m (Maybe String)
getNativeType ref = do
    node  <- read ref
    tpRef <- follow source $ node # Type
    tp    <- read tpRef
    caseTest (uncover tp) $ do
        of' $ \(Lam args out) -> do
            let rawArgs  = unlayer <$> args
            -- sigElems     <- mapM (follow source) (rawArgs <> [out])
            -- sigElemNames <- mapM getTypeNameForType sigElems
            sigOut        <- (follow source) out
            sigOutName    <- (fmap ((evalMonad <> " ") <>)) <$> getTypeNameForType sigOut
            sigParams     <- mapM (follow source) rawArgs
            sigParamNames <- mapM getTypeNameForType sigParams
            let sigElemNames = sigParamNames <> [sigOutName]
            return $ if all isJust sigElemNames
                then
                    let funSig = intercalate " -> " (catMaybes sigElemNames) in
                    Just funSig
                else
                    Nothing

        of' $ \ANY -> return Nothing -- error "Incorrect native type"


exceptionHandler :: (InterpreterCtx(m, ls, term), HS.SessionMonad (GhcT m)) => SomeException -> m (Maybe (EvalMonad Any))
exceptionHandler e = do
    let asyncExcMay = ((fromException e) :: Maybe AsyncException)
    putStrLn $ "Exception catched:\n" <> show e
    case asyncExcMay of
        Nothing  -> return Nothing
        Just exc -> do
                        putStrLn "Async exception occurred"
                        liftIO $ throwIO e

evaluateNative :: (InterpreterCtx(m, ls, term), HS.SessionMonad (GhcT m)) => Ref Node (ls :<: term) -> [Ref Node (ls :<: term)] -> m (Maybe (EvalMonad Any))
evaluateNative ref args = do
    -- putStrLn $ "Evaluating native"
    node <- read ref
    (name, tpNativeMay) <- caseTest (uncover node) $ do
        of' $ \(Native nameStr) -> do
            tpNativeMay <- getNativeType ref
            return (unwrap' nameStr, tpNativeMay)
        of' $ \ANY -> return ("", Nothing) -- error "Error: native cannot be evaluated"

    case tpNativeMay of
        Nothing -> return Nothing
        Just tpNative -> do
            putStrLn $ name <> " :: " <> tpNative
            valuesMay <- argumentsValues args
            case valuesMay of
                Nothing -> return Nothing
                Just valuesM -> do
                    res <- handleAll exceptionHandler $ runGHC $ do
                        HS.setImports defaultImports
                        fun <- Session.findSymbol name tpNative
                        args <- liftIO $ sequence valuesM
                        -- let res = foldl Session.appArg fun $ Session.toAny <$> values
                        -- let res = foldl (\f a -> Session.appArg f a) fun $ Session.toAny <$> args
                        let resA = foldl (\f a -> Session.appArg f a) fun $ args
                        let resM = Session.unsafeCast resA :: EvalMonad Any
                        res <- liftIO $ resM
                        -- liftIO $ threadDelay 5000000
                        -- let res = Session.toAny 0
                        return $ Just $ return res
                    return res

-- join $ (pure f) (<*> a1) <*> a2
-- foldl (\f a -> f <*> a) (pure f) (values)

evaluateNode :: (InterpreterCtx(m, ls, term), HS.SessionMonad (GhcT m)) => Ref Node (ls :<: term) -> m ()
evaluateNode ref = do
    startTime <- liftIO getCPUTime
    putStrLn $ "startTime " <> show startTime
    node <- read ref
    putStrLn $ "evaluating " <> show ref
    case (node # TCData) ^. redirect of
        Just redirect -> do
            redirRef <- (follow source) redirect
            putStrLn $ "redirecting to " <> show redirRef
            evaluateNode redirRef
            copyValue redirRef ref startTime
        Nothing -> do
            if isDirty node
                then caseTest (uncover node) $ do
                    of' $ \(Unify l r)  -> return ()
                    of' $ \(Acc n t)    -> return ()
                    of' $ \(Var n)      -> return ()
                    of' $ \(App f args) -> do
                        funRef       <- follow source f
                        unpackedArgs <- unpackArguments args
                        mapM evaluateNode unpackedArgs
                        funNode      <- read funRef
                        nativeVal    <- caseTest (uncover funNode) $ do
                            of' $ \native@(Native nameStr)  -> evaluateNative funRef unpackedArgs
                            of' $ \ANY                      -> return Nothing  -- error "evaluating non native function"
                        setValue nativeVal ref startTime
                        return ()
                    of' $ \(Lit.String str)                 -> do
                        setValue (Just $ toMonadAny str) ref startTime
                    of' $ \number@(Lit.Number radix system) -> do
                        setValue (Just $ numberToAny number) ref startTime
                    of' $ \Blank -> return ()
                    of' $ \ANY   -> return ()
                else return ()
    return ()

evaluateNodes :: InterpreterCtx(m, ls, term) => [Ref Node (ls :<: term)] -> m ()
evaluateNodes reqRefs = do
    mapM_ collectNodesToEval reqRefs
    mapM_ evaluateNode =<< Env.getNodesToEval


#define PassCtx(m, ls, term) ( ls   ~ NetLayers                                         \
                             , term ~ Draft Static                                      \
                             , ne   ~ Link (ls :<: term)                                \
                             , BiCastable e ne                                          \
                             , BiCastable n (ls :<: term)                               \
                             , MonadIO (m)                                              \
                             , MonadBuilder ((Hetero (VectorGraph n e c))) (m)          \
                             , NodeInferable (m) (ls :<: term)                          \
                             , TermNode Lam  (m) (ls :<: term)                          \
                             , MonadFix (m)                                             \
                             , HasProp InterpreterData (ls :<: term)                    \
                             , Prop    InterpreterData (ls :<: term) ~ InterpreterLayer \
                             , MonadMask (m)                                            \
                             )

run :: forall env m ls term ne n e c. (PassCtx(InterpreterT env m, ls, term), MonadIO m, MonadFix m, env ~ Env (Ref Node (ls :<: term)))
    => [Ref Node (ls :<: term)] -> m ()
run reqRefs = do
    putStrLn $ "Paths.libdir " <> Paths.libdir
    -- putStrLn $ "g " <> show g
    putStrLn $ "reqRefs " <> show reqRefs
    -- ((), env) <- flip runInterpreterT (def :: env) $ collectNodesToEval (head reqRefs) runStateT
    ((), env) <- flip runInterpreterT (def :: env) $ evaluateNodes reqRefs
    putStrLn $ "env " <> show env

    -- putStrLn $ show StdLib.symbols

    return ()
