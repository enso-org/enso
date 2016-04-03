{-# LANGUAGE CPP                       #-}
{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Luna.Compilation.Pass.Interpreter.Interpreter where

import           Prelude.Luna                                    hiding (pre, succ)

import           Control.Monad                                   (forM_)
import           Control.Monad.Event                             (Dispatcher)
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

import           Luna.Compilation.Pass.Interpreter.Class         (InterpreterMonad, InterpreterT, runInterpreterT)
import           Luna.Compilation.Pass.Interpreter.Env           (Env)
import qualified Luna.Compilation.Pass.Interpreter.Env           as Env
import           Luna.Compilation.Pass.Interpreter.Layer         (InterpreterData (..), InterpreterLayer, EvalMonad, evalMonad, ValueErr(..))
import qualified Luna.Compilation.Pass.Interpreter.Layer         as Layer

import           Luna.Runtime.Dynamics                           (Dynamic, Static)
import           Luna.Syntax.Term.Expr                           (Lam (..), Acc (..), App (..), Native (..), Blank (..), Unify (..), Var (..), Cons (..))
import           Luna.Syntax.Model.Network.Builder               (redirect, replacement, readSuccs)
import           Luna.Syntax.Model.Layer
import           Luna.Syntax.Model.Network.Builder.Node          (NodeInferable, TermNode)
import           Luna.Syntax.Model.Network.Builder.Node.Inferred
import           Luna.Syntax.Model.Network.Term
import qualified Luna.Syntax.Term.Lit                            as Lit

import           Type.Inference

-- import qualified Luna.Library.StdLib                             as StdLib

import           Luna.Syntax.Term.Function                       (Arg, Function (..), Signature (..))
import qualified Luna.Syntax.Term.Function                       as Function
import qualified Luna.Syntax.Term.Function.Argument              as Arg

--import qualified Luna.Evaluation.Session                         as Session

import           GHC.Prim                                        (Any)

import           Control.Monad.Catch                             (MonadCatch, MonadMask, handleAll, handleJust)
import           Control.Monad.Ghc                               (GhcT)
import           Control.Exception                               (SomeException(..), AsyncException(..), throwIO)
import           Language.Haskell.Session                        (GhcMonad)
import qualified Language.Haskell.Session                        as HS
import qualified Language.Haskell.Session.Hint.Eval as HEval
import           Unsafe.Coerce   -- TODO: move to another module


import           Data.Digits                                     (unDigits, digits)
import           Data.Ratio
-- import           Luna.Syntax.Model.Network.Builder.Term.Class    (NetLayers)
import           Luna.Syntax.Model.Network.Builder.Term.Class    (NetGraph, NetLayers, NetCluster, runNetworkBuilderT, TermBuilder)

import           Data.String.Utils                               (replace)

import           System.Clock

import qualified GHC.Paths              as Paths
import qualified Control.Monad.Ghc      as MGHC
import qualified GHC
import           Control.Concurrent     (threadDelay)
import           GHC.Exception          (fromException, Exception)
import           Control.DeepSeq        (deepseq, force)
import qualified DynFlags               as GHC




-------

-- initialize :: GhcMonad m => Config -> m ()
-- initialize config = do
--     globalPkgDb <- liftIO $ expand' $ Config.pkgDb $ Config.global config
--     localPkgDb  <- liftIO $ expand' $ Config.pkgDb $ Config.local config
--     let isNotUser GHC.UserPkgConf = False
--         isNotUser _ = True
--         extraPkgConfs p = [ GHC.PkgConfFile globalPkgDb
--                           , GHC.PkgConfFile localPkgDb
--                           ] ++ filter isNotUser p
--     flags <- GHC.getSessionDynFlags
--     _ <- GHC.setSessionDynFlags flags
--                 { GHC.extraPkgConfs = extraPkgConfs
--                 , GHC.hscTarget = GHC.HscInterpreted
--                 , GHC.ghcLink   = GHC.LinkInMemory
--                 --, GHC.verbosity = 4
--                 }
--     return ()


-- run :: Config -> Ghc a -> IO a
-- run config r = do
--     topDir <- liftIO $ expand' $ Config.topDir $ Config.ghcS config
--     GHC.runGhc (Just topDir) r

-- topDir =  "x/base/lib/ghc-7.8.4"
-- global = "x/pkgDb"
-- local = "y/pkgDb"


-----

-- TODO: move to another module and encapsulate

findSymbol :: HS.SessionMonad m => String -> String -> m Any
findSymbol name tpe = unsafeCoerce <$> HEval.interpretTyped name tpe

appArg :: Any -> Any -> Any
appArg = unsafeCoerce

unsafeCast :: Any -> a
unsafeCast = unsafeCoerce

toAny :: a -> Any
toAny = unsafeCoerce



--

externalLibs = False

libdir = "/opt/ghc/7.10.3/lib64/ghc-7.10.3"
globalPkgDb = "/opt/ghc/7.10.3/lib64/ghc-7.10.3/package.conf.d"
localPkgDb  = "/home/adam/.ghc/x86_64-linux-7.10.3/package.conf.d"


-- globalPkgDb = "/usr/lib64/ghc-7.10.2/package.conf.d"
-- localPkgDb  = "/home/adam/.ghc/x86_64-linux-7.10.2/package.conf.d"


-- libdir = "/home/adam/.stack/global/.stack-work/install/x86_64-linux/lts-4.1/7.10.3/lib/"
-- libdir = ".stack/global/.stack-work/install/x86_64-linux/lts-4.1/7.10.3/lib/x86_64-linux-ghc-7.10.3/"

libDirectory = if externalLibs then libdir else Paths.libdir

runGHC :: (MGHC.MonadIO m, MonadMask m, Functor m) => MGHC.GhcT m a -> m a
runGHC session = MGHC.runGhcT (Just libDirectory) $ initializeGHC >> session
-- runGHC session = MGHC.runGhcT (Just Paths.libdir) $ initializeGHC >> session

initializeGHC :: GhcMonad m => m ()
initializeGHC = do
    -- HS.setStrFlags ["-fno-ghci-sandbox"]
    let isNotUser GHC.UserPkgConf = False
        isNotUser _ = True
        extraPkgConfs p = [ GHC.PkgConfFile globalPkgDb
                          , GHC.PkgConfFile localPkgDb
                          ] ++ filter isNotUser p
    flags <- GHC.getSessionDynFlags
    void  $  GHC.setSessionDynFlags flags
                { GHC.hscTarget     = GHC.HscInterpreted
                , GHC.ghcLink       = GHC.LinkInMemory
                -- , GHC.extraPkgConfs = extraPkgConfs
                , GHC.ctxtStkDepth  = 1000
                -- , GHC.verbosity     = 4
                }
    if externalLibs then do
                            flags <- GHC.getSessionDynFlags
                            void  $  GHC.setSessionDynFlags flags
                                        { GHC.extraPkgConfs = extraPkgConfs
                                        }
                    else return ()


defaultImports :: [HS.Import]
defaultImports = [ "Prelude"
                 , "Control.Applicative"
                 , "Control.Monad"
                 , "Data.List"
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
                                    , graph ~ Hetero (NEC.Graph n e c)                \
                                    , BiCastable e edge                               \
                                    , BiCastable n node                               \
                                    , MonadIO (m)                                     \
                                    , MonadBuilder graph (m)                          \
                                    , NodeInferable (m) node                          \
                                    , TermNode Lam  (m) node                          \
                                    , HasProp InterpreterData node                    \
                                    , Prop    InterpreterData node ~ InterpreterLayer \
                                    , InterpreterMonad (Env (Ref Node node)) (m)      \
                                    , MonadMask (m)                                   \
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

markDirty :: InterpreterCtx(m, ls, term) => Ref Node (ls :<: term) -> m ()
markDirty ref = do
    node <- read ref
    write ref (node & prop InterpreterData . Layer.dirty .~ True)

setValue :: InterpreterCtx(m, ls, term) => ValueErr (EvalMonad Any) -> Ref Node (ls :<: term) -> Integer -> m ()
setValue value ref startTime = do
    endTime <- liftIO getCPUTime
    -- putStrLn $ "startTime " <> show startTime <> " endTime " <> show endTime
    let !time = endTime - startTime
    node <- read ref
    let dirty = isLeft value
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
    -- putStrLn $ "startTime " <> show startTime <> " endTime " <> show endTime
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

getValueString :: InterpreterCtx(m, ls, term) => Ref Node (ls :<: term) -> m String
getValueString ref = do
    typeName <- getTypeName ref
    value    <- getValue    ref
    case value of
        Left err  -> return ""
        Right val -> do -- $notImplemented --
                        pureVal <- liftIO val
                        return $ getValueByType typeName pureVal
                        where
                           getValueByType typeName pureVal
                               | typeName == Right "String" = show ((unsafeCast pureVal) :: String)
                               | typeName == Right "Int"    = show ((unsafeCast pureVal) :: Integer)
                               | typeName == Right "Bool"   = show ((unsafeCast pureVal) :: Bool)
                               | otherwise                 = "unknown type"


displayValue :: InterpreterCtx(m, ls, term) => Ref Node (ls :<: term) -> m ()
displayValue ref = do
    typeName <- getTypeName ref
    valueString <- getValueString ref
    putStrLn $ "Type " <> show typeName <> " value " <> valueString

getTypeName :: InterpreterCtx(m, ls, term) => Ref Node (ls :<: term) -> m (ValueErr String)
getTypeName ref = do
    node  <- read ref
    tpRef <- follow source $ node # Type
    getTypeNameForType tpRef

getListType :: InterpreterCtx(m, ls, term) => [Ref Node (ls :<: term)] -> m (ValueErr String)
getListType (arg:more:_) = return $ Left ["Too many parameters to list"]
getListType (arg:_)      = do
    name <- getTypeNameForType arg
    return $ (\s -> "[" <> s <> "]") <$> name
getListType _            = return $ Left ["List should have a type parameter"]

getArgsType :: InterpreterCtx(m, ls, term) => [Ref Node (ls :<: term)] -> m (ValueErr String)
getArgsType args@(arg:rest) = do
    typeNameMays <- mapM getTypeNameForType args
    return $ (\typeNames -> "(" <> intercalate ") (" typeNames <> ")") <$> sequence typeNameMays
getArgsType _ = return $ Left ["Bad type arguments"]

getTypeNameForType :: InterpreterCtx(m, ls, term) => Ref Node (ls :<: term) -> m (ValueErr String)
getTypeNameForType tpRef = do
    tp    <- read tpRef
    caseTest (uncover tp) $ do
        of' $ \(Cons (Lit.String s) args) -> do
            let rawArgs  = unlayer <$> args
            sigElems     <- mapM (follow source) rawArgs
            case s of
                "List"      -> getListType sigElems
                "Histogram" -> return $ Right "[(Int, Int)]"
                _           -> if null sigElems
                    then return $ Right s
                    else do
                           argsTypeMay <- getArgsType sigElems
                           return $ case argsTypeMay of
                               Left err       -> Left err
                               Right argsType -> Right $ s <> " " <> argsType
        of' $ \(Var (Lit.String name)) -> return . Right $ replace "#" "_" name
        of' $ \ANY                     -> return $ Left ["Ambiguous node type"]


-- run :: (MonadIO m, MonadMask m, Functor m) => GhcT m a -> m a

getNativeType :: (InterpreterCtx(m, ls, term), HS.SessionMonad (GhcT m)) => Ref Node (ls :<: term) -> m (ValueErr String)
getNativeType ref = do
    node  <- read ref
    tpRef <- follow source $ node # Type
    tp    <- read tpRef
    caseTest (uncover tp) $ do
        of' $ \(Lam args out) -> do
            let rawArgs  = unlayer <$> args
            sigOut        <- (follow source) out
            sigOutName    <- (fmap ((evalMonad <> " ") <>)) <$> getTypeNameForType sigOut
            sigParams     <- mapM (follow source) rawArgs
            sigParamNames <- mapM getTypeNameForType sigParams
            let sigElemNames = sigParamNames <> [sigOutName]
            return $ if all isRight sigElemNames
                then
                    let funSig = intercalate " -> " (rights sigElemNames) in
                    Right funSig
                else
                    Left ["Could not evaluate all signature elements types"]

        of' $ \ANY -> return $ Left ["Incorrect native type"]


exceptionHandler :: (InterpreterCtx(m, ls, term), HS.SessionMonad (GhcT m)) => SomeException -> m (ValueErr (EvalMonad Any))
exceptionHandler e = do
    let asyncExcMay = ((fromException e) :: Maybe AsyncException)
    putStrLn $ "Exception catched:\n" <> show e
    case asyncExcMay of
        Nothing  -> return $ Left [show e]
        Just exc -> do
                        putStrLn "Async exception occurred"
                        liftIO $ throwIO e

evaluateNative :: (InterpreterCtx(m, ls, term), HS.SessionMonad (GhcT m)) => Ref Node (ls :<: term) -> [Ref Node (ls :<: term)] -> m (ValueErr (EvalMonad Any))
evaluateNative ref args = do -- $notImplemented -- do
    -- putStrLn $ "Evaluating native"
    node <- read ref
    (name, tpNativeE) <- caseTest (uncover node) $ do
       of' $ \(Native nameStr) -> do
           tpNativeMay <- getNativeType ref
           return (unwrap' nameStr, tpNativeMay)
       of' $ \ANY -> return ("", Left ["Error: native cannot be evaluated"])

    case tpNativeE of
       Left err -> return $ Left err
       Right tpNative -> do
           -- putStrLn $ name <> " :: " <> tpNative
           valuesE <- argumentsValues args
           case valuesE of
               Left err -> return $ Left err
               Right valuesM -> do
                   res <- handleAll exceptionHandler $ runGHC $ do
                       HS.setImports defaultImports
                       fun  <- findSymbol name tpNative
                       args <- liftIO $ sequence valuesM
                       let resA = foldl appArg fun args
                       let resM = unsafeCast resA :: EvalMonad Any
                       res <- liftIO $ resM
                       -- liftIO $ threadDelay 5000000
                       return $ Right $ return res
                   return $ res

-- join $ foldl (\f a -> f <*> a) (pure f) values

-- TODO: import buildera
--     runGraph :: InterprCtx(m) => Graph -> m a -> IO a
-- interpretFunction :: Graph -> Signature -> Any -> IO Any
-- interpretFunction g sig arg = runGraph

-- handleVar ::  (InterpreterCtx(m, ls, term), HS.SessionMonad (GhcT m)) => NetGraph -> Ref Node (ls :<: term) -> m ()
interpretFunction ::  (InterpreterCtx(m, ls, term), HS.SessionMonad (GhcT m)) => Hetero (NEC.Graph n e c) -> Signature (Ref Node (ls :<: term)) -> Any -> m (EvalMonad Any)
-- interpretFunction ::  Hetero (NEC.Graph n e c) -> Signature nodeRef -> Any -> IO Any
interpretFunction g sig arg = do
    -- let g = testG
    let val = Right arg

    (v, g) <- runBuild g $ do
        let input = head $ sig ^. Function.args
        -- withRef input $ (prop InterpreterData . Layer.dirty .~ False)
        --               . (prop InterpreterData . Layer.value .~ val)
        -- return ()
        evaluateNode $ sig ^. Function.out



    -- let a = toAny 0 :: Any
    -- Either
    case v of
        Left err -> return $ $notImplemented -- error "No value evaluated"
        Right val -> return val




-- runBuild (g :: NetGraph) m = runInferenceT ELEMENT (Proxy :: Proxy (Ref Node (NetLayers :<: Draft Static)))
runBuild (g :: Hetero (NEC.Graph n e c)) m = runInferenceT ELEMENT (Proxy :: Proxy (Ref Node (NetLayers :<: Draft Static)))
                                           $ runNetworkBuilderT g m

evalBuild g m = fmap snd $ runBuild g m


testG :: Hetero (NEC.Graph n e c)
testG = $notImplemented

testSig :: Signature (Ref Node (ls :<: term))
testSig = $notImplemented

testRef :: Ref Node (ls :<: term)
testRef = $notImplemented

evaluateNode :: (InterpreterCtx(m, ls, term), HS.SessionMonad (GhcT m)) => Ref Node (ls :<: term) -> m (ValueErr (EvalMonad Any))
evaluateNode ref = do
    startTime <- liftIO getCPUTime
    -- putStrLn $ "startTime " <> show startTime
    node <- read ref
    -- g <- lift $ get
    g <- GraphBuilder.get


    let a = toAny 0 :: Any
        sig = testSig

    interpretFunction g sig a

    -- -- putStrLn $ "evaluating " <> show ref
    case (node # TCData) ^. redirect of
        Just redirect -> do
            redirRef <- (follow source) redirect
            -- putStrLn $ "redirecting to " <> show redirRef
            evaluateNode redirRef
            copyValue redirRef ref startTime
        Nothing -> do
            if isDirty node
                then caseTest (uncover node) $ do
                    of' $ \(Unify l r)  -> return ()
                    of' $ \(Acc n t)    -> return ()
                    of' $ \(Var n)      -> do
                        let clusterPtr = fromJust $ (node # TCData) ^. replacement
                        -- let (cluster :: (Ref Cluster _)) = cast clusterPtr
                        -- let cluster :: NetCluster = cast clusterPtr
                        -- clustPtr :: Loc Cluster
                        -- let abc = cast <$> clustPtr
                        -- clust <- read $ cast <$> clustPtr

                        -- sig <- fromJust $ clust # Lambda
                        -- g <- GraphBuilder.get
                        -- v <- interpretFunction g sig
                        -- let val = toAny v
                        -- setValue (Right $ val) ref startTime
                        return ()
                    of' $ \(App f args) -> do
                        funRef       <- follow source f
                        unpackedArgs <- unpackArguments args
                        mapM evaluateNode unpackedArgs
                        funNode      <- read funRef
                        nativeVal    <- caseTest (uncover funNode) $ do
                            of' $ \native@(Native nameStr)  -> evaluateNative funRef unpackedArgs
                            of' $ \ANY                      -> return $ Left ["evaluating non native function"]
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


evaluateNodes :: InterpreterCtx(m, ls, term) => [Ref Node (ls :<: term)] -> m ()
evaluateNodes reqRefs = do
    mapM_ collectNodesToEval reqRefs
    mapM_ evaluateNode =<< Env.getNodesToEval


#define PassCtx(m, ls, term) ( ls    ~ NetLayers                               \
                             , term  ~ Draft Static                            \
                             , node  ~ (ls :<: term)                           \
                             , edge  ~ Link node                               \
                             , graph ~ Hetero (NEC.Graph n e c)                \
                             , BiCastable e edge                               \
                             , BiCastable n node                               \
                             , MonadIO (m)                                     \
                             , MonadBuilder graph (m)                          \
                             , NodeInferable (m) node                          \
                             , TermNode Lam  (m) node                          \
                             , MonadFix (m)                                    \
                             , HasProp InterpreterData node                    \
                             , Prop    InterpreterData node ~ InterpreterLayer \
                             , MonadMask (m)                                   \
                             , ReferencedM Node graph (m) node                 \
                             , ReferencedM Edge graph (m) edge                 \
                             , Dispatcher ELEMENT (Ptr Node ('Known (NetLayers :<: Draft Static))) (m) \
                             , Dispatcher CONNECTION (Ptr Edge ('Known (Arc (NetLayers :< Draft Static NetLayers) (NetLayers :< Draft Static NetLayers)))) (m) \
                             )

run :: forall env m ls term node edge graph n e c. (PassCtx(InterpreterT env m, ls, term), MonadIO m, MonadFix m, env ~ Env (Ref Node (ls :<: term)))
    => [Ref Node (ls :<: term)] -> m ()
run reqRefs = do
    -- putStrLn $ "Paths.libdir " <> Paths.libdir
    -- putStrLn $ "g " <> show g
    -- putStrLn $ "reqRefs " <> show reqRefs
    -- ((), env) <- flip runInterpreterT (def :: env) $ collectNodesToEval (head reqRefs) runStateT
    -- g <- Builder.get
    -- g <- lift $ Builder.get

    ((), env) <- flip runInterpreterT (def :: env) $ evaluateNodes reqRefs
    -- putStrLn $ "env " <> show env

    -- putStrLn $ show StdLib.symbols

    return ()


testRun :: forall env m ls term node edge graph n e c. (PassCtx(InterpreterT env m, ls, term), MonadIO m, MonadFix m, env ~ Env (Ref Node (ls :<: term)))
    => [Ref Node (ls :<: term)] -> m ()
testRun reqRefs = do
    putStrLn "I'm here"
    return ()
