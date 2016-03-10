{-# LANGUAGE CPP                       #-}

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
import           Luna.Compilation.Pass.Interpreter.Layer         (InterpreterData (..), InterpreterLayer)
import qualified Luna.Compilation.Pass.Interpreter.Layer         as Layer

import           Luna.Evaluation.Runtime                         (Dynamic, Static)
import           Luna.Syntax.AST.Term                            (Lam (..), Acc (..), App (..), Native (..), Blank (..), Unify (..), Var (..), Cons (..))
import           Luna.Syntax.Model.Network.Builder               (redirect)
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

import           Control.Monad.Catch                             (MonadCatch, MonadMask, catchAll)
import           Control.Monad.Ghc                               (GhcT)
import           Language.Haskell.Session                        (GhcMonad)
import qualified Language.Haskell.Session                        as HS

import           Data.Digits                                     (unDigits, digits)
import           Data.Ratio
import           Luna.Syntax.Model.Network.Builder.Term.Class    (NetLayers)

import           Data.String.Utils                               (replace)



convertBase :: Integral a => a -> a -> a
convertBase radix = unDigits radix . digits 10



convertRationalBase :: Integer -> Rational -> Rational
convertRationalBase radix rational = nom % den where
    nom = convertBase radix (numerator   rational)
    den = convertBase radix (denominator rational)

numberToAny :: Lit.Number -> Any
numberToAny (Lit.Number radix (Lit.Rational r)) = Session.toAny $ convertRationalBase (toInteger radix) r
numberToAny (Lit.Number radix (Lit.Integer  i)) = Session.toAny $ convertBase         (toInteger radix) i

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
    mapM (follow target) $ node # Succs

isDirty :: (Prop InterpreterData n ~ InterpreterLayer, HasProp InterpreterData n) => n -> Bool
isDirty node = (node # InterpreterData) ^. Layer.dirty

isRequired :: (Prop InterpreterData n ~ InterpreterLayer, HasProp InterpreterData n) => n -> Bool
isRequired node = (node # InterpreterData) ^. Layer.required

markDirty :: InterpreterCtx(m, ls, term) => Ref Node (ls :<: term) -> m ()
markDirty ref = do
    node <- read ref
    write ref (node & prop InterpreterData . Layer.dirty .~ True)

setValue :: InterpreterCtx(m, ls, term) => Maybe Any -> Ref Node (ls :<: term) -> m ()
setValue value ref = do
    node <- read ref
    let dirty = isNothing value
    write ref (node & prop InterpreterData . Layer.value .~ value
                    & prop InterpreterData . Layer.dirty .~ dirty
              )
    valueString <- getValueString ref
    displayValue ref
    updNode <- read ref
    write ref $ updNode & prop InterpreterData . Layer.debug .~ valueString

copyValue :: InterpreterCtx(m, ls, term) => Ref Node (ls :<: term) -> Ref Node (ls :<: term) -> m ()
copyValue fromRef toRef = do
    fromNode <- read fromRef
    toNode   <- read toRef
    let value = (fromNode # InterpreterData) ^. Layer.value
        debug = (fromNode # InterpreterData) ^. Layer.debug
    let dirty = isNothing value

    write toRef (toNode & prop InterpreterData . Layer.value .~ value
                        & prop InterpreterData . Layer.dirty .~ dirty
                        & prop InterpreterData . Layer.debug .~ debug
                )

getValue :: InterpreterCtx(m, ls, term) => Ref Node (ls :<: term) -> m (Maybe Any)
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
argumentValue :: InterpreterCtx(m, ls, term) => Ref Node (ls :<: term) -> m (Maybe Any)
argumentValue ref = do
    node <- read ref
    return $ (node # InterpreterData) ^. Layer.value

argumentsValues :: InterpreterCtx(m, ls, term) => [Ref Node (ls :<: term)] -> m (Maybe [Any])
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
    value <- getValue ref
    return $ case value of
                Nothing  -> ""
                Just val -> if typeName == Just "String"
                               then show ((Session.unsafeCast val) :: String)
                               else (if typeName == Just "Int"
                                   then show ((Session.unsafeCast val) :: Integer)
                                   else "unknown type")

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
    return $ (\typeNames -> "(" <> intercalate " " typeNames <> ")") <$> sequence typeNameMays
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
            sigElems     <- mapM (follow source) (rawArgs <> [out])
            sigElemNames <- mapM getTypeNameForType sigElems
            return $ if all isJust sigElemNames
                then
                    let funSig = intercalate " -> " (catMaybes sigElemNames) in
                    Just funSig
                else
                    Nothing

        of' $ \ANY -> return Nothing -- error "Incorrect native type"

evaluateNative :: (InterpreterCtx(m, ls, term), HS.SessionMonad (GhcT m)) => Ref Node (ls :<: term) -> [Ref Node (ls :<: term)] -> m (Maybe Any)
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
                Just values -> do
                    res <- flip catchAll (\e -> do putStrLn $ show e; return $ Session.toAny False) $ HS.run $ do
                        HS.setImports Session.defaultImports
                        fun <- Session.findSymbol name tpNative
                        let res = foldl Session.appArg fun $ Session.toAny <$> values
                        -- let res = Session.toAny 0
                        return res
                    return $ Just res

evaluateNode :: (InterpreterCtx(m, ls, term), HS.SessionMonad (GhcT m)) => Ref Node (ls :<: term) -> m ()
evaluateNode ref = do
    node <- read ref
    -- putStrLn $ "evaluating " <> show ref
    case (node # TCData) ^. redirect of
        Just redirect -> do
            redirRef <- (follow source) redirect
            -- putStrLn $ "redirecting to " <> show redirRef
            evaluateNode redirRef
            copyValue redirRef ref
        Nothing -> do
            -- TODO: use caches
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
                        setValue nativeVal ref
                        return ()
                    of' $ \(Lit.String str)                 -> do
                        setValue (Just $ Session.toAny str) ref
                    of' $ \number@(Lit.Number radix system) -> do
                        setValue (Just $ numberToAny number) ref
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
    -- putStrLn $ "g " <> show g
    putStrLn $ "reqRefs " <> show reqRefs
    -- ((), env) <- flip runInterpreterT (def :: env) $ collectNodesToEval (head reqRefs) runStateT
    ((), env) <- flip runInterpreterT (def :: env) $ evaluateNodes reqRefs
    putStrLn $ "env " <> show env

    -- putStrLn $ show StdLib.symbols

    return ()
