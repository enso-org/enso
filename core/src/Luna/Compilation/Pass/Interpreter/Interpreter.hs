{-# LANGUAGE CPP                       #-}

module Luna.Compilation.Pass.Interpreter.Interpreter where

import           Prologue                                        hiding (Getter, Setter, pre, read, succ, ( # ))

import           Control.Monad                                   (forM_)
import           Control.Monad.Event                             (Dispatcher)
import           Control.Monad.Trans.Identity
import           Control.Monad.Trans.State
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
import           Luna.Syntax.AST.Term                            (Lam, Acc (..), App (..), Native (..), Blank (..), Num (..), Str (..), Unify (..), Var (..))
import           Luna.Syntax.Builder
import           Luna.Syntax.Model.Layer
import           Luna.Syntax.Model.Network.Builder.Node          (NodeInferable, TermNode)
import           Luna.Syntax.Model.Network.Builder.Node.Inferred
import           Luna.Syntax.Model.Network.Term

import           Type.Inference

-- import qualified Luna.Library.StdLib                             as StdLib

import           Luna.Syntax.AST.Arg                             (Arg)
import qualified Luna.Syntax.AST.Arg                             as Arg

import qualified Luna.Evaluation.Session                         as Session

import           GHC.Prim                                        (Any)


import           Control.Monad.Catch                             (MonadCatch, MonadMask, catchAll)
import           Control.Monad.Ghc                               (GhcT)
import           Language.Haskell.Session                        (GhcMonad)
import qualified Language.Haskell.Session                        as HS



#define InterpreterCtx(m, ls, term) ( ls   ~ NetLayers a                                       \
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

setValue :: InterpreterCtx(m, ls, term) => Int -> Ref Node (ls :<: term) -> m ()
setValue val ref = do
    node <- read ref
    write ref (node & prop InterpreterData . Layer.value .~ (Just val)
                    & prop InterpreterData . Layer.dirty .~ False)


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
unpackArguments args = mapM (follow source . Arg.__arec) args


argumentValue :: InterpreterCtx(m, ls, term) => Ref Node (ls :<: term) -> m Int
argumentValue ref = do
    node <- read ref
    return $ fromJust $ (node # InterpreterData) ^. Layer.value

argumentsValues :: InterpreterCtx(m, ls, term) => [Ref Node (ls :<: term)] -> m [Int]
argumentsValues refs = mapM argumentValue refs

collectNodesToEval :: InterpreterCtx(m, ls, term) => Ref Node (ls :<: term) -> m ()
collectNodesToEval ref = do
    Env.addNodeToEval ref
    prevs <- pre ref
    forM_ prevs $ \p -> do
        whenM (isDirty <$> read p) $
            collectNodesToEval p


-- run :: (MonadIO m, MonadMask m, Functor m) => GhcT m a -> m a

evaluateNode :: (InterpreterCtx(m, ls, term), HS.SessionMonad (GhcT m)) => Ref Node (ls :<: term) -> m ()
evaluateNode ref = do
    node <- read ref
    putStrLn $ "evaluating " <> show ref
    caseTest (uncover node) $ do
        match $ \(Unify l r)  -> return ()
        match $ \(Acc n t)    -> return ()
        match $ \(Var n)      -> return ()
        match $ \(App f args) -> do
            funRep       <- follow source f
            unpackedArgs <- unpackArguments args
            funNode <- read funRep
            name <- caseTest (uncover funNode) $ do
                match $ \(Str name) -> return name
                match $ \ANY        -> error "Function name is not string"
            putStrLn $ "App " <> show funRep <> " (" <> name <> ") " <> show unpackedArgs
            values <- argumentsValues unpackedArgs
            return ()
        match $ \(Native nameStr argsEdges) -> do
            -- let tpString = (intercalate " -> " $ snd <$> values) <> " -> " <> outType
            let tpString = "Int -> Int -> Int"
            -- args :: _
            let (Str name) = nameStr
            args   <- mapM (follow source) argsEdges
            values <- argumentsValues args
            putStrLn $ "Native " <> name <> " " <> show values
            res <- flip catchAll (\e -> do putStrLn $ show e; return $ Session.toAny (-1)) $ HS.run $ do
                HS.setImports Session.defaultImports
                fun <- Session.findSymbol name tpString
                -- putStrLn $ "found fun " <> show fun
                let res = foldl Session.appArg fun $ Session.toAny <$> values
                return res
            let value = ((Session.unsafeCast res) :: Int)
            putStrLn $ "res " <> show value
            setValue value ref
            return ()
            -- GHC.Prim.Any
        match $ \Blank        -> return ()
        match $ \(Str str)    -> do
            setValue (-1) ref
            -- putStrLn "string"
        match $ \(Num num)    -> setValue num ref
        match $ \ANY          -> return ()
    return ()

evaluateNodes :: InterpreterCtx(m, ls, term) => [Ref Node (ls :<: term)] -> m ()
evaluateNodes reqRefs = do
    mapM_ collectNodesToEval reqRefs
    mapM_ evaluateNode =<< Env.getNodesToEval


#define PassCtx(m, ls, term) ( ls   ~ NetLayers a                                       \
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

run :: forall env m ls term ne a n e c. (PassCtx(InterpreterT env m, ls, term), MonadIO m, MonadFix m, env ~ Env (Ref Node (ls :<: term)))
    => [Ref Node (ls :<: term)] -> m ()
run reqRefs = do
    -- putStrLn $ "g " <> show g
    putStrLn $ "reqRefs " <> show reqRefs
    -- ((), env) <- flip runInterpreterT (def :: env) $ collectNodesToEval (head reqRefs) runStateT
    ((), env) <- flip runInterpreterT (def :: env) $ evaluateNodes reqRefs
    putStrLn $ "env " <> show env

    -- putStrLn $ show StdLib.symbols

    return ()
