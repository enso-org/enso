{-# LANGUAGE CPP                  #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Compilation.Pass.Inference.Calling where

import Prelude.Luna

import Control.Monad.Except                         (throwError, ExceptT, runExceptT)
import Data.Container                               (add)
import Data.Construction
import Data.Either                                  (rights)
import Data.Prop
import Data.Record.Match
import Data.Layer_OLD
import Luna.Runtime.Dynamics                      (Static)
import Old.Luna.Syntax.Term.Class                         hiding (source)
import Data.Graph.Builder                           as Graph hiding (run)
import Data.Graph                                   as Graph hiding (add)
import qualified Data.Graph.Backend.NEC               as NEC
import Luna.Syntax.Model.Layer
import Luna.Syntax.Model.Network.Builder            (dupCluster, replacement, redirect, readSuccs, requester, originSign, Sign (..))
import Luna.Syntax.Model.Network.Builder.Node
import Luna.Syntax.Model.Network.Builder.Term.Class (runNetworkBuilderT, NetGraph, NetLayers, NetCluster)
import Luna.Syntax.Model.Network.Class              ()
import Luna.Syntax.Model.Network.Term
import Type.Inference

import qualified Data.Map as Map
import           Data.Map (Map)

import qualified Luna.Syntax.Term.Function as Function

import           Luna.Compilation.Stage.TypeCheck       (ProgressStatus (..), TypeCheckerPass, hasJobs, runTCPass)
import           Luna.Compilation.Stage.TypeCheck.Class (MonadTypeCheck)
import qualified Luna.Compilation.Stage.TypeCheck.Class as TypeCheck
import           Data.Layer_OLD.Cover_OLD


-- FIXME[MK]: Do not explicitly type stuff here as NetGraph, solve the problems with typing it differently

#define PassCtx(m) ( term  ~ Draft Static                         \
                   , ls    ~ NetLayers                            \
                   , edge  ~ Link (ls :<: term)                   \
                   , node  ~ (ls :<: term)                        \
                   , clus  ~ NetCluster                           \
                   , graph ~ Hetero (NEC.Graph n e c)             \
                   , BiCastable     e edge                        \
                   , BiCastable     n node                        \
                   , BiCastable     c clus                        \
                   , MonadBuilder graph (m)                       \
                   , NodeInferable  (m) (ls :<: term)             \
                   , TermNode Var   (m) (ls :<: term)             \
                   , TermNode Acc   (m) (ls :<: term)             \
                   , TermNode Cons  (m) (ls :<: term)             \
                   , TermNode Lam   (m) (ls :<: term)             \
                   , TermNode Unify (m) (ls :<: term)             \
                   , TermNode Curry (m) (ls :<: term)             \
                   , Clusterable Node node clus (m)               \
                   , Destructor (m) (Ref Edge edge)               \
                   , ReferencedM Node graph (m) node              \
                   , ReferencedM Edge graph (m) edge              \
                   , MonadIO (m) \
                   )

data CallError = NotAFuncallNode | NotACurryNode | UnresolvedFunction | MalformedFunction deriving (Show, Eq)

type CallErrorT = ExceptT CallError

isUni :: PassCtx(m) => Ref Node node -> m Bool
isUni r = do
    n <- read r
    return $ caseTest (uncover n) $ do
        of' $ \(Unify _ _) -> True
        of' $ \ANY -> False

unifyArgTypes :: PassCtx(CallErrorT m) => Function.Signature (Ref Node node) -> Ref Node node ->  [Ref Node node] -> CallErrorT m [Ref Node node]
unifyArgTypes fptr caller args = do
    let getType = follow (prop Type) >=> follow source
    argTps  <- mapM getType args
    argFTps <- mapM getType $ (unlayer <$> fptr ^. Function.args) -- FIXME[WD->MK] handle arg names. Using unlayer for now
    argUnis <- zipWithM unify argFTps argTps
    mapM_ (flip (reconnect $ prop TCData . requester) $ Just caller) argUnis
    mapM_ (flip withRef $ prop TCData . originSign .~ Negative) argUnis
    return argUnis

unifyAppOutType :: PassCtx(CallErrorT m) => Function.Signature (Ref Node node) -> Ref Node node -> [Ref Node node] -> CallErrorT m (Ref Node node)
unifyAppOutType fptr app args = do
    let getType = follow (prop Type) >=> follow source
    outTp   <- getType app
    outFTp  <- getType $ fptr ^. Function.out
    outUni  <- unify outFTp outTp
    reconnect (prop TCData . requester) outUni $ Just app
    return outUni

unifyCurryOutType :: PassCtx(CallErrorT m) => Function.Signature (Ref Node node) -> Ref Node node -> [Ref Node node] -> CallErrorT m (Ref Node node)
unifyCurryOutType fptr curry args = do
    let getType = follow (prop Type) >=> follow source
    currentTp <- getType curry
    outFTp  <- getType $ fptr ^. Function.out
    argFTps <- mapM getType $ (unlayer <$> fptr ^. Function.args)
    let outstandingArgTypes = drop (length args) argFTps
    curryTp <- lam (arg <$> outstandingArgTypes) outFTp
    print "curry!"
    outUni  <- unify curryTp currentTp
    reconnect (prop TCData . requester) outUni $ Just curry
    return outUni

setupCall :: (PassCtx(CallErrorT m), Monad m) => Ref Node node -> [Ref Node node] -> Ref Cluster clus -> CallErrorT m (Ref Cluster clus, Function.Signature (Ref Node node))
setupCall caller args funClus = do
    (cls, trans) <- dupCluster funClus $ show caller
    fptr <- follow (prop Lambda) cls <?!> MalformedFunction
    withRef caller $ (prop TCData . replacement ?~ cast cls)
    zipWithM (reconnect $ prop TCData . redirect) (unlayer <$> fptr ^. Function.args) (Just <$> args)
    return (cls, fptr)

makeFuncall :: (PassCtx(CallErrorT m), Monad m) => Ref Node node -> [Ref Node node] -> Ref Cluster clus -> CallErrorT m [Ref Node node]
makeFuncall app args funClus = do
    (cls, fptr) <- setupCall app args funClus
    argUnis     <- unifyArgTypes fptr app args
    outUni      <- unifyAppOutType fptr app args
    importUnis  <- filterM isUni =<< members cls
    reconnect (prop TCData . redirect) app $ Just $ fptr ^. Function.out
    return $ importUnis <> (outUni : argUnis)

makeCurriedCall :: (PassCtx(CallErrorT m), Monad m) => Ref Node node -> [Ref Node node] -> Ref Cluster clus -> CallErrorT m [Ref Node node]
makeCurriedCall curry args funClus = do
    (cls, fptr) <- setupCall curry args funClus
    argUnis     <- unifyArgTypes fptr curry args
    outUni      <- unifyCurryOutType fptr curry args
    importUnis  <- filterM isUni =<< members cls
    withRef cls $ prop Lambda . _Just . Function.args %~ drop (length args)
    return $ importUnis <> (outUni : argUnis)

processApp :: (PassCtx(CallErrorT m), Monad m) => Ref Node node -> CallErrorT m [Ref Node node]
processApp ref = do
    node <- read ref
    caseTest (uncover node) $ do
        of' $ \(App f as) -> do
            funReplacement <- (follow (prop TCData . replacement . casted) =<< follow source f) <?!> UnresolvedFunction
            args <- mapM (follow source . unlayer) as
            makeFuncall ref args funReplacement
        of' $ \ANY -> throwError NotAFuncallNode

processCurry :: (PassCtx(CallErrorT m), Monad m) => Ref Node node -> CallErrorT m [Ref Node node]
processCurry ref = do
    node <- read ref
    caseTest (uncover node) $ do
        of' $ \(Curry f as) -> do
            funReplacement <- (follow (prop TCData . replacement . casted) =<< follow source f) <?!> UnresolvedFunction
            args <- mapM (follow source . unlayer) as
            makeCurriedCall ref args funReplacement
        of' $ \ANY -> throwError NotAFuncallNode

-----------------------------
-- === TypeCheckerPass === --
-----------------------------

data FunctionCallingPass = FunctionCallingPass deriving (Show, Eq)

instance ( PassCtx(CallErrorT m)
         , PassCtx(m)
         , MonadTypeCheck (ls :<: term) m
         ) => TypeCheckerPass FunctionCallingPass m where
    hasJobs _ = do
        tc <- TypeCheck.get
        let apps  = tc ^. TypeCheck.uncalledApps
            currs = tc ^. TypeCheck.uncalledApps
        return $ (not . null $ apps) || (not . null $ currs)

    runTCPass _ = do
        apps        <- view TypeCheck.uncalledApps <$> TypeCheck.get
        appResults  <- mapM (runExceptT . processApp) apps
        currs       <- view TypeCheck.uncalledCurries <$> TypeCheck.get
        currResults <- mapM (runExceptT . processCurry) currs
        let withRefs      = zip apps appResults
            appFailures   = fst <$> filter (isLeft . snd) withRefs
            curryRefs     = zip apps currResults
            curryFailures = fst <$> filter (isLeft . snd) withRefs
        TypeCheck.modify_ $ (TypeCheck.unresolvedUnis %~ (++ (concat $ rights $ appResults ++ currResults)))
                          . (TypeCheck.uncalledApps    .~ appFailures)
                          . (TypeCheck.uncalledCurries .~ curryFailures)
        if length appFailures == length apps && length curryFailures == length currs
            then return Stuck
            else return Progressed
