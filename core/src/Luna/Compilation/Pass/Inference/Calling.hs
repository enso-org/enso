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
import Data.Layer
import Luna.Runtime.Dynamics                      (Static)
import Luna.Syntax.Term.Expr                         hiding (source)
import Data.Graph.Builder                           as Graph hiding (run)
import Data.Graph                                   as Graph hiding (add)
import qualified Data.Graph.Backend.NEC               as NEC
import Luna.Syntax.Model.Layer
import Luna.Syntax.Model.Network.Builder            (dupCluster, replacement, redirect, readSuccs, requester)
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
                   , Clusterable Node node clus (m)               \
                   , Destructor (m) (Ref Edge edge)               \
                   , ReferencedM Node graph (m) node              \
                   , ReferencedM Edge graph (m) edge              \
                   )

data CallError = NotAFuncallNode | UnresolvedFunction | MalformedFunction deriving (Show, Eq)

type CallErrorT = ExceptT CallError

unifyTypes :: PassCtx(CallErrorT m) => Function.Signature (Ref Node node) -> Ref Node node -> [Ref Node node] -> CallErrorT m [Ref Node node]
unifyTypes fptr app args = do
    let getType = follow (prop Type) >=> follow source
    outTp   <- getType app
    outFTp  <- getType $ fptr ^. Function.out
    outUni  <- unify outFTp outTp
    reconnect (prop TCData . requester) outUni $ Just app
    argTps  <- mapM getType args
    argFTps <- mapM getType $ (unlayer <$> fptr ^. Function.args) -- FIXME[WD->MK] handle arg names. Using unlayer for now
    argUnis <- zipWithM unify argFTps argTps
    mapM_ (flip (reconnect $ prop TCData . requester) $ Just app) argUnis
    return $ outUni : argUnis

makeFuncall :: (PassCtx(CallErrorT m), Monad m) => Ref Node node -> [Ref Node node] -> Ref Cluster clus -> CallErrorT m [Ref Node node]
makeFuncall app args funClus = do
    (cls, trans) <- dupCluster funClus $ show app
    fptr <- follow (prop Lambda) cls <?!> MalformedFunction
    withRef app $ (prop TCData . replacement ?~ cast cls)
    reconnect (prop TCData . redirect) app $ Just $ fptr ^. Function.out
    zipWithM (reconnect $ prop TCData . redirect) (unlayer <$> fptr ^. Function.args) (Just <$> args) -- FIXME[WD->MK] handle arg names. Using unlayer for now
    unifyTypes fptr app args

processNode :: (PassCtx(CallErrorT m), Monad m) => Ref Node node -> CallErrorT m [Ref Node node]
processNode ref = do
    node <- read ref
    caseTest (uncover node) $ do
        of' $ \(App f as) -> do
            funReplacement <- (follow (prop TCData . replacement . casted) =<< follow source f) <?!> UnresolvedFunction
            args <- mapM (follow source . unlayer) as
            makeFuncall ref args funReplacement
        of' $ \ANY -> throwError NotAFuncallNode

-----------------------------
-- === TypeCheckerPass === --
-----------------------------

data FunctionCallingPass = FunctionCallingPass deriving (Show, Eq)

instance ( PassCtx(CallErrorT m)
         , PassCtx(m)
         , MonadTypeCheck (ls :<: term) m
         ) => TypeCheckerPass FunctionCallingPass m where
    hasJobs _ = not . null . view TypeCheck.uncalledApps <$> TypeCheck.get

    runTCPass _ = do
        apps    <- view TypeCheck.uncalledApps <$> TypeCheck.get
        results <- mapM (runExceptT . processNode) apps
        let withRefs = zip apps results
            failures = fst <$> filter (isLeft . snd) withRefs
        TypeCheck.modify_ $ (TypeCheck.unresolvedUnis %~ (++ (concat $ rights results)))
                          . (TypeCheck.uncalledApps   .~ failures)
        if length failures == length apps
            then return Stuck
            else return Progressed
