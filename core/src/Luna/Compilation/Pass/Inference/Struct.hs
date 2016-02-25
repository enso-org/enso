{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE UndecidableInstances      #-}

module Luna.Compilation.Pass.Inference.Struct where

import Prelude.Luna

import Data.Construction
import Data.Prop
import Data.Record
import Data.Graph.Builder
import Luna.Evaluation.Runtime                      (Static, Dynamic)
import Luna.Syntax.AST.Term                         hiding (source)
import Luna.Syntax.Model.Layer
import Luna.Syntax.Model.Network.Builder.Node
import Luna.Syntax.Model.Network.Builder.Term.Class (runNetworkBuilderT, NetGraph, NetLayers)
import Luna.Syntax.Model.Network.Class              ()
import Luna.Syntax.Model.Network.Term
import Luna.Syntax.Name.Ident.Pool                  (MonadIdentPool, newVarIdent')
import Luna.Compilation.Stage.TypeCheck             (ProgressStatus (..), TypeCheckerPass, hasJobs, runTCPass)
import Luna.Compilation.Stage.TypeCheck.Class       (MonadTypeCheck)
import Type.Inference

import qualified Luna.Compilation.Stage.TypeCheck.Class as TypeCheck
import qualified Luna.Syntax.Name                       as Name
import Data.Graph.Backend.VectorGraph


#define PassCtx(m,ls,term) ( term ~ Draft Static                           \
                           , ne   ~ Link (ls :<: term)                     \
                           , Prop Type   (ls :<: term) ~ Ref Edge ne       \
                           , Prop Succs  (ls :<: term) ~ [Ref Edge ne]     \
                           , BiCastable     e ne                           \
                           , BiCastable     n (ls :<: term)                \
                           , MonadBuilder  (Hetero (VectorGraph n e c)) m  \
                           , HasProp Type     (ls :<: term)                \
                           , HasProp Succs    (ls :<: term)                \
                           , NodeInferable  m (ls :<: term)                \
                           , TermNode Var   m (ls :<: term)                \
                           , TermNode Lam   m (ls :<: term)                \
                           , TermNode Unify m (ls :<: term)                \
                           , TermNode Acc   m (ls :<: term)                \
                           , TermNode Sub   m (ls :<: term)                \
                           , MonadIdentPool m                              \
                           )

-----------------------------
-- === TypeCheckerPass === --
-----------------------------

data StructuralInferencePass = StructuralInferencePass deriving (Show, Eq)

instance ( PassCtx(m, ls, term)
         , MonadTypeCheck (ls :<: term) m
         ) => TypeCheckerPass StructuralInferencePass m where
    hasJobs _ = do
        tcState <- TypeCheck.get
        return $ (not . null $ tcState ^. TypeCheck.untypedApps)
              || (not . null $ tcState ^. TypeCheck.untypedAccs)

    runTCPass _ = do
        tcState <- TypeCheck.get
        let apps = tcState ^. TypeCheck.untypedApps
            accs = tcState ^. TypeCheck.untypedAccs
        (newUnis, newSubs) <- runPass apps accs
        TypeCheck.put $ tcState & TypeCheck.untypedApps   .~ []
                                & TypeCheck.untypedAccs   .~ []
                                & TypeCheck.unresolvedUnis %~ (newUnis ++)
                                & TypeCheck.unresolvedSubs %~ (newSubs ++)
        if (not $ null apps) || (not $ null accs)
            then return Progressed
            else return Stuck


---------------------------------
-- === Pass Implementation === --
---------------------------------

buildAppType :: (PassCtx(m,ls,term), nodeRef ~ Ref Node (ls :<: term)) => nodeRef -> m ([nodeRef], [nodeRef])
buildAppType appRef = do
    appNode <- read appRef
    caseTest (uncover appNode) $ do
        match $ \(App srcConn argConns) -> do
            src      <- follow source srcConn
            args     <- mapM2 (follow source) argConns
            specArgs <- mapM2 getOrSpecType args
            out      <- var' =<< newVarIdent'
            l        <- lam' specArgs out

            srcTSpec <- getOrSpecType src
            srcSub   <- sub srcTSpec l
            appTSpec <- getOrSpecType appRef
            uniAppTp <- unify appTSpec out

            return ([uniAppTp], [srcSub])

        match $ \ANY -> impossible


buildAccType :: (PassCtx(m,ls,term), nodeRef ~ Ref Node (ls :<: term)) => nodeRef -> m [nodeRef]
buildAccType accRef = do
    appNode <- read accRef
    caseTest (uncover appNode) $ do
        match $ \(Acc name srcConn) -> do
            src      <- follow source srcConn
            srcTSpec <- getOrSpecType src
            newType  <- acc name srcTSpec
            acc_v    <- read accRef
            let acc_tc = acc_v # Type
            acc_t    <- follow source acc_tc
            uniTp    <- unify acc_t newType
            reconnect accRef (prop Type) uniTp
            return [uniTp]
        match $ \ANY -> impossible


-- | Returns a concrete type of a node
--   If the type is just universe, create a new type variable
getOrSpecType :: PassCtx(m,ls,term) => Ref Node (ls :<: term) -> m (Ref Node (ls :<: term))
getOrSpecType ref = do
    val <- read ref
    tp  <- follow source $ val # Type
    if tp /= universe then return tp else do
        ntp <- var' =<< newVarIdent'
        reconnect ref (prop Type) ntp
        return ntp

runPass :: (PassCtx(m,ls,term), nodeRef ~ Ref Node (ls :<: term)) => [nodeRef] -> [nodeRef] -> m ([nodeRef], [nodeRef])
runPass apps accs = do
    (appUnis, appSubs) <- (\x -> (concat $ fmap fst x, concat $ fmap snd x)) <$> mapM buildAppType apps
    accUnis            <- concat <$> mapM buildAccType accs
    return $ (appUnis <> accUnis, appSubs) -- FIXME[WD]: use monadic element registration instead

universe = Ref 0 -- FIXME [WD]: Implement it in safe way. Maybe "star" should always result in the top one?


