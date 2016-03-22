{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE UndecidableInstances      #-}

module Luna.Compilation.Pass.Inference.Struct where

import Prelude.Luna

import Data.Construction
import Data.Prop
import Data.Record
import Data.Graph.Builder
import Luna.Runtime.Dynamics                      (Static, Dynamic)
import Luna.Syntax.Term.Expr                         hiding (source)
import Luna.Syntax.Model.Layer
import Luna.Syntax.Model.Network.Builder.Node
import Luna.Syntax.Model.Network.Builder.Term.Class (runNetworkBuilderT, NetGraph, NetLayers)
import Luna.Syntax.Model.Network.Builder.Layer      (TCDataPayload, redirect)
import Luna.Syntax.Model.Network.Class              ()
import Luna.Syntax.Model.Network.Term
import Luna.Syntax.Name.Ident.Pool                       (MonadIdentPool, newVarIdent')
import Luna.Compilation.Stage.TypeCheck             (ProgressStatus (..), TypeCheckerPass, hasJobs, runTCPass)
import Luna.Compilation.Stage.TypeCheck.Class       (MonadTypeCheck)
import Type.Inference

import qualified Luna.Compilation.Stage.TypeCheck.Class as TypeCheck
import Data.Graph.Backend.VectorGraph


#define PassCtx(m,ls,term) ( term ~ Draft Static                                     \
                           , ne   ~ Link (ls :<: term)                               \
                           , Prop Type   (ls :<: term) ~ Ref Edge ne                 \
                           , Prop TCData (ls :<: term) ~ TCDataPayload (ls :<: term) \
                           , BiCastable     e ne                                     \
                           , BiCastable     n (ls :<: term)                          \
                           , MonadBuilder  (Hetero (VectorGraph n e c)) m            \
                           , HasProp Type     (ls :<: term)                          \
                           , HasProp TCData   (ls :<: term)                          \
                           , NodeInferable  m (ls :<: term)                          \
                           , TermNode Var   m (ls :<: term)                          \
                           , TermNode Lam   m (ls :<: term)                          \
                           , TermNode Unify m (ls :<: term)                          \
                           , TermNode Acc   m (ls :<: term)                          \
                           , MonadIdentPool m                                        \
                           , Destructor m (Ref Edge ne)                              \
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
              || (not . null $ tcState ^. TypeCheck.untypedBinds)

    runTCPass _ = do
        tcState <- TypeCheck.get
        let apps  = tcState ^. TypeCheck.untypedApps
            accs  = tcState ^. TypeCheck.untypedAccs
            binds = tcState ^. TypeCheck.untypedBinds
        newUnis <- runPass apps accs binds
        TypeCheck.put $ tcState & TypeCheck.untypedApps   .~ []
                                & TypeCheck.untypedAccs   .~ []
                                & TypeCheck.untypedBinds  .~ []
                                & TypeCheck.unresolvedUnis %~ (newUnis ++)
        if (not $ null apps) || (not $ null accs) || (not $ null binds)
            then return Progressed
            else return Stuck

---------------------------------
-- === Pass Implementation === --
---------------------------------

buildAppType :: (PassCtx(m,ls,term), nodeRef ~ Ref Node (ls :<: term)) => nodeRef -> m [nodeRef]
buildAppType appRef = do
    appNode <- read appRef
    caseTest (uncover appNode) $ do
        of' $ \(App srcConn argConns) -> do
            src      <- follow source srcConn
            args     <- mapM2 (follow source) argConns
            specArgs <- mapM2 getTypeSpec args
            out      <- var' =<< newVarIdent'
            l        <- lam' specArgs out

            src_v    <- read src
            let src_tc = src_v # Type
            src_t    <- follow source src_tc
            uniSrcTp <- unify src_t l
            reconnect (prop Type) src uniSrcTp

            app_v    <- read appRef
            let app_tc = app_v # Type
            app_t    <- follow source app_tc
            uniAppTp <- unify app_t out
            reconnect (prop Type) appRef uniAppTp

            return [uniSrcTp, uniAppTp]

        of' $ \ANY -> impossible


buildAccType :: (PassCtx(m,ls,term), nodeRef ~ Ref Node (ls :<: term)) => nodeRef -> m [nodeRef]
buildAccType accRef = do
    appNode <- read accRef
    caseTest (uncover appNode) $ do
        of' $ \(Acc name srcConn) -> do
            src      <- follow source srcConn
            srcTSpec <- getTypeSpec src
            newType  <- acc name srcTSpec
            acc_v    <- read accRef
            let acc_tc = acc_v # Type
            acc_t    <- follow source acc_tc
            uniTp    <- unify acc_t newType
            reconnect (prop Type) accRef uniTp
            return [uniTp]
        of' $ \ANY -> impossible

buildBindType :: (PassCtx(m,ls,term), nodeRef ~ Ref Node (ls :<: term)) => nodeRef -> m nodeRef
buildBindType ref = do
    node <- read ref
    caseTest (uncover node) $ do
        of' $ \(Match l r) -> do
            lr       <- follow source l
            rr       <- follow source r
            reconnect (prop TCData . redirect) lr (Just rr) -- FIXME[MK] : move the logic of creating redirect to a separate pass
            rtp      <- follow source =<< follow (prop Type) rr
            commonTp <- getTypeSpec lr
            uniTp    <- unify rtp commonTp
            reconnect (prop Type) lr uniTp
            reconnect (prop Type) rr uniTp
            return uniTp
        of' $ \ANY -> impossible


-- | Returns a concrete type of a node
--   If the type is just universe, create a new type variable
getTypeSpec :: PassCtx(m,ls,term) => Ref Node (ls :<: term) -> m (Ref Node (ls :<: term))
getTypeSpec ref = do
    val <- read ref
    tp  <- follow source $ val # Type
    if tp /= universe then return tp else do
        ntp <- var' =<< newVarIdent'
        reconnect (prop Type) ref ntp
        return ntp

runPass :: (PassCtx(m,ls,term), nodeRef ~ Ref Node (ls :<: term)) => [nodeRef] -> [nodeRef] -> [nodeRef] -> m [nodeRef]
runPass apps accs binds = do
    -- FIXME [MK]: Some of those still will be needed (definitely Acc typing) just in a form that does not introduce
    -- monomorphisms. Consider later.
    --appUnis  <- concat <$> mapM buildAppType apps -- FIXME [MK]: This is inherently monomorphic. Move this pass after importing and work on local copies of types... later.
    {-accUnis <- concat <$> mapM buildAccType accs-}
    bindUnis <- mapM buildBindType binds
    return $ bindUnis -- <> appUnis <> accUnis

universe = Ref 0
