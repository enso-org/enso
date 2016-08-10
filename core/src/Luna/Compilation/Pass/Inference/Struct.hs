{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE UndecidableInstances      #-}

module Luna.Compilation.Pass.Inference.Struct where

import Prelude.Luna

import Data.Construction
import Data.Prop
import Data.Record
import Data.Graph.Builder
import Data.Maybe                                 (catMaybes)
import Luna.Runtime.Dynamics                      (Static, Dynamic)
import Old.Luna.Syntax.Term.Class                         hiding (source)
import Luna.Syntax.Model.Layer
import Luna.Syntax.Model.Network.Builder.Node
import Luna.Syntax.Model.Network.Builder            (requester)
import Luna.Syntax.Model.Network.Builder.Term.Class (runNetworkBuilderT, NetGraph, NetLayers)
import Luna.Syntax.Model.Network.Builder.Layer      (TCDataPayload, redirect)
import Luna.Syntax.Model.Network.Class              ()
import Luna.Syntax.Model.Network.Term
import Luna.Syntax.Name.Ident.Pool                       (MonadIdentPool, newVarIdent')
import Luna.Compilation.Stage.TypeCheck             (ProgressStatus (..), TypeCheckerPass, hasJobs, runTCPass)
import Luna.Compilation.Stage.TypeCheck.Class       (MonadTypeCheck)
import Type.Inference

import qualified Luna.Compilation.Stage.TypeCheck.Class as TypeCheck
import qualified Data.Graph.Backend.NEC as NEC
import           Data.Graph
import           Data.Layer_OLD.Cover_OLD


#define PassCtx(m,ls,term) ( term ~ Draft Static                                     \
                           , ne   ~ Link (ls :<: term)                               \
                           , Prop Type   (ls :<: term) ~ Ref Edge ne                 \
                           , Prop TCData (ls :<: term) ~ TCDataPayload (ls :<: term) \
                           , BiCastable     e ne                                     \
                           , BiCastable     n (ls :<: term)                          \
                           , MonadBuilder  (Hetero (NEC.Graph n e c)) m              \
                           , HasProp Type     (ls :<: term)                          \
                           , HasProp TCData   (ls :<: term)                          \
                           , NodeInferable  m (ls :<: term)                          \
                           , TermNode Var   m (ls :<: term)                          \
                           , TermNode Lam   m (ls :<: term)                          \
                           , TermNode Unify m (ls :<: term)                          \
                           , TermNode Acc   m (ls :<: term)                          \
                           , MonadTypeCheck (ls :<: term) m                          \
                           , MonadIdentPool m                                        \
                           , Destructor m (Ref Edge ne)                              \
                           , ReferencedM Node (Hetero (NEC.Graph n e c)) m (ls :<: term) \
                           , ReferencedM Edge (Hetero (NEC.Graph n e c)) m ne \
                           )

-----------------------------
-- === TypeCheckerPass === --
-----------------------------

data StructuralInferencePass = StructuralInferencePass deriving (Show, Eq)

instance (PassCtx(m, ls, term)) => TypeCheckerPass StructuralInferencePass m where
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
            all   = tcState ^. TypeCheck.allNodes
        newUnis <- runPass all apps accs binds
        TypeCheck.modify_ $ (TypeCheck.untypedApps   .~ [])
                          . (TypeCheck.untypedAccs   .~ [])
                          . (TypeCheck.untypedBinds  .~ [])
                          . (TypeCheck.unresolvedUnis %~ (newUnis ++))
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
        of' $ \(App f as) -> do
            fun      <- follow source f
            args     <- mapM (follow source . unlayer) as
            argTypes <- mapM getTypeSpec args
            outType  <- getTypeSpec appRef

            curriedArgs <- fmap catMaybes $ forM (zip args argTypes) $ \(arg, typ) -> do
                node <- read arg
                return $ caseTest (uncover node) $ do
                    of' $ \Blank -> Just typ
                    of' $ \ANY   -> Nothing

            (unis, newOutType) <- if (not . null $ curriedArgs)
                then do
                    newOutTpe <- var' =<< newVarIdent'
                    newType   <- lam (arg <$> curriedArgs) newOutTpe
                    uni       <- unify outType newType
                    return ([uni], newOutTpe)
                else return ([], outType)

            funType  <- case as of
                [] -> return newOutType
                _  -> lam (arg <$> argTypes) newOutType

            oldFunType <- follow (prop Type) fun >>= follow source
            funUni     <- unify oldFunType funType

            return $ funUni : unis

        of' $ \ANY -> impossible


buildAccType :: (PassCtx(m,ls,term), nodeRef ~ Ref Node (ls :<: term)) => nodeRef -> m [nodeRef]
buildAccType accRef = do
    appNode <- read accRef
    caseTest (uncover appNode) $ do
        of' $ \(Acc name srcConn) -> do
            src      <- follow source srcConn
            srcNode  <- read src
            srcType  <- follow (prop Type) src >>= follow source
            accType  <- acc name srcType
            newType  <- caseTest (uncover srcNode) $ do
                of' $ \Blank -> lam [arg srcType] accType
                of' $ \ANY   -> return accType
            oldType  <- follow (prop Type) accRef >>= follow source
            uniTp    <- unify oldType newType
            reconnect (prop Type) accRef uniTp
            reconnect (prop TCData . requester) accType (Just accRef)
            TypeCheck.modify_ $ TypeCheck.typelevelAccs %~ (accType :)
            return [uniTp]
        of' $ \ANY -> impossible

buildBindType :: (PassCtx(m,ls,term), nodeRef ~ Ref Node (ls :<: term)) => nodeRef -> m nodeRef
buildBindType ref = do
    node <- read ref
    caseTest (uncover node) $ do
        of' $ \(Match l r) -> do
            lr       <- follow source l
            rr       <- follow source r
            rtp      <- follow source =<< follow (prop Type) rr
            commonTp <- getTypeSpec lr
            uniTp    <- unify rtp commonTp
            reconnect (prop Type) lr commonTp
            reconnect (prop Type) rr commonTp
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

runPass :: (PassCtx(m,ls,term), nodeRef ~ Ref Node (ls :<: term)) => [nodeRef] -> [nodeRef] -> [nodeRef] -> [nodeRef] -> m [nodeRef]
runPass all apps accs binds = do
    mapM getTypeSpec all
    accUnis  <- concat <$> mapM buildAccType accs
    appUnis  <- concat <$> mapM buildAppType apps
    bindUnis <- mapM buildBindType binds
    return $ bindUnis <> accUnis <> appUnis

universe = Ptr 0
