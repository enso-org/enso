{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE UndecidableInstances      #-}

module Luna.Pass.Inference.Struct where

import Luna.Prelude

import Data.Construction
import Old.Data.Prop
import Old.Data.Record
import Old.Data.Graph.Builder
import Old.Luna.Runtime.Dynamics                      (Static)
import Old.Luna.Syntax.Term.Class                         hiding (source)
import Old.Luna.Syntax.Model.Layer
import Old.Luna.Syntax.Model.Network.Builder.Node
import Old.Luna.Syntax.Model.Network.Builder            (requester)
import Old.Luna.Syntax.Model.Network.Builder.Layer      (TCDataPayload, originSign, Sign (..))
import Old.Luna.Syntax.Model.Network.Class              ()
import Old.Luna.Syntax.Model.Network.Term
import Luna.IR.Name.Ident.Pool                       (MonadIdentPool, newVarIdent')
import Old.Luna.Compilation.Stage.TypeCheck             (ProgressStatus (..), TypeCheckerPass, hasJobs, runTCPass)
import Old.Luna.Compilation.Stage.TypeCheck.Class       (MonadTypeCheck)

import qualified Old.Luna.Compilation.Stage.TypeCheck.Class as TypeCheck
import qualified Old.Data.Graph.Backend.NEC as NEC
import           Old.Data.Graph
import           Data.Layer_OLD.Cover_OLD


#define PassCtx(m,ls,term) ( term ~ Draft Static                                     \
                           , ne   ~ Link (ls :<: term)                               \
                           , nodeRef ~ Ref Node (ls :<: term)                        \
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
              || (not . null $ tcState ^. TypeCheck.untypedLambdas)

    runTCPass _ = do
        tcState <- TypeCheck.get
        let apps  = tcState ^. TypeCheck.untypedApps
            accs  = tcState ^. TypeCheck.untypedAccs
            binds = tcState ^. TypeCheck.untypedBinds
            lams  = tcState ^. TypeCheck.untypedLambdas
            all'   = tcState ^. TypeCheck.allNodes
        newUnis <- runPass all' apps accs binds lams
        TypeCheck.modify_ $ (TypeCheck.untypedApps    .~ [])
                          . (TypeCheck.untypedAccs    .~ [])
                          . (TypeCheck.untypedBinds   .~ [])
                          . (TypeCheck.untypedLambdas .~ [])
                          . (TypeCheck.unresolvedUnis %~ (newUnis ++))
        if (not $ null apps) || (not $ null accs) || (not $ null binds) || (not $ null lams)
            then return Progressed
            else return Stuck

---------------------------------
-- === Pass Implementation === --
---------------------------------

buildLambdaType :: PassCtx(m,ls,term) => nodeRef -> m [nodeRef]
buildLambdaType lamRef = do
    lamNode <- read lamRef
    caseTest (uncover lamNode) $ do
        of' $ \(Lam as r) -> do
            res      <- follow source r
            args     <- mapM (follow source . unlayer) as
            argTypes <- mapM getTypeSpec args
            outType  <- getTypeSpec res
            oldType  <- getTypeSpec lamRef
            newType  <- lam (arg <$> argTypes) outType
            uni      <- unify oldType newType
            reconnect (prop TCData . requester) uni (Just lamRef)
            return [uni]
        of' $ \ANY -> impossible

buildAppType :: PassCtx(m,ls,term) => nodeRef -> m [nodeRef]
buildAppType appRef = do
    appNode <- read appRef
    caseTest (uncover appNode) $ do
        of' $ \(App f as) -> do
            fun      <- follow source f
            args     <- mapM (follow source . unlayer) as
            argTypes <- mapM getTypeSpec args
            outType  <- getTypeSpec appRef

            funType  <- case as of
                [] -> return outType
                _  -> lam (arg <$> argTypes) outType

            oldFunType <- follow (prop Type) fun >>= follow source
            funUni     <- unify oldFunType funType
            withRef funUni $ prop TCData . originSign .~ Negative
            reconnect (prop TCData . requester) funUni (Just appRef)

            return [funUni]

        of' $ \ANY -> impossible


buildAccType :: PassCtx(m,ls,term) => nodeRef -> m [nodeRef]
buildAccType accRef = do
    appNode <- read accRef
    caseTest (uncover appNode) $ do
        of' $ \(Acc name srcConn) -> do
            src      <- follow source srcConn
            srcNode  <- read src
            srcType  <- follow (prop Type) src >>= follow source
            newType  <- acc name srcType
            oldType  <- follow (prop Type) accRef >>= follow source
            uniTp    <- unify oldType newType
            reconnect (prop Type) accRef uniTp
            reconnect (prop TCData . requester) newType (Just accRef)
            TypeCheck.modify_ $ TypeCheck.typelevelAccs %~ (newType :)
            return [uniTp]
        of' $ \ANY -> impossible

buildBindType :: PassCtx(m,ls,term) => nodeRef -> m nodeRef
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

runPass :: PassCtx(m,ls,term) => [nodeRef] -> [nodeRef] -> [nodeRef] -> [nodeRef] -> [nodeRef] -> m [nodeRef]
runPass all' apps accs binds lams = do
    mapM getTypeSpec all'
    accUnis  <- concat <$> mapM buildAccType accs
    appUnis  <- concat <$> mapM buildAppType apps
    lamUnis  <- concat <$> mapM buildLambdaType lams
    bindUnis <- mapM buildBindType binds
    return $ bindUnis <> accUnis <> appUnis <> lamUnis

universe :: forall r (tgt :: Knowledge *). Ptr r tgt
universe = Ptr 0
