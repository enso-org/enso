{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Pass.Inference.Importing where

import Prelude.Luna

import Control.Monad.Error                          (throwError, ErrorT, runErrorT, Error)
import Data.Either                                  (rights)
import Old.Data.Prop
import Data.Record.Match
import Data.Maybe                                   (fromMaybe)
import Old.Luna.Runtime.Dynamics                        (Static)
import Luna.IR.Library.Symbol                          (MonadSymbol, lookupFunction, lookupLambda, loadLambda)
import Luna.IR.Function                    (Function, Signature)
import Old.Luna.Syntax.Term.Class                   hiding (source)
import Data.Graph                                   as Graph hiding (add)
import Data.Graph.Builder                           as Graph hiding (run)
import qualified Data.Graph.Backend.NEC             as NEC
import Old.Luna.Syntax.Model.Layer
import Old.Luna.Syntax.Model.Network.Builder            (importToCluster, dupCluster, requester, tcErrors, translateSignature, originSign, Sign (..), replaceNode)
import Old.Luna.Syntax.Model.Network.Builder.Node
import Old.Luna.Syntax.Model.Network.Builder.Term.Class (NetLayers, NetCluster, NetClusterLayers)
import Old.Luna.Syntax.Model.Network.Class              ()
import Old.Luna.Syntax.Model.Network.Term

import qualified Luna.IR.Name.Path     as QualPath
import qualified Luna.IR.Function         as Function
import qualified Old.Luna.Syntax.Term.Expr.Lit         as Lit

import           Old.Luna.Compilation.Stage.TypeCheck       (ProgressStatus (..), TypeCheckerPass, hasJobs, runTCPass)
import           Old.Luna.Compilation.Stage.TypeCheck.Class (MonadTypeCheck)
import qualified Old.Luna.Compilation.Stage.TypeCheck.Class as TypeCheck
import           Old.Luna.Compilation.Error
import           Data.Layer_OLD.Cover_OLD

import Data.Graph.Model.Pointer.Set (RefSet)

-- FIXME[MK]: Do not explicitly type stuff here as NetGraph, solve the problems with typing it differently

#define PassCtx(m) ( term  ~ Draft Static                            \
                   , ls    ~ NetLayers                               \
                   , edge  ~ Link (ls :<: term)                      \
                   , node  ~ (ls :<: term)                           \
                   , clus  ~ NetCluster                              \
                   , graph ~ Hetero (NEC.Graph n e c)                \
                   , BiCastable     e edge                           \
                   , BiCastable     n node                           \
                   , BiCastable     c clus                           \
                   , MonadBuilder graph (m)                          \
                   , NodeInferable  (m) (ls :<: term)                \
                   , TermNode Var   (m) (ls :<: term)                \
                   , TermNode Acc   (m) (ls :<: term)                \
                   , TermNode Cons  (m) (ls :<: term)                \
                   , TermNode Lam   (m) (ls :<: term)                \
                   , TermNode Unify (m) (ls :<: term)                \
                   , MonadSymbol node clus graph (m)                 \
                   , Connectible (Ref Node node) (Ref Node node) (m) \
                   , Clusterable Node node clus (m)                  \
                   , Destructor (m) (Ref Edge edge)                  \
                   , Destructor (m) (Ref Node node)                  \
                   , ReferencedM Node graph (m) node                 \
                   , ReferencedM Edge graph (m) edge                 \
                   , ReferencedM Node graph (ErrorT ImportError (m)) node {- FIXME[WD->MK]: czemu tu pojawia sie ErrorT? -} \
                   , ReferencedM Edge graph (ErrorT ImportError (m)) edge {- FIXME[WD->MK]: czemu tu pojawia sie ErrorT? -} \
                   , Monad (m) \
                   )


data ImportError = NotABindingNode | AmbiguousNodeType | SymbolNotFound deriving (Show, Eq)
instance Error ImportError

type ImportErrorT = ErrorT ImportError

getTypeName :: PassCtx(m) => Ref Node node -> ImportErrorT m String
getTypeName ref = do
    tp    <- read ref
    caseTest (uncover tp) $ do
        of' $ \(Cons (Lit.String s) args) -> return s  -- TODO: handle args
        of' $ \ANY                        -> throwError AmbiguousNodeType

getAccFunctionName :: PassCtx(m) => Ref Node node -> ImportErrorT m String
getAccFunctionName ref = do
    node <- read ref
    caseTest (uncover node) $ do
        of' $ \(Acc (Lit.String n) s) -> do
            t <- follow source s >>= getTypeName
            return $ t <> "." <> n
        of' $ \ANY -> throwError NotABindingNode

getVarFunctionName :: PassCtx(m) => Ref Node node -> ImportErrorT m String
getVarFunctionName ref = do
    node <- read ref
    caseTest (uncover node) $ do
        of' $ \(Var name) -> return $ unwrap' name
        of' $ \ANY -> throwError NotABindingNode

funLookup :: (Monad m, PassCtx(ImportErrorT m)) => String -> ImportErrorT m (Function (Ref Node node) graph)
funLookup name = do
    f <- lookupFunction $ QualPath.mk name
    fromMaybe (throwError SymbolNotFound) (return <$> f)

importFunction :: (PassCtx(ImportErrorT m), ReferencedM Cluster (Hetero (NEC.Graph n e c)) (ErrorT ImportError m) (NetClusterLayers :< RefSet Node (NetLayers :<: Draft Static)))
               => String -> Function (Ref Node node) graph -> ImportErrorT m (Ref Cluster clus)
importFunction name fun = do
    (cls, translator) <- importToCluster $ fun ^. Function.body
    let sig = fun ^. Function.sig & translateSignature translator
    withRef cls $ (prop Lambda  ?~ sig)
                . (prop Name    .~ name)
    loadLambda (QualPath.mk name) cls
    return cls

buildTypeRep :: PassCtx(ImportErrorT m) => Signature (Ref Node node) -> ImportErrorT m (Ref Node node)
buildTypeRep sig = do
    argTypes <- (mapM . mapM) (follow (prop Type) >=> follow source) $ sig ^. Function.args
    outType  <- follow (prop Type) (sig ^. Function.out) >>= follow source
    case argTypes of
        [] -> return outType
        _  -> lam argTypes outType

attachVarTypeRep :: PassCtx(ImportErrorT m) => Signature (Ref Node node) -> Ref Node node -> ImportErrorT m (Ref Node node)
attachVarTypeRep sig ref = do
    currentTp <- follow (prop Type) ref >>= follow source
    importTp  <- buildTypeRep sig
    uni <- unify currentTp importTp
    reconnect (prop TCData . requester) uni $ Just ref
    return uni

attachAccTypeRep :: PassCtx(ImportErrorT m) => Signature (Ref Node node) -> Ref Node node -> ImportErrorT m ()
attachAccTypeRep sig ref = do
    importTp <- buildTypeRep sig
    replaceNode ref importTp

attachSelfType :: PassCtx(ImportErrorT m) => Signature (Ref Node node) -> Ref Node node -> ImportErrorT m (Ref Node node)
attachSelfType sig ref = do
    node       <- read ref
    mySelf     <- caseTest (uncover node) $ do
        of' $ \(Acc _ t) -> follow source t
        of' $ \ANY -> impossible
    let sigSelf = fromJust $ sig ^. Function.self
    sigSelfType <- follow (prop Type) sigSelf >>= follow source
    uni         <- unify mySelf sigSelfType
    reconnect (prop TCData . requester) uni (Just ref)
    withRef uni $ prop TCData . originSign .~ Negative
    return uni

attachError :: PassCtx(m) => Ref Node node -> m ()
attachError ref = do
    node <- read ref
    caseTest (uncover node) $ do
        of' $ \(Acc (Lit.String name) s) -> do
            req <- follow (prop TCData . requester) ref
            case req of
                Nothing -> return ()
                Just e  -> do
                    r <- follow source e
                    n <- read r
                    caseTest (uncover n) $ do
                        of' $ \(Acc _ t) -> do
                            tgt <- follow source t
                            withRef r $ prop TCData . tcErrors %~ ((ImportError (Just tgt) name) :)
                        of' $ \ANY -> return ()
        of' $ \(Var (Lit.String n)) -> withRef ref $ prop TCData . tcErrors %~ ((ImportError Nothing n) :)
        of' $ \ANY -> return ()

tryImportSymbol :: (Monad m, PassCtx(ImportErrorT m), ReferencedM Cluster (Hetero (NEC.Graph n e c)) (ErrorT ImportError m) (NetClusterLayers :< RefSet Node (NetLayers :<: Draft Static)))
            => String -> Ref Node node -> ImportErrorT m (Ref Cluster clus)
tryImportSymbol name ref = do
    lambda <- lookupLambda $ QualPath.mk name
    lamb <- case lambda of
        Just l  -> return l
        Nothing -> do
            fun <- funLookup name
            importFunction name fun
    (localLamb, _) <- dupCluster lamb $ name <> " @ " <> (show ref)
    return localLamb

tryImportAcc :: (PassCtx(m), PassCtx(ImportErrorT m), ReferencedM Cluster (Hetero (NEC.Graph n e c)) (ErrorT ImportError m) (NetClusterLayers :< RefSet Node (NetLayers :<: Draft Static)))
            => Ref Node node -> m (Either ImportError (Ref Node node))
tryImportAcc ref = runErrorT $ do
    name      <- getAccFunctionName ref
    localLamb <- tryImportSymbol name ref
    fptr <- fromJust <$> follow (prop Lambda) localLamb
    u    <- attachSelfType   fptr ref
    attachAccTypeRep fptr ref
    return u

tryImportVar :: (PassCtx(m), PassCtx(ImportErrorT m), ReferencedM Cluster (Hetero (NEC.Graph n e c)) (ErrorT ImportError m) (NetClusterLayers :< RefSet Node (NetLayers :<: Draft Static)))
            => Ref Node node -> m (Either ImportError (Ref Node node))
tryImportVar ref = runErrorT $ do
    name      <- getVarFunctionName ref
    localLamb <- tryImportSymbol name ref
    fptr <- fromJust <$> follow (prop Lambda) localLamb
    u    <- attachVarTypeRep  fptr ref
    return u

-----------------------------
-- === TypeCheckerPass === --
-----------------------------

data SymbolImportingPass = SymbolImportingPass deriving (Show, Eq)

instance ( PassCtx(ImportErrorT m)
         , PassCtx(m)
         , MonadTypeCheck (ls :<: term) m
         , ReferencedM Cluster (Hetero (NEC.Graph n e c)) (ErrorT ImportError m) (NetClusterLayers :< RefSet Node (NetLayers :<: Draft Static))
         ) => TypeCheckerPass SymbolImportingPass m where
    hasJobs _ = do
        tc <- TypeCheck.get
        let unresolvedSymbols = tc ^. TypeCheck.unresolvedSymbols
            typelevelAccs     = tc ^. TypeCheck.typelevelAccs
        return $ (not . null $ unresolvedSymbols) || (not . null $ typelevelAccs)

    runTCPass _ = do
        vars       <- view TypeCheck.unresolvedSymbols <$> TypeCheck.get
        accs       <- view TypeCheck.typelevelAccs     <$> TypeCheck.get
        varResults <- mapM tryImportVar vars
        accResults <- mapM tryImportAcc accs

        let interpretResults syms results = (retries, newUnis, errors) where
                withResult   = zip syms results
                retry (_, Left r)    = r == AmbiguousNodeType
                retry (_, Right r)   = False
                isError (_, Left  r) = r == SymbolNotFound
                isError (_, Right _) = False
                retries      = fst <$> filter retry   withResult
                errors       = fst <$> filter isError withResult
                newUnis      = rights results

        let (varRetries, varUnis, varErrors) = interpretResults vars varResults
            (accRetries, accUnis, accErrors) = interpretResults accs accResults

        TypeCheck.modify_ $ (TypeCheck.unresolvedSymbols .~ varRetries)
                          . (TypeCheck.typelevelAccs     .~ accRetries)
                          . (TypeCheck.unresolvedUnis    %~ ((varUnis ++ accUnis) ++))

        mapM_ attachError (varErrors ++ accErrors)

        if length accRetries < length accs || length varRetries < length vars
            then return Progressed
            else return Stuck
