{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Compilation.Pass.Inference.Importing where

import Prelude.Luna

import Control.Monad.Error                          (throwError, ErrorT, runErrorT, Error)
import Data.Construction
import Data.Either                                  (rights)
import Data.Prop
import Data.Record.Match
import Data.Maybe                                   (fromMaybe, maybeToList)
import Luna.Runtime.Dynamics                      (Static)
import Luna.Library.Symbol                    (MonadSymbol, lookupFunction, lookupLambda, loadLambda)
import Luna.Syntax.Term.Function                     (Function, Signature)
import Luna.Syntax.Term.Expr                         hiding (source)
import Data.Graph                                   as Graph
import Data.Graph.Builder                           as Graph hiding (run)
import qualified Data.Graph.Backend.NEC               as NEC
import Luna.Syntax.Model.Layer
import Luna.Syntax.Model.Network.Builder            (importToCluster, dupCluster, replacement, redirect, requester, tcErrors, translateSignature, NodeTranslator, originSign, Sign (..))
import Luna.Syntax.Model.Network.Builder.Node
import Luna.Syntax.Model.Network.Builder.Term.Class (runNetworkBuilderT, NetGraph, NetLayers, NetCluster, NetClusterLayers)
import Luna.Syntax.Model.Network.Class              ()
import Luna.Syntax.Model.Network.Term
import Type.Inference

import qualified Data.Map as Map
import           Data.Map (Map)

import qualified Luna.Syntax.Name.Path     as QualPath
import qualified Luna.Syntax.Term.Function         as Function
import qualified Luna.Syntax.Term.Lit         as Lit

import           Luna.Compilation.Stage.TypeCheck       (ProgressStatus (..), TypeCheckerPass, hasJobs, runTCPass)
import           Luna.Compilation.Stage.TypeCheck.Class (MonadTypeCheck)
import qualified Luna.Compilation.Stage.TypeCheck.Class as TypeCheck
import           Luna.Compilation.Error
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
                   , ReferencedM Node graph (m) node                 \
                   , ReferencedM Edge graph (m) edge                 \
                   , ReferencedM Node graph (ErrorT ImportError (m)) node {- FIXME[WD->MK]: czemu tu pojawia sie ErrorT? -} \
                   , ReferencedM Edge graph (ErrorT ImportError (m)) edge {- FIXME[WD->MK]: czemu tu pojawia sie ErrorT? -} \
                   )


data ImportError = NotABindingNode | AmbiguousNodeType | SymbolNotFound deriving (Show, Eq)
instance Error ImportError

type ImportErrorT = ErrorT ImportError

getSelf :: PassCtx(m) => Ref Node node -> m (Maybe $ Ref Node node)
getSelf ref = do
    node <- read ref
    caseTest (uncover node) $ do
        of' $ \(Acc _ t) -> Just <$> follow source t
        of' $ \ANY -> return Nothing

getTypeName :: PassCtx(m) => Ref Node node -> ImportErrorT m String
getTypeName ref = do
    node  <- read ref
    tpRef <- follow source $ node # Type
    tp    <- read tpRef
    caseTest (uncover tp) $ do
        of' $ \(Cons (Lit.String s) args) -> return s  -- TODO: handle args
        of' $ \ANY                        -> throwError AmbiguousNodeType

getFunctionName :: PassCtx(m) => Ref Node node -> ImportErrorT m String
getFunctionName ref = do
    node <- read ref
    caseTest (uncover node) $ do
        of' $ \(Acc name t) -> do
            tpName <- getTypeName =<< follow source t
            return $ tpName <> "." <> unwrap' name
        of' $ \(Var name) -> return $ unwrap' name
        of' $ \ANY -> throwError NotABindingNode

funLookup :: PassCtx(m) => String -> ImportErrorT m (Function (Ref Node node) graph)
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
    lam argTypes outType

attachTypeRep :: PassCtx(ImportErrorT m) => Signature (Ref Node node) -> Ref Node node -> ImportErrorT m (Ref Node node)
attachTypeRep sig ref = do
    currentTp <- follow (prop Type) ref >>= follow source
    importTp  <- buildTypeRep sig
    uni <- unify currentTp importTp
    reconnect (prop TCData . requester) uni $ Just ref
    return uni

attachSelfType :: PassCtx(ImportErrorT m) => Signature (Ref Node node) -> Ref Node node -> ImportErrorT m ([Ref Node node])
attachSelfType sig ref = do
    mySelf     <- maybeToList <$> getSelf ref
    let sigSelf = maybeToList $ sig ^. Function.self
    selfType    <- mapM (follow (prop Type) >=> follow source) mySelf
    sigSelfType <- mapM (follow (prop Type) >=> follow source) sigSelf
    uni         <- zipWithM unify selfType sigSelfType
    mapM (flip (reconnect $ prop TCData . requester) (Just ref)) uni
    mapM (flip withRef $ prop TCData . originSign .~ Negative) uni
    return uni

attachError :: PassCtx(m) => Ref Node node -> m ()
attachError ref = do
    node <- read ref
    err <- caseTest (uncover node) $ do
        of' $ \(Acc (Lit.String name) s) -> do
            n <- follow source s
            return [ImportError (Just n) name]
        of' $ \(Var (Lit.String n)) -> return [ImportError Nothing n]
        of' $ \ANY -> return []
    withRef ref $ prop TCData . tcErrors %~ (err ++)

processNode :: (PassCtx(ImportErrorT m), PassCtx(m), ReferencedM Cluster (Hetero (NEC.Graph n e c)) (ErrorT ImportError m) (NetClusterLayers :< RefSet Node (NetLayers :<: Draft Static)))
            => Ref Node node -> m (Either ImportError [Ref Node node])
processNode ref = runErrorT $ do
    name   <- getFunctionName ref
    lambda <- lookupLambda $ QualPath.mk name
    lamb <- case lambda of
        Just l  -> return l
        Nothing -> do
            fun <- funLookup name
            importFunction name fun
    (localLamb, trans) <- dupCluster lamb $ name <> " @ " <> (show ref)
    withRef ref $ prop TCData . replacement ?~ cast localLamb
    fptr <- fromJust <$> follow (prop Lambda) localLamb
    selfMay <- getSelf ref
    zipWithM (reconnect $ prop TCData . redirect) (maybeToList $ fptr ^. Function.self) (Just <$> maybeToList selfMay)
    u1 <- attachTypeRep  fptr ref
    u2 <- attachSelfType fptr ref -- FIXME[MK]: monomorphic! copy self type and do unis in replacement only.
    let unis = u1 : u2
    mapM_ (flip include localLamb) unis
    return unis


-----------------------------
-- === TypeCheckerPass === --
-----------------------------

data SymbolImportingPass = SymbolImportingPass deriving (Show, Eq)

instance ( PassCtx(ImportErrorT m)
         , PassCtx(m)
         , MonadTypeCheck (ls :<: term) m
         , ReferencedM Cluster (Hetero (NEC.Graph n e c)) (ErrorT ImportError m) (NetClusterLayers :< RefSet Node (NetLayers :<: Draft Static))
         ) => TypeCheckerPass SymbolImportingPass m where
    hasJobs _ = not . null . view TypeCheck.unresolvedSymbols <$> TypeCheck.get

    runTCPass _ = do
        syms    <- view TypeCheck.unresolvedSymbols <$> TypeCheck.get
        results <- mapM processNode syms
        let withResult   = zip syms results
            retry (_, Left r)  = r == AmbiguousNodeType
            retry (_, Right r) = False
            isError (_, Left  r) = r == SymbolNotFound
            isError (_, Right _) = False
            retries      = fst <$> filter retry   withResult
            errors       = fst <$> filter isError withResult
            newUnis      = concat $ rights results

        TypeCheck.modify_ $ (TypeCheck.unresolvedSymbols .~ retries)
                          . (TypeCheck.unresolvedUnis    %~ (newUnis ++))

        mapM_ attachError errors

        if length retries == length syms
            then return Stuck
            else return Progressed
