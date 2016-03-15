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
import Luna.Evaluation.Runtime                      (Static)
import Luna.Library.Symbol.Class                    (MonadSymbol, lookupFunction, lookupLambda, loadLambda)
import Luna.Syntax.AST.Function                     (Function, Signature)
import Luna.Syntax.AST.Term                         hiding (source)
import Data.Graph.Builder                           as Graph hiding (run)
import Data.Graph.Backend.VectorGraph               as Graph
import Luna.Syntax.Model.Layer
import Luna.Syntax.Model.Network.Builder            (importToCluster, dupCluster, replacement, redirect, translateSignature, NodeTranslator)
import Luna.Syntax.Model.Network.Builder.Node
import Luna.Syntax.Model.Network.Builder.Term.Class (runNetworkBuilderT, NetGraph, NetLayers, NetCluster)
import Luna.Syntax.Model.Network.Class              ()
import Luna.Syntax.Model.Network.Term
import Type.Inference

import qualified Data.Map as Map
import           Data.Map (Map)

import qualified Luna.Library.Symbol.QualPath     as QualPath
import qualified Luna.Syntax.AST.Function         as Function
import qualified Luna.Syntax.AST.Term.Lit         as Lit

import           Luna.Compilation.Stage.TypeCheck       (ProgressStatus (..), TypeCheckerPass, hasJobs, runTCPass)
import           Luna.Compilation.Stage.TypeCheck.Class (MonadTypeCheck)
import qualified Luna.Compilation.Stage.TypeCheck.Class as TypeCheck

-- FIXME[MK]: Do not explicitly type stuff here as NetGraph, solve the problems with typing it differently

#define PassCtx(m) ( term  ~ Draft Static                            \
                   , ls    ~ NetLayers                               \
                   , edge  ~ Link (ls :<: term)                      \
                   , node  ~ (ls :<: term)                           \
                   , clus  ~ NetCluster                              \
                   , graph ~ Hetero (VectorGraph n e c)              \
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
                   , Referred Node graph n                           \
                   , Connectible (Ref Node node) (Ref Node node) (m) \
                   , Clusterable node clus (m)                       \
                   , Destructor (m) (Ref Edge edge)                  \
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

importFunction :: PassCtx(ImportErrorT m) => String -> Function (Ref Node node) graph -> ImportErrorT m (Ref Cluster clus)
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
    reconnect (prop Type) ref uni
    return uni

attachSelfType :: PassCtx(ImportErrorT m) => Signature (Ref Node node) -> Ref Node node -> ImportErrorT m ([Ref Node node])
attachSelfType sig ref = do
    mySelf  <- maybeToList <$> getSelf ref
    let sigSelf = maybeToList $ sig ^. Function.self
    selfType <- mapM (follow (prop Type) >=> follow source) mySelf
    sigSelfType <- mapM (follow (prop Type) >=> follow source) sigSelf
    uni <- zipWithM unify selfType sigSelfType
    zipWithM (reconnect $ prop Type) mySelf  uni
    zipWithM (reconnect $ prop Type) sigSelf uni
    return uni


processNode :: (PassCtx(ImportErrorT m), PassCtx(m)) => Ref Node node -> m (Either ImportError [Ref Node node])
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
    zipWithM (reconnect $ prop TCData . redirect) (maybeToList $ fptr ^. Function.self) (maybeToList selfMay)
    u1 <- attachTypeRep  fptr ref
    u2 <- attachSelfType fptr ref -- TODO[MK]: monomorphic! copy self type and do unis in replacement only.
    return $ u1 : u2


-----------------------------
-- === TypeCheckerPass === --
-----------------------------

data SymbolImportingPass = SymbolImportingPass deriving (Show, Eq)

instance ( PassCtx(ImportErrorT m)
         , PassCtx(m)
         , MonadTypeCheck (ls :<: term) m
         ) => TypeCheckerPass SymbolImportingPass m where
    hasJobs _ = not . null . view TypeCheck.unresolvedSymbols <$> TypeCheck.get

    runTCPass _ = do
        syms    <- view TypeCheck.unresolvedSymbols <$> TypeCheck.get
        results <- mapM processNode syms
        let withResult   = zip syms results
            retry (_, Left r)  = r == AmbiguousNodeType
            retry (_, Right r) = False
            retries      = fst <$> filter retry withResult
            newUnis      = concat $ rights results

        TypeCheck.modify_ $ (TypeCheck.unresolvedSymbols .~ retries)
                          . (TypeCheck.unresolvedUnis    %~ (newUnis ++))

        if length retries == length syms
            then return Stuck
            else return Progressed
