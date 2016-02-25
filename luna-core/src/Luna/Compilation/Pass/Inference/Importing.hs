{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Compilation.Pass.Inference.Importing where

import Prelude.Luna

import Control.Monad.Error                          (throwError, ErrorT, runErrorT, Error)
import Data.Construction
import Data.Either                                  (rights)
import Data.Prop
import Data.Record
import Data.Maybe                                   (fromMaybe)
import Luna.Evaluation.Runtime                      (Static, Dynamic)
import Luna.Library.Symbol.Class                    (MonadSymbol, lookupFunction, lookupLambda, loadLambda)
import Luna.Syntax.AST.Decl.Function                (Function, FunctionPtr)
import Luna.Syntax.AST.Term                         hiding (source)
import Data.Graph.Builder                           as Graph hiding (run)
import Data.Graph.Backend.VectorGraph               as Graph
import Luna.Syntax.Model.Layer
import Luna.Syntax.Model.Network.Builder            (merge, dupCluster, replacement)
import Luna.Syntax.Model.Network.Builder.Node
import Luna.Syntax.Model.Network.Builder.Term.Class (runNetworkBuilderT, NetGraph, NetLayers, NetCluster)
import Luna.Syntax.Model.Network.Class              ()
import Luna.Syntax.Model.Network.Term
import Type.Inference

import qualified Data.Map as Map
import           Data.Map (Map)

import qualified Luna.Library.Symbol.QualPath     as QualPath
import qualified Luna.Syntax.AST.Decl.Function    as Function

import           Luna.Compilation.Stage.TypeCheck       (ProgressStatus (..), TypeCheckerPass, hasJobs, runTCPass)
import           Luna.Compilation.Stage.TypeCheck.Class (MonadTypeCheck)
import qualified Luna.Compilation.Stage.TypeCheck.Class as TypeCheck


#define PassCtx(m) ( term  ~ Draft Static                         \
                   , ls    ~ NetLayers a                          \
                   , edge  ~ Link (ls :<: term)                   \
                   , node  ~ (ls :<: term)                        \
                   , clus  ~ NetCluster a                         \
                   , graph ~ Hetero (VectorGraph n e c)           \
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
                   , MonadSymbol node clus graph (m)              \
                   , Referred Node n graph                        \
                   )


data ImportError = NotABindingNode | AmbiguousNodeType | SymbolNotFound deriving (Show, Eq)
instance Error ImportError

type ImportErrorT = ErrorT ImportError

getTypeName :: PassCtx(m) => Ref Node node -> ImportErrorT m String
getTypeName ref = do
    node  <- read ref
    tpRef <- follow source $ node # Type
    tp    <- read tpRef
    caseTest (uncover tp) $ do
        match $ \(Cons (Str n)) -> return n
        match $ \ANY -> throwError AmbiguousNodeType

getFunctionName :: PassCtx(m) => Ref Node node -> ImportErrorT m String
getFunctionName ref = do
    node <- read ref
    caseTest (uncover node) $ do
        match $ \(Acc (Str n) t) -> do
            tpName <- getTypeName =<< follow source t
            return $ tpName <> "." <> n
        match $ \(Var (Str n)) -> return n
        match $ \ANY -> throwError NotABindingNode

funLookup :: PassCtx(m) => String -> ImportErrorT m (Function node graph)
funLookup name = do
    f <- lookupFunction $ QualPath.mk name
    fromMaybe (throwError SymbolNotFound) (return <$> f)

unsafeTranslateFunctionPtr :: Map (Ref Node a) (Ref Node a) -> FunctionPtr a -> FunctionPtr a
unsafeTranslateFunctionPtr m fptr = fptr & over (Function.self . mapped) unsafeTranslate
                                         & over (Function.args . mapped) unsafeTranslate
                                         & over Function.out unsafeTranslate
    where unsafeTranslate i = fromJust $ Map.lookup i m

importFunction :: PassCtx(m) => String -> Function node graph -> ImportErrorT m (Ref Cluster clus)
importFunction name fun = do
    translations <- merge $ fun ^. Function.graph
    cls <- subgraph
    mapM (flip include cls) $ Map.elems $ translations
    let fptr = fun ^. Function.fptr & unsafeTranslateFunctionPtr translations
    withRef cls $ (prop Lambda  ?~ fptr)
                . (prop Name    .~ name)
    loadLambda (QualPath.mk name) cls
    return cls

attachTypeRepr :: PassCtx(ImportErrorT m) => FunctionPtr node -> Ref Node node -> ImportErrorT m (Ref Node node)
attachTypeRepr fptr ref = do
    argTypes <- mapM (follow (prop Type) >=> follow source) $ fptr ^. Function.args
    outType  <- follow (prop Type) (fptr ^. Function.out) >>= follow source
    tp <- lam (arg <$> argTypes) outType
    currentTp <- follow (prop Type) ref >>= follow source
    uni <- unify tp currentTp
    reconnect ref (prop Type) uni
    return uni

processNode :: (PassCtx(ImportErrorT m), PassCtx(m)) => Ref Node node -> m (Either ImportError (Ref Node node))
processNode ref = runErrorT $ do
    name   <- getFunctionName ref
    lambda <- lookupLambda $ QualPath.mk name
    lamb <- case lambda of
        Just l  -> return l
        Nothing -> do
            fun <- funLookup name
            importFunction name fun
    (localLamb, transes) <- dupCluster lamb $ name <> " @ " <> (show ref)
    fptr <- fromJust . view (prop Lambda) <$> read lamb
    let transPtr = unsafeTranslateFunctionPtr transes fptr
    withRef localLamb $ (prop Name   .~ show ref)
                      . (prop Lambda ?~ transPtr)
    withRef ref $ prop TCData . replacement ?~ cast localLamb
    attachTypeRepr transPtr ref

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
            newUnis      = rights results

        TypeCheck.modify_ $ (TypeCheck.unresolvedSymbols .~ retries)
                          . (TypeCheck.unresolvedUnis    %~ (newUnis ++))

        if length retries == length syms
            then return Stuck
            else return Progressed
