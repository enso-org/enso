{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Compilation.Pass.Inference.Importing where

import Prelude.Luna

import Control.Monad.Error                          (throwError, ErrorT, runErrorT, Error)
import Data.Construction
import Data.Either                                  (rights)
import Old.Data.Prop
import Data.Record.Match
import Data.Maybe                                   (fromMaybe, maybeToList, isJust, isNothing)
import Data.List                                    (partition)
import Luna.Runtime.Dynamics                        (Static)
import Luna.Library.Symbol                          (MonadSymbol, lookupFunction, lookupLambda, loadLambda)
import Luna.Syntax.Term.Function                    (Function, Signature)
import Old.Luna.Syntax.Term.Class                   hiding (source)
import Data.Graph                                   as Graph
import Data.Graph.Builder                           as Graph hiding (run)
import qualified Data.Graph.Backend.NEC             as NEC
import Luna.Syntax.Model.Layer
import Luna.Syntax.Model.Network.Builder            (importToCluster, dupCluster, replacement, redirect, requester, tcErrors, translateSignature, NodeTranslator, originSign, Sign (..), isLambda)
import Luna.Syntax.Model.Network.Builder.Node
import Luna.Syntax.Model.Network.Builder.Term.Class (runNetworkBuilderT, NetGraph, NetLayers, NetCluster, NetClusterLayers)
import Luna.Syntax.Model.Network.Class              ()
import Luna.Syntax.Model.Network.Term
import Type.Inference

import qualified Data.Map as Map
import           Data.Map (Map)

import qualified Luna.Syntax.Name.Path     as QualPath
import qualified Luna.Syntax.Term.Function         as Function
import qualified Old.Luna.Syntax.Term.Expr.Lit         as Lit

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

buildTypeRep :: PassCtx(ImportErrorT m) => Signature (Ref Node node) -> Bool -> ImportErrorT m (Ref Node node)
buildTypeRep sig includeSelf = do
    let getType = (follow (prop Type) >=> follow source)
    selfType <- if includeSelf then mapM (fmap arg . getType) $ sig ^. Function.self else return Nothing
    argTypes <- (mapM . mapM) (follow (prop Type) >=> follow source) $ sig ^. Function.args

    outType  <- follow (prop Type) (sig ^. Function.out) >>= follow source
    lam (maybeToList selfType ++ argTypes) outType

attachTypeRep :: PassCtx(ImportErrorT m) => Signature (Ref Node node) -> Ref Node node -> ImportErrorT m (Ref Node node)
attachTypeRep sig ref = do
    currentTp <- follow (prop Type) ref >>= follow source
    isLam     <- follow (prop TCData . isLambda) ref
    importTp  <- buildTypeRep sig isLam
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

tryImport :: (PassCtx(ImportErrorT m), PassCtx(m), ReferencedM Cluster (Hetero (NEC.Graph n e c)) (ErrorT ImportError m) (NetClusterLayers :< RefSet Node (NetLayers :<: Draft Static)))
            => Ref Node node -> m (Either ImportError [Ref Node node])
tryImport ref = runErrorT $ do
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
    isLam <- follow (prop TCData . isLambda) ref
    if not isLam then
        zipWithM_ (reconnect $ prop TCData . redirect) (maybeToList $ fptr ^. Function.self) (Just <$> maybeToList selfMay)
        else return ()
    u1 <- attachTypeRep  fptr ref
    u2 <- attachSelfType fptr ref -- FIXME[MK]: monomorphic! copy self type and do unis in replacement only.
    let unis = u1 : u2
    mapM_ (flip include localLamb) unis
    return unis

getFirstLambdaArgument :: PassCtx(m) => Ref Node node -> m (Maybe (Ref Node node))
getFirstLambdaArgument ref = do
    n <- read ref
    caseTest (uncover n) $ do
        of' $ \(Lam as _) -> mapM (follow source . unlayer) $ tryHead as
        of' $ \ANY -> return Nothing

-----------------------------
-- === TypeCheckerPass === --
-----------------------------

--TODO[MK]: move to another pass, this does not _really_ belong here (it's not terrible, though)

typeLambdaAccessors :: (PassCtx(m), MonadTypeCheck (ls :<: term) m) => m Bool
typeLambdaAccessors = do
    tc <- TypeCheck.get
    let untypedLambdaAccs = tc ^. TypeCheck.untypedLambdaAccs
    resolutions <- forM untypedLambdaAccs $ \acc -> do
        n <- read acc
        caseTest (uncover n) $ do
            of' $ \(Acc n t) -> do
                tp    <- follow source =<< follow (prop Type) acc
                tgtTp <- follow source =<< follow (prop Type) =<< follow source t
                firstArgType <- getFirstLambdaArgument tp
                mapM (unify tgtTp) firstArgType
            of' $ \ANY -> impossible
    let newUnis = catMaybes resolutions
        withRes = zip untypedLambdaAccs resolutions
        (success, retry) = (both %~ fmap fst) $ partition (isJust . snd) withRes
    TypeCheck.modify_ $ (TypeCheck.unresolvedSymbols   %~ (success ++))
                      . (TypeCheck.unshiftedLambdaAccs %~ (success ++))
                      . (TypeCheck.untypedLambdaAccs   .~ retry)
                      . (TypeCheck.unresolvedUnis      %~ (newUnis ++))
    return . not . null $ success

shiftLambdaAccessors :: (PassCtx(m), MonadTypeCheck (ls :<: term) m) => m Bool
shiftLambdaAccessors = do
    tc <- TypeCheck.get
    let unshiftedLams = tc ^. TypeCheck.unshiftedLambdaAccs
    resolutions <- forM unshiftedLams $ \acc -> do
        repl :: Maybe (Ref Cluster NetCluster) <- fmap cast <$> follow (prop TCData . replacement) acc
        case repl of
            Just ref -> do
                lamb <- follow (prop Lambda) ref
                case lamb of
                    Just l -> do
                        let self = fmap arg . maybeToList $ l ^. Function.self
                        withRef ref $ prop Lambda ?~ (l & Function.self .~ Nothing & Function.args %~ (self ++))
                        return True
                    Nothing -> return False
            Nothing  -> return False
    let (success, retry) = (both %~ fmap fst) $ partition snd (zip unshiftedLams resolutions)
    TypeCheck.modify_ $ (TypeCheck.unshiftedLambdaAccs .~ retry)
    return . not . null $ success

data SymbolImportingPass = SymbolImportingPass deriving (Show, Eq)

instance ( PassCtx(ImportErrorT m)
         , PassCtx(m)
         , MonadTypeCheck (ls :<: term) m
         , ReferencedM Cluster (Hetero (NEC.Graph n e c)) (ErrorT ImportError m) (NetClusterLayers :< RefSet Node (NetLayers :<: Draft Static))
         ) => TypeCheckerPass SymbolImportingPass m where
    hasJobs _ = do
        tc <- TypeCheck.get
        let unresolvedSymbols = tc ^. TypeCheck.unresolvedSymbols
            untypedLambdaAccs = tc ^. TypeCheck.untypedLambdaAccs
            unshiftedLams     = tc ^. TypeCheck.unshiftedLambdaAccs
        return $ (not . null $ unresolvedSymbols) || (not . null $ untypedLambdaAccs) || (not . null $ unshiftedLams)

    runTCPass _ = do
        lambdaProgressed <- typeLambdaAccessors
        syms    <- view TypeCheck.unresolvedSymbols <$> TypeCheck.get
        results <- mapM tryImport syms
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

        shiftProgressed <- shiftLambdaAccessors

        if length retries < length syms || lambdaProgressed || shiftProgressed
            then return Progressed
            else return Stuck
