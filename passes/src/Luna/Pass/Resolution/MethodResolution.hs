{-# LANGUAGE OverloadedStrings #-}

module Luna.Pass.Resolution.MethodResolution where

import Luna.Prelude hiding (String, s, new, (<+>))
import OCI.Pass     hiding (compile)
import Data.Maybe   (isJust)

import OCI.IR.Combinators

import qualified Data.Set as Set
import qualified Data.Map as Map
import OCI.IR.Name           (Name)
import qualified Luna.IR.Expr as Term

import           Luna.Builtin.Data.Function as Function
import           Luna.Builtin.Data.Module   as Module
import           Luna.Builtin.Data.Module   (Imports)
import           Luna.Builtin.Data.Class    as Class
import Luna.IR                              hiding (Function, Import)
import OCI.IR.Class (Import)
import Luna.IR.Layer.Errors (ModuleTagged, ErrorSource)

import Luna.Pass.Resolution.FunctionResolution (ImportError(..))

import Luna.Pass.Data.ExprRoots
import Luna.Pass.Resolution.Data.CurrentTarget
import Luna.Pass.Resolution.Data.UnresolvedAccs
import Luna.Pass.Inference.Data.SimplifierQueue
import Luna.Pass.Inference.Data.MergeQueue
import Luna.Pass.Inference.Data.Unifications


data AccessorFunction
type instance Abstract   AccessorFunction = AccessorFunction
type instance Inputs     Net   AccessorFunction = '[AnyExpr, AnyExprLink]
type instance Inputs     Layer AccessorFunction = '[AnyExpr // Model, AnyExprLink // Model, AnyExpr // Type, AnyExpr // Succs, AnyExpr // Errors, AnyExpr // RequiredBy, AnyExpr // Requester]
type instance Inputs     Attr  AccessorFunction = '[UnresolvedAccs, SimplifierQueue, Unifications, Imports, MergeQueue, CurrentTarget, ExprRoots]
type instance Inputs     Event AccessorFunction = '[]

type instance Outputs    Net   AccessorFunction = '[AnyExpr, AnyExprLink]
type instance Outputs    Layer AccessorFunction = '[AnyExpr // Model, AnyExprLink // Model, AnyExpr // Succs, AnyExpr // Type, AnyExpr // Errors, AnyExpr // Requester]
type instance Outputs    Attr  AccessorFunction = '[UnresolvedAccs, SimplifierQueue, Unifications, MergeQueue]
type instance Outputs    Event AccessorFunction = '[New // AnyExpr, New // AnyExprLink, Import // AnyExpr, Import // AnyExprLink, Delete // AnyExpr, Delete // AnyExprLink, OnDeepDelete // AnyExpr]

type instance Preserves        AccessorFunction = '[]


data AccessorError = MethodNotFound Name Name
                   | AmbiguousType
    deriving (Eq, Show)

importErrorDoc :: Name -> Name -> Text
importErrorDoc n cl = "Can't find method " <> convert n <> " of " <> convert cl

runMethodResolution :: MonadPassManager m => SubPass AccessorFunction m Bool
runMethodResolution = do
    accs <- getAccs <$> getAttr @UnresolvedAccs
    res <- forM accs $ \acc -> do
        result <- importAccessor acc
        case result of
            True -> return $ Just acc
            _    -> return Nothing
    freshAccs <- getAccs <$> getAttr @UnresolvedAccs
    let initialAccsSet   = Set.fromList accs
        resolvedAccsSet  = Set.fromList $ catMaybes res
        newlyAdded       = filter (`Set.notMember` initialAccsSet)  freshAccs
        resolutionFailed = filter (`Set.notMember` resolvedAccsSet) freshAccs
    putAttr @UnresolvedAccs $ putAccs $ newlyAdded <> resolutionFailed
    return . not . Set.null $ resolvedAccsSet

destructMonad :: (MonadRef m, MonadIO m, MonadPassManager m) => Expr Draft -> SubPass AccessorFunction m (Expr Draft, Expr Draft)
destructMonad e = do
    Term (Term.Monadic t m) <- readTerm $ (unsafeGeneralize e :: Expr Monadic)
    (,) <$> source t <*> source m

importAccessor :: MonadPassManager m => Expr Monadic -> SubPass AccessorFunction m Bool
importAccessor tacc = do
    Term (Term.Monadic ac' _) <- readTerm tacc
    ac <- source ac'
    matchExpr ac $ \case
        Acc tv n -> do
            tv' <- source tv
            (tgtT, _) <- destructMonad tv'
            req        <- mapM source =<< getLayer @Requester tacc
            requiredBy <- getLayer @RequiredBy tacc
            matchExpr tgtT $ \case
                Cons cls _args -> importMethod cls n req requiredBy >>= \case
                    Left e      -> do
                        forM_ req $ \requester -> do
                            modifyLayer_ @Errors requester (e <>)
                        reconnectLayer' @Requester (Nothing :: Maybe (Expr Draft)) tacc
                        return True
                    Right root  -> do
                        tap <- app root tv'
                        replace tap ac
                        modifyAttr_ @SimplifierQueue $ wrapped %~ (generalize tacc :)
                        return True
                Lam{} -> do
                    forM_ req $ \requester -> do
                        modifyLayer_ @Errors requester (CompileError (importErrorDoc n "(->)") requiredBy [] :)
                    reconnectLayer' @Requester (Nothing :: Maybe (Expr Draft)) tacc
                    return True
                _ -> return False

importMethod :: MonadPassManager m => Name -> Name -> Maybe (Expr Draft) -> [ModuleTagged ErrorSource] -> SubPass AccessorFunction m (Either [CompileError] SomeExpr)
importMethod className methodName newReq requiredBy = do
    current <- fromCurrentTarget className methodName
    case current of
        Just a  -> return $ Right a
        Nothing -> importMethodFromImports className methodName newReq requiredBy

fromCurrentTarget :: MonadPassManager m => Name -> Name -> SubPass AccessorFunction m (Maybe SomeExpr)
fromCurrentTarget className methodName = do
    currentTgt <- getAttr @CurrentTarget
    root       <- (head . unwrap <$> getAttr @ExprRoots) >>= getLayer @Type >>= source
    return $ case currentTgt of
        TgtMethod _ c m -> if c == className && m == methodName then Just $ generalize root else Nothing
        _               -> Nothing

importMethodFromImports :: MonadPassManager m => Name -> Name -> Maybe (Expr Draft) -> [ModuleTagged ErrorSource] ->  SubPass AccessorFunction m (Either [CompileError] SomeExpr)
importMethodFromImports className methodName newReq requiredBy = do
    imports <- getAttr @Imports
    let  method = lookupClass methodName className imports requiredBy >>= lookupMethod methodName className requiredBy
    case method of
        Left err -> return $ Left err
        Right f  -> do
            (root, Assumptions unis merges apps accs) <- importFunction f
            mapM_ (reconnectLayer' @Requester newReq) merges
            mapM_ (reconnectLayer' @Requester newReq) unis
            mapM_ (reconnectLayer' @Requester newReq) apps
            mapM_ (reconnectLayer' @Requester newReq) accs
            modifyAttr_ @MergeQueue      $ wrapped %~ (merges ++)
            modifyAttr_ @Unifications    $ wrapped %~ (unis ++)
            modifyAttr_ @SimplifierQueue $ wrapped %~ (apps ++)
            modifyAttr_ @UnresolvedAccs  $ flip (foldr addAcc) accs
            return $ Right root

lookupClass :: Name -> Name -> Imports -> [ModuleTagged ErrorSource] -> Either [CompileError] Class
lookupClass n clsN imps reqBy = case imps ^? importedClasses . ix clsN . documentedItem of
    Nothing -> Left [CompileError (importErrorDoc n clsN) reqBy []]
    Just f  -> Right f

lookupMethod :: Name -> Name -> [ModuleTagged ErrorSource] -> Class -> Either [CompileError] Function
lookupMethod n clsN reqBy cls = case cls ^? methods . ix n . documentedItem of
    Just e  -> e
    _       -> Left [CompileError (importErrorDoc n clsN) reqBy []]
