{-# LANGUAGE OverloadedStrings #-}

module Luna.Pass.Resolution.FunctionResolution where

import Luna.Prelude hiding (String, (<+>))
import Luna.Builtin.Data.Function as Function
import Luna.Builtin.Data.Module   as Module
import Luna.IR                    hiding (Function)
import OCI.IR.Combinators
import OCI.IR.Name                (Name)
import qualified OCI.IR.Class      as Event
import qualified Data.Map          as Map
import           Data.Map          (Map)
import           Data.Maybe        (isJust)
import           OCI.Pass        (Pass, SubPass, Inputs, Outputs, Preserves, Events)
import qualified OCI.Pass        as Pass
import           Luna.Pass.Resolution.Data.UnresolvedVars
import           Luna.Pass.Resolution.Data.UnresolvedAccs
import           Luna.Pass.Inference.Data.Unifications
import           Luna.Pass.Inference.Data.SimplifierQueue
import           Luna.Pass.Inference.Data.MergeQueue
import           Luna.Pass.Resolution.Data.CurrentTarget
import           Luna.Pass.Data.ExprRoots


-- === Errors === --

data ImportError = SymbolNotFound
                 | SymbolAmbiguous [Name]
                 deriving (Show, Eq)


-- === Pass === --

data FunctionResolution
type instance Abstract  FunctionResolution = FunctionResolution

type instance Inputs     Net   FunctionResolution = '[AnyExpr, AnyExprLink]
type instance Inputs     Layer FunctionResolution = '[AnyExpr // Model, AnyExpr // Type, AnyExpr // Succs, AnyExpr // UID, AnyExpr // Errors, AnyExpr // Requester, AnyExprLink // Model, AnyExprLink // UID]
type instance Inputs     Attr  FunctionResolution = '[UnresolvedVars, Imports, UnresolvedAccs, Unifications, SimplifierQueue, MergeQueue, CurrentTarget, ExprRoots]
type instance Inputs     Event FunctionResolution = '[]

type instance Outputs    Net   FunctionResolution = '[AnyExpr, AnyExprLink]
type instance Outputs    Layer FunctionResolution = '[AnyExpr // Model, AnyExpr // Type, AnyExpr // Succs, AnyExpr // UID, AnyExpr // Errors, AnyExpr // Requester, AnyExprLink // Model, AnyExprLink // UID]
type instance Outputs    Attr  FunctionResolution = '[UnresolvedVars, UnresolvedAccs, Unifications, SimplifierQueue, MergeQueue]
type instance Outputs    Event FunctionResolution = '[New // AnyExpr, New // AnyExprLink, Event.Import // AnyExpr, Event.Import // AnyExprLink, Delete // AnyExpr, Delete // AnyExprLink, OnDeepDelete // AnyExpr]

type instance Preserves        FunctionResolution = '[]

runFunctionResolution :: (MonadRef m, MonadPassManager m) => Pass FunctionResolution m
runFunctionResolution = do
    vars <- unwrap <$> getAttr @UnresolvedVars
    mapM_ importVar vars
    putAttr @UnresolvedVars $ UnresolvedVars []

lookupSym :: Name -> Imports -> Either ImportError Function
lookupSym n imps = case imps ^. importedFunctions . at n of
    Nothing -> Left  SymbolNotFound
    Just f  -> Right f

resolveSymbol :: (MonadRef m, MonadPassManager m) => Name -> Expr Var -> SubPass FunctionResolution m (Either ImportError SomeExpr)
resolveSymbol name var = do
    current <- getAttr @CurrentTarget
    if current == TgtDef name
        then do
            root <- head . unwrap <$> getAttr @ExprRoots
            fmap (Right . generalize) $ getLayer @Type root >>= source
        else do
            fun <- lookupSym name <$> getAttr @Imports
            forM fun $ \f -> do
                (root, Assumptions unis merges apps accs) <- importFunction f
                mapM_ (reconnectLayer' @Requester $ Just var) unis
                mapM_ (reconnectLayer' @Requester $ Just var) merges
                mapM_ (reconnectLayer' @Requester $ Just var) apps
                mapM_ (reconnectLayer' @Requester $ Just var) accs
                modifyAttr_ @MergeQueue      $ wrapped %~ (merges ++)
                modifyAttr_ @Unifications    $ wrapped %~ (unis ++)
                modifyAttr_ @SimplifierQueue $ wrapped %~ (apps ++)
                modifyAttr_ @UnresolvedAccs  $ flip (foldr addAcc) accs
                return root


importErrorDoc :: Name -> ImportError -> Text
importErrorDoc n SymbolNotFound         = "Can't find function: " <> " " <> fromString (show n)
importErrorDoc n (SymbolAmbiguous mods) = "Function" <> " " <> fromString (show n) <> " " <> "is ambiguous." <> "\n" <> "It's exported by the following modules:" <> " " <> (foldl (\l r -> l <> "\n" <> r) "" $ fromString . show <$> mods)

importVar :: (MonadRef m, MonadPassManager m) => Expr Var -> SubPass FunctionResolution m ()
importVar var = do
    sym  <- view name <$> readTerm var
    r    <- resolveSymbol sym var
    case r of
        Left  err  -> modifyLayer_ @Errors var (importErrorDoc sym err :)
        Right root -> do
            tp  <- getLayer @Type   var  >>= source
            reconnectLayer @Type root (generalize var :: SomeExpr)
            uni <- unify root tp
            reconnectLayer' @Requester (Just var)  (unsafeGeneralize uni :: Expr Draft)
            modifyAttr_ @Unifications $ wrapped %~ (unsafeGeneralize uni :)
