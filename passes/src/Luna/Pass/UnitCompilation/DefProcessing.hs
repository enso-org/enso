{-# LANGUAGE OverloadedStrings #-}

module Luna.Pass.UnitCompilation.DefProcessing where

import Luna.Prelude hiding (String, s, new, Constructor, Destructor, cons)
import           OCI.Pass         (SubPass, Pass)
import qualified OCI.Pass         as Pass
import qualified OCI.Pass.Manager as Pass
import qualified Luna.IR.Expr     as Term
import OCI.IR.Combinators
import qualified OCI.IR.Class as Event
import Luna.Builtin.Data.Class    as Class
import Luna.Builtin.Data.Module
import Luna.Builtin.Data.Function
import Luna.Builtin.Data.LunaValue (LunaValue, LunaData (..))
import Luna.IR hiding (Function)
import qualified Control.Monad.State.Dependent as Dep
import Control.Monad.Trans.State
import Luna.Pass.Data.UniqueNameGen
import qualified Luna.Pass.UnitCompilation.RecordProcessing as RecordProcessing
import qualified Luna.IR.Layer.Errors as Errors

import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Luna.Pass.Evaluation.Interpreter
import Luna.Pass.Typechecking.Typecheck
import Luna.Pass.Inference.Data.Unifications    (Unifications(Unifications))
import Luna.Pass.Inference.Data.SimplifierQueue (SimplifierQueue(SimplifierQueue))
import Luna.Pass.Inference.Data.MergeQueue      (MergeQueue(MergeQueue))
import Luna.Pass.Resolution.Data.UnresolvedAccs (UnresolvedAccs, getAccs)
import Luna.Pass.Resolution.Data.CurrentTarget
import Data.TypeDesc


import Luna.Test.IR.Runner

data DefProcessing
type instance Abstract   DefProcessing = DefProcessing
type instance Pass.Inputs     Net   DefProcessing = '[AnyExpr, AnyExprLink]
type instance Pass.Inputs     Layer DefProcessing = '[AnyExpr // Model, AnyExpr // Succs, AnyExpr // Type, AnyExprLink // Model, AnyExpr // Errors, AnyExpr // RequiredBy]
type instance Pass.Inputs     Attr  DefProcessing = '[]
type instance Pass.Inputs     Event DefProcessing = '[]

type instance Pass.Outputs    Net   DefProcessing = '[AnyExpr, AnyExprLink]
type instance Pass.Outputs    Layer DefProcessing = '[AnyExpr // Model, AnyExpr // Type, AnyExprLink // Model, AnyExpr // Succs, AnyExpr // RequiredBy]
type instance Pass.Outputs    Attr  DefProcessing = '[]
type instance Pass.Outputs    Event DefProcessing = '[Event.Import // AnyExpr, Event.Import // AnyExprLink, New // AnyExpr, Delete // AnyExpr, Delete // AnyExprLink, New // AnyExprLink, OnDeepDelete // AnyExpr]

type instance Pass.Preserves        DefProcessing = '[]

processDef :: Name -> Imports -> Name -> Rooted SomeExpr -> IO (Either [CompileError] Function)
processDef modName imps functionName defIR@(Rooted _ root) = fmap (\(Right x) -> x) $ runPM False $ do
    runRegs
    defRoot <- Pass.eval' @DefProcessing $ ($ root) <$> importRooted defIR
    mkDef modName imps (TgtDef modName functionName) defRoot

mkDef :: (MonadPassManager m, MonadIO m) => Name -> Imports -> CurrentTarget -> SomeExpr -> m (Either [CompileError] Function)
mkDef modName imports currentTarget defRoot = mdo
    typecheckedRoot <- fromJust . Map.lookup (unsafeGeneralize defRoot) <$> typecheck currentTarget imports [unsafeGeneralize defRoot]
    value           <- Pass.eval' @Interpreter $ interpret' imports typecheckedRoot

    Just (unifies :: Unifications)    <- unsafeCoerce <$> unsafeGetAttr (getTypeDesc @Unifications)
    Just (merges  :: MergeQueue)      <- unsafeCoerce <$> unsafeGetAttr (getTypeDesc @MergeQueue)
    Just (apps    :: SimplifierQueue) <- unsafeCoerce <$> unsafeGetAttr (getTypeDesc @SimplifierQueue)
    Just (accs    :: UnresolvedAccs)  <- unsafeCoerce <$> unsafeGetAttr (getTypeDesc @UnresolvedAccs)

    errors <- Pass.eval' @DefProcessing $ getLayer @Errors typecheckedRoot
    let addArisingFrom = maybe id (:) $ case currentTarget of
            TgtDef    mod n   -> Just $ Errors.ModuleTagged mod $ Errors.FromFunction n
            TgtMethod mod c m -> Just $ Errors.ModuleTagged mod $ Errors.FromMethod c m
            _             -> Nothing
    case errors of
        e@(_:_) -> return $ Left $ e & traverse . Errors.arisingFrom %~ addArisingFrom
        _       -> do
            rooted <- Pass.eval' @DefProcessing $ do
                forM_ (unwrap unifies) $ flip (modifyLayer_ @RequiredBy) addArisingFrom
                forM_ (unwrap merges)  $ flip (modifyLayer_ @RequiredBy) addArisingFrom
                forM_ (unwrap apps)    $ flip (modifyLayer_ @RequiredBy) addArisingFrom
                forM_ (getAccs accs)   $ flip (modifyLayer_ @RequiredBy) addArisingFrom
                tp <- getLayer @Type (unsafeGeneralize typecheckedRoot :: SomeExpr) >>= source
                compile tp

            let localDef = case currentTarget of
                  TgtDef _ n -> Map.singleton n val
                  _          -> def
                val = evalScopeT value $ LocalScope def localDef

            return $ Right $ Function rooted val (Assumptions (unwrap unifies)
                                                              (unwrap merges)
                                                              (unwrap apps)
                                                              (getAccs accs))
