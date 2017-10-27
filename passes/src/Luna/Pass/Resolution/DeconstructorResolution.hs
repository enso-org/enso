module Luna.Pass.Resolution.DeconstructorResolution where

import Luna.Prelude hiding (String, Destructor)
import Luna.Builtin.Data.Function as Function
import Luna.Builtin.Data.Class    as Class
import Luna.Builtin.Data.Module   as Module
import qualified Luna.IR.Expr     as Named
import Luna.IR
import OCI.IR.Combinators
import OCI.IR.Name                (Name)
import qualified OCI.IR.Class      as Event
import qualified Data.Map          as Map
import           Data.Map          (Map)
import           Data.Maybe        (isJust)
import           OCI.Pass        (Pass, SubPass, Inputs, Outputs, Preserves, Events)
import qualified OCI.Pass        as Pass
import           Luna.Pass.Resolution.Data.UnresolvedConses
import           Luna.Pass.Inference.Data.Unifications

data ImportError = SymbolNotFound
                 | SymbolAmbiguous [Name]

data DeconstructorResolution
type instance Abstract  DeconstructorResolution = DeconstructorResolution

type instance Inputs     Net   DeconstructorResolution = '[AnyExpr, AnyExprLink]
type instance Inputs     Layer DeconstructorResolution = '[AnyExpr // Model, AnyExpr // Type, AnyExpr // Succs, AnyExpr // UID, AnyExprLink // Model, AnyExprLink // UID]
type instance Inputs     Attr  DeconstructorResolution = '[NegativeConses, Imports, Unifications]
type instance Inputs     Event DeconstructorResolution = '[]

type instance Outputs    Net   DeconstructorResolution = '[AnyExpr, AnyExprLink]
type instance Outputs    Layer DeconstructorResolution = '[AnyExpr // Model, AnyExpr // Type, AnyExpr // Succs, AnyExpr // UID, AnyExprLink // Model, AnyExprLink // UID]
type instance Outputs    Attr  DeconstructorResolution = '[NegativeConses, Unifications]
type instance Outputs    Event DeconstructorResolution = '[New // AnyExpr, New // AnyExprLink, Event.Import // AnyExpr, Event.Import // AnyExprLink, Delete // AnyExpr, Delete // AnyExprLink, OnDeepDelete // AnyExpr]

type instance Preserves        DeconstructorResolution = '[]

runDeconstructorResolution :: (MonadRef m, MonadPassManager m) => Pass DeconstructorResolution m
runDeconstructorResolution = do
    conses <- unwrap <$> getAttr @NegativeConses
    mapM_ resolveDecons conses
    putAttr @NegativeConses $ NegativeConses []

lookupDestr :: Name -> Imports -> Either ImportError Destructor
lookupDestr n imps = case itoListOf (importedClasses .> itraversed <. Function.documentedItem . Class.constructors . ix n) imps of
    []       -> Left SymbolNotFound
    [(_, d)] -> Right $ snd d
    matches  -> Left . SymbolAmbiguous $ fst <$> matches

resolveDecons :: (MonadRef m, MonadPassManager m) => Expr Cons -> SubPass DeconstructorResolution m (Either ImportError ())
resolveDecons c = do
    Named.Term (Named.Cons n fields) <- readTerm c
    res <- lookupDestr n <$> getAttr @Imports
    case res of
        Left err   -> return $ Left err
        -- TODO: throw err when number of arguments does not match
        Right dest -> do
            (root, argTypes) <- importDestructor dest
            rootTp           <- getLayer @Type c >>= source
            rootUni          <- unify rootTp root
            argTps           <- mapM (source >=> getLayer @Type >=> source) fields
            argUnis          <- zipWithM unify argTps argTypes
            modifyAttr_ @Unifications $ wrap . ((unsafeRelayout rootUni : fmap unsafeRelayout argUnis) ++) . unwrap
            return $ Right ()
