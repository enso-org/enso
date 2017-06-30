module Luna.Pass.Typechecking.Typecheck where

import Luna.Prelude
import Data.TypeDesc

import Luna.Pass.Data.ExprRoots
import Luna.Pass.Data.UniqueNameGen
import Luna.Pass.Data.ExprMapping
import Luna.Pass.Resolution.Data.UnresolvedVars
import Luna.Pass.Resolution.Data.UnresolvedConses
import Luna.Pass.Resolution.Data.UnresolvedAccs
import Luna.Pass.Resolution.Data.CurrentTarget
import Luna.Pass.Inference.Data.SimplifierQueue
import Luna.Pass.Inference.Data.MergeQueue
import Luna.Pass.Inference.Data.Unifications

import qualified Luna.Pass.Transform.Desugaring.BlankArguments        as BlankDesugaring
import qualified Luna.Pass.Transform.Desugaring.PatternTransformation as PatternTransformation
import qualified Luna.Pass.Transform.Desugaring.RemoveGrouped         as RemoveGrouped

import qualified Luna.Pass.Resolution.AliasAnalysis           as AliasAnalysis
import qualified Luna.Pass.Resolution.FunctionResolution      as FunctionResolution
import qualified Luna.Pass.Resolution.ConstructorResolution   as ConstructorResolution
import qualified Luna.Pass.Resolution.DeconstructorResolution as DeconstructorResolution
import qualified Luna.Pass.Resolution.MethodResolution        as MethodResolution

import qualified Luna.Pass.Inference.StructuralTyping   as StructuralTyping
import qualified Luna.Pass.Inference.TypeSimplification as Simplification
import qualified Luna.Pass.Inference.UnificationSolver  as UniSolver
import qualified Luna.Pass.Inference.MergeSolver        as MergeSolver
import qualified Luna.Pass.Inference.ErrorPropagation   as ErrorPropagation

import Luna.IR
import Luna.Builtin.Data.Module

import qualified OCI.Pass  as Pass
import qualified Data.Map   as Map
import           Data.Map   (Map)

import Control.Monad.State.Dependent

typecheck :: (MonadState Cache m, MonadPassManager m, MonadIO m) => CurrentTarget -> Imports -> [Expr Draft] -> m (Map (Expr Draft) (Expr Draft))
typecheck tgt imports roots = do
    setAttr (getTypeDesc @CurrentTarget) $ tgt
    setAttr (getTypeDesc @ExprRoots)     $ ExprRoots roots
    setAttr (getTypeDesc @Imports)       $ imports

    initNameGen
    Pass.eval' BlankDesugaring.runBlankDesugaring
    Pass.eval' RemoveGrouped.runRemoveGrouped
    Pass.eval' PatternTransformation.runPatternTransformation

    initUnresolvedVars
    initUnresolvedConses
    initNegativeConses
    initSimplifierQueue
    initUnifications
    initUnresolvedAccs
    initMergeQueue
    initExprMapping
    Pass.eval' AliasAnalysis.runAliasAnalysis
    Pass.eval' ConstructorResolution.runConstructorResolution

    Pass.eval' StructuralTyping.runStructuralTyping
    Pass.eval' FunctionResolution.runFunctionResolution
    Pass.eval' DeconstructorResolution.runDeconstructorResolution

    runTCWhileProgress

    Pass.eval' ErrorPropagation.run

    Just (ExprRoots newRoots) <- unsafeCoerce <$> unsafeGetAttr (getTypeDesc @ExprRoots)

    return $ Map.fromList $ zip roots newRoots

runTCWhileProgress :: (MonadState Cache m, MonadPassManager m, MonadIO m) => m ()
runTCWhileProgress = do
    uniRes  <- Pass.eval' UniSolver.runUnificationSolver
    tsRes   <- Pass.eval' Simplification.runTypeSimplification
    uniRes2 <- Pass.eval' UniSolver.runUnificationSolver
    mrRes   <- Pass.eval' MethodResolution.runMethodResolution
    Pass.eval' MergeSolver.runMergeSolver
    if or [tsRes, uniRes, uniRes2, mrRes] then runTCWhileProgress else return ()

