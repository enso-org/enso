module Luna.Build.Dependency.Resolver ( solveConstraints ) where

import Data.Generics hiding (Generic)

import Prologue hiding ((.>))

import qualified Prelude as P

import qualified Data.Text as Text

import qualified Control.Monad.Trans as C
import qualified Data.Map.Strict     as M
import qualified Data.Traversable    as T

import Control.Applicative
import Control.Monad ( join )
import Data.Maybe

import Data.SBV

import Luna.Build.Dependency.Constraint

import Debug.Trace

-- TODO [Ara] define this ourselves
solverConfig :: SMTConfig
solverConfig = defaultSMTCfg -- current default is Z3, so leave it as is

data RVersion = RVersion
    { __major             :: SInteger
    , __minor             :: SInteger
    , __patch             :: SInteger
    , __prerelease        :: SInteger
    , __prereleaseVersion :: SInteger
    } deriving (Eq, Generic, Show, Typeable)
makeLenses ''RVersion

instance Mergeable RVersion
instance HasKind RVersion
instance Data RVersion
instance Ord RVersion
instance Read RVersion
instance SymWord RVersion
instance SatModel RVersion

instance EqSymbolic RVersion where
    (RVersion majorL minorL patchL prereleaseL prereleaseVersionL) .==
        (RVersion majorR minorR patchR prereleaseR prereleaseVersionR) =
        (majorL             .== majorR) &&&
        (minorL             .== minorR) &&&
        (patchL             .== patchR) &&&
        (prereleaseL        .== prereleaseR) &&&
        (prereleaseVersionL .== prereleaseVersionR)

instance OrdSymbolic RVersion where
    (RVersion majorL minorL patchL prereleaseL prereleaseVersionL) .<
        (RVersion majorR minorR patchR prereleaseR prereleaseVersionR) =
        ite (majorL             .< majorR) true       $
        ite (majorL             .> majorR) false      $
        ite (minorL             .< minorR) true       $
        ite (minorL             .> minorR) false      $
        ite (patchL             .< patchR) true       $
        ite (patchL             .> patchR) false      $
        ite (prereleaseL        .< prereleaseR) true  $
        ite (prereleaseL        .> prereleaseR) false $
        ite (prereleaseVersionL .< prereleaseVersionR) true false

type SRVersion = SBV RVersion

mkSRVersion :: SInteger -> SInteger -> SInteger -> SInteger -> SInteger
            -> SRVersion
mkSRVersion a b c d e = literal $ RVersion a b c d e

symbolicRVersion :: String -> Symbolic SRVersion
symbolicRVersion name = do
    maj  <- symbolic $ name <> "_major"
    min  <- symbolic $ name <> "_minor"
    pat  <- symbolic $ name <> "_patch"
    pre  <- symbolic $ name <> "_prerelease"
    preV <- symbolic $ name <> "_preVersion"
    pure $ mkSRVersion maj min pat pre preV

data B = B Integer Integer deriving (Eq, Ord, Show, Read, Data)

instance SymWord B
instance HasKind B
instance SatModel B

constraintPredicate :: ConstraintMap -> Predicate
constraintPredicate constraints = do
    let keys = M.keys constraints
        elems = M.elems constraints

    {- traceShowM keys -}
    {- traceShowM elems -}

    {- foo <- symbolic "foo" -}
    {- bar <- symbolic "bar" -}

    let ver1 = mkSRVersion 0 0 1 3 0
        ver2 = mkSRVersion 1 1 2 3 0

    {- traceShowM $ isSymbolic ver1 -}

    pure $ true --ver1 .< ver2

-- TODO [Ara] Needs to take list of available versions. (foo == a ||| b ||| c)
-- TODO [Ara] Turn result into resolved deps
solveConstraints :: (MonadIO m) => ConstraintMap -> m (Maybe Int)
solveConstraints constraints = do
    r@(SatResult modelResult) <- liftIO $ runSolver constraints
    traceShowM r
    case modelResult of
        Unsatisfiable _ -> pure $ Just 0
        Satisfiable _ model -> do
            traceShowM model
            pure $ Just 1
        SatExtField _ model -> pure $ Just 1
        Unknown _ reason -> traceShowM reason >> pure $ Just 0
        ProofError _ xs -> traceShowM xs >> pure $ Just 0

runSolver :: ConstraintMap -> IO SatResult
runSolver constraints = sat $ constraintPredicate constraints

