module Luna.Build.Dependency.Resolver ( solveConstraints ) where

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

data SVersion = SVersion
    { __major             :: SInteger
    , __minor             :: SInteger
    , __patch             :: SInteger
    , __prerelease        :: SInteger
    , __prereleaseVersion :: SInteger
    } deriving (Generic, Show, Typeable)
makeLenses ''SVersion

instance EqSymbolic SVersion where
    (SVersion majorL minorL patchL prereleaseL prereleaseVersionL) .==
        (SVersion majorR minorR patchR prereleaseR prereleaseVersionR) =
        (majorL             .== majorR) &&&
        (minorL             .== minorR) &&&
        (patchL             .== patchR) &&&
        (prereleaseL        .== prereleaseR) &&&
        (prereleaseVersionL .== prereleaseVersionR)

instance OrdSymbolic SVersion where
    (SVersion majorL minorL patchL prereleaseL prereleaseVersionL) .<
        (SVersion majorR minorR patchR prereleaseR prereleaseVersionR) =
        ite (majorL             .< majorR) true       $
        ite (majorL             .> majorR) false      $
        ite (minorL             .< minorR) true       $
        ite (minorL             .> minorR) false      $
        ite (patchL             .< patchR) true       $
        ite (patchL             .> patchR) false      $
        ite (prereleaseL        .< prereleaseR) true  $
        ite (prereleaseL        .> prereleaseR) false $
        ite (prereleaseVersionL .< prereleaseVersionR) true false

instance Mergeable SVersion

constraintPredicate :: ConstraintMap -> Predicate
constraintPredicate constraints = do
    let keys = M.keys constraints
        elems = M.elems constraints

    traceShowM keys
    traceShowM elems

    {- foo <- symbolic "foo" -}
    {- bar <- symbolic "bar" -}

    let ver1 = SVersion 0 0 1 3 0
        ver2 = SVersion 1 1 2 3 0

    {- traceShowM $ isSymbolic ver1 -}

    pure $ ver1 .< ver2

-- TODO [Ara] Make this actually do something with the result
solveConstraints :: (MonadIO m) => ConstraintMap -> m (Maybe Int)
solveConstraints constraints = do
    liftIO $ runSolver constraints
    pure $ Just 1

runSolver :: ConstraintMap -> IO SatResult
runSolver constraints = sat $ constraintPredicate constraints

