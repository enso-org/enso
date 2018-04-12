module Luna.Build.Dependency.Resolver ( solveConstraints ) where

import Prologue hiding ( (.>), Constraint, Constraints )

import qualified Prelude as P

import qualified Data.Text as Text

import qualified Control.Monad.Trans as C
import qualified Data.Map.Strict     as M
import qualified Data.Traversable    as T
import qualified Data.List           as L

import Control.Applicative
import Control.Monad ( join )
import Data.Maybe

import Data.SBV

import Luna.Build.Dependency.Constraint
import Luna.Build.Dependency.Version

import Debug.Trace

-- TODO [Ara] define this ourselves
solverConfig :: SMTConfig
solverConfig = defaultSMTCfg -- current default is Z3, so leave it as is

data SVersion = SVersion
    { __major             :: SWord64
    , __minor             :: SWord64
    , __patch             :: SWord64
    , __prerelease        :: SWord64
    , __prereleaseVersion :: SWord64
    } deriving (Eq, Generic, Show)
makeLenses ''SVersion

instance Mergeable SVersion

instance EqSymbolic SVersion where
    SVersion a1 b1 c1 d1 e1 .== SVersion a2 b2 c2 d2 e2 =
        (a1, b1, c1, d1, e1) .== (a2, b2, c2, d2, e2)

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

instance (Provable p) => Provable (SVersion -> p) where
    forAll_ f    = forAll_ (\(a,b,c,d,e)    -> f (SVersion a b c d e))
    forAll ns f  = forAll ns (\(a,b,c,d,e)  -> f (SVersion a b c d e))
    forSome_ f   = forSome_ (\(a,b,c,d,e)   -> f (SVersion a b c d e))
    forSome ns f = forSome ns (\(a,b,c,d,e) -> f (SVersion a b c d e))

sVersion :: String -> Symbolic SVersion
sVersion name = do
    a <- free $ name <> ":major"
    b <- free $ name <> ":minor"
    c <- free $ name <> ":patch"
    d <- free $ name <> ":prerelease"
    e <- free $ name <> ":prereleaseVersion"
    pure $ SVersion a b c d e

literalSVersion :: Word64 -> Word64 -> Word64 -> Word64 -> Word64
                -> Symbolic SVersion
literalSVersion a b c d e =
    pure $ SVersion (literal a) (literal b) (literal c) (literal d) (literal e)

versionToSVersion :: Version -> Symbolic SVersion
versionToSVersion (Version a b c pre) = literalSVersion a b c d e
    where (d, e) = case pre of
            Nothing -> (3, 0)
            Just (Prerelease ty ver) -> (numOf ty, ver)
          numOf Alpha = 0
          numOf Beta  = 1
          numOf RC    = 2

-- TODO [Ara] function to detect prereleases not able to be chosen.
-- Can this be done faster than n^2?

constraintPredicate :: Constraints -> Versions -> Predicate
constraintPredicate constraints versions = do
    let packageNames = M.keys constraints
        unpackedConstraints = unpack constraints
        unpackedVersions = unpack versions

    traceShowM packageNames
    traceShowM unpackedConstraints
    traceShowM unpackedVersions

    freeV1 <- sVersion "foo"
    minPossibleVersion <- literalSVersion 0 0 1 0 0

    literalV1 <- literalSVersion 44 31 36 3 0
    literalV2 <- literalSVersion 71 46 3 2 7

    pure $ literalV1 .< literalV2

-- TODO [Ara] Should not select prereleases unless there is an EQ constraint
-- TODO [Ara] Can we construct a metric function from the result to maximise?
-- TODO [Ara] Use everything before the last : as the package name.
-- TODO [Ara] Want to provide the maximal package version in the bounds.
-- TODO [Ara] Return Either err res so as to be able to provide some diganostics
solveConstraints :: (MonadIO m) => Constraints -> Versions -> m (Either Int Int)
solveConstraints constraints versions = do
    if (L.sort $ M.keys constraints) /= (L.sort $ M.keys versions) then do
        pure $ Left 0
    else do
        r@(SatResult modelResult) <- liftIO $ runSolver constraints versions
        traceShowM r
        case modelResult of
            Unsatisfiable _ -> pure $ Left 0
            Satisfiable _ model -> pure $ Right 1
            SatExtField _ model -> pure $ Right 1
            Unknown _ reason -> traceShowM reason >> pure $ Left 0
            ProofError _ xs -> traceShowM xs >> pure $ Left 0

-- needs `getModelValue`
extractPackageSet :: (Modelable a) => [Text] -> a -> PackageSet
extractPackageSet packages model = undefined

runSolver :: Constraints -> Versions -> IO SatResult
runSolver constraints versions = sat $ constraintPredicate constraints versions

