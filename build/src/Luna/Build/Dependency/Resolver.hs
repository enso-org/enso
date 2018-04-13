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
import Data.SBV.Control hiding (Version)

import Luna.Build.Dependency.Constraint
import qualified Luna.Build.Dependency.Version as V

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
    } deriving (Eq, Generic)
makeLenses ''SVersion

instance Show SVersion where
    show _ = "<symbolic> :: SVersion"

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

literalSVersion :: Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> SVersion
literalSVersion a b c d e =
    SVersion (literal a) (literal b) (literal c) (literal d) (literal e)

symLiteralSVersion :: Word64 -> Word64 -> Word64 -> Word64 -> Word64
                   -> Symbolic SVersion
symLiteralSVersion a b c d e = pure $ literalSVersion a b c d e

versionToSVersion :: V.Version -> SVersion
versionToSVersion (V.Version a b c pre) = literalSVersion a b c d e
    where (d, e) = case pre of
            Nothing -> (3, 0)
            Just (V.Prerelease ty ver) -> (V.prereleaseTyToNum ty, ver)

extractSVersion :: SVersion -> Query V.Version
extractSVersion name = V.Version <$> getValue (__major name)
                                 <*> getValue (__minor name)
                                 <*> getValue (__patch name)
                                 <*> mkPrerelease
    where mkPrerelease = do
            pre <- getValue (__prerelease name)
            preV <- getValue (__prereleaseVersion name)
            if pre >= 3 then pure $ Nothing
            else pure $ Just (V.Prerelease (V.numToPrereleaseTy pre) preV)

makeRestriction :: (SVersion, Constraint) -> SBool
makeRestriction (pkg, Constraint ty ver) = pkg `op` versionToSVersion ver
    where op = case ty of
                    ConstraintEQ -> (.==)
                    ConstraintGT -> (.>)
                    ConstraintLT -> (.<)
                    ConstraintLE -> (.<=)
                    ConstraintGE -> (.>=)

constraintQuery :: Constraints -> Versions -> Symbolic (Either Text PackageSet)
constraintQuery constraints versions = do
    let packageNames        = M.keys constraints
        constraintLists     = M.elems constraints
        availableVersions   = M.elems versions
        requiredPrereleases = fmap (fmap (\(Constraint _ ver) -> ver))
                            $ (filter isEQPrereleaseConstraint)
                           <$> constraintLists
        filteredVersions    = zip requiredPrereleases availableVersions

    traceShowM requiredPrereleases

    packageSyms <- sequence $ sVersion . Text.unpack <$> packageNames

    -- Restrict symbols by version bounds
    let symConstraintPairs = flatten $ zip packageSyms constraintLists
        flatten a = concat $ convert <$> a
        convert (a, b) = (\x -> (a, x)) <$> b

    constrain $ bAnd $ makeRestriction <$> symConstraintPairs

    -- Restrict symbols by available versions
    let packageEqualities = makeEqualities <$> zip packageSyms filteredVersions
        makeEqualities (pkg, version) = (\x -> pkg .== versionToSVersion x)
                                     <$> version
        packageDisjunction = bOr <$> packageEqualities

    constrain $ bAnd packageDisjunction

    -- Extract the results after running if sat
    query $ do
        satResult <- checkSat
        case satResult of
            Unsat -> pure $ Left "Unsat"
            Unk -> pure $ Left "Unknown solution."
            Sat -> do
                concreteVersions <- sequence $ extractSVersion <$> packageSyms
                pure $ Right $ M.fromList $ zip packageNames concreteVersions

-- TODO [Ara] function to detect prereleases not able to be chosen.
-- TODO [Ara] Should not select prereleases unless there is an EQ constraint
-- TODO [Ara] Want to provide the maximal package version in the bounds.
solveConstraints :: (MonadIO m) => Constraints -> Versions -> m (Either Int Int)
solveConstraints constraints versions = do
    if (L.sort $ M.keys constraints) /= (L.sort $ M.keys versions) then do
        pure $ Left 0
    else do
        result <- liftIO $ runSMT $ constraintQuery constraints versions
        case result of
            Left _ -> pure $ Left 0
            Right foo -> do
                traceShowM foo
                pure $ Right 1

