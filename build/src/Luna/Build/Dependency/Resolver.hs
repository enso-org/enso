module Luna.Build.Dependency.Resolver
    ( SolverFailure(..)
    , solveConstraints
    ) where

import Prologue hiding ( Constraint, Constraints )

import qualified Data.Text as Text

import qualified Data.Map.Strict     as M
import qualified Data.List           as L

import Data.SBV
import Data.SBV.Control hiding (Version)

import Luna.Build.Dependency.Constraint
import qualified Luna.Build.Dependency.Version as V

import Debug.Trace

solverConfig :: SMTConfig
solverConfig = defaultSMTCfg

data SolverFailure
    = UnavailablePackages [Text]
    | UnsatisfiableConstraints
    deriving (Eq, Generic, Ord, Show)

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

constraintQuery :: Constraints -> Versions
                -> Symbolic (Either SolverFailure PackageSet)
constraintQuery constraints versions = do
    let packageNames        = M.keys constraints
        constraintLists     = M.elems constraints
        requiredPrereleases = fmap (fmap (\(Constraint _ ver) -> ver))
                            $ (filter isEQPrereleaseConstraint)
                           <$> constraintLists
        filteredVersions    = (\(r, a) ->
                                filter (\x ->
                                    (not $ V.isPrerelease x) || x `elem` r)
                                a)
                           <$> zip requiredPrereleases (M.elems versions)

    packageSyms <- sequence $ sVersion . Text.unpack <$> packageNames

    -- Restrict symbols by version bounds
    let symConstraintPairs = flatten $ zip packageSyms constraintLists
        flatten a          = concat $ convert <$> a
        convert (a, b)     = (\x -> (a, x)) <$> b

    constrain $ bAnd $ makeRestriction <$> symConstraintPairs

    -- Restrict symbols by available versions
    let makeEqualities (pkg, version) = (\x -> pkg .== versionToSVersion x)
                                     <$> version
        packageEqualities             = makeEqualities
                                     <$> zip packageSyms filteredVersions
        packageDisjunction            = bOr <$> packageEqualities

    constrain $ bAnd packageDisjunction

    -- Extract the results after running if sat
    query $ do
        satResult <- checkSat
        case satResult of
            Unsat -> pure $ Left UnsatisfiableConstraints
            Unk   -> pure $ Left UnsatisfiableConstraints
            Sat   -> do
                concreteVersions <- sequence $ extractSVersion <$> packageSyms
                pure $ Right $ M.fromList $ zip packageNames concreteVersions

-- TODO [Ara] Want to provide the maximal package version in the bounds.
solveConstraints :: (MonadIO m) => Constraints -> Versions
                 -> m (Either SolverFailure PackageSet)
solveConstraints constraints versions = do
    if (L.sort $ M.keys constraints) /= (L.sort $ M.keys versions) then do
        let missingPackages = filter (\x -> x `notElem` M.keys versions)
                            $ M.keys constraints
        pure $ Left $ UnavailablePackages missingPackages
    else do
        result <- liftIO $ runSMTWith solverConfig
                $ constraintQuery constraints versions
        pure result

