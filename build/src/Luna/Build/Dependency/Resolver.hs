module Luna.Build.Dependency.Resolver where

import Prologue hiding (Constraint, Constraints)

import qualified Data.List                        as List
import qualified Data.Map.Strict                  as Map
import qualified Data.SBV                         as SBV
import qualified Data.SBV.Control                 as SBV hiding (Version)
import qualified Data.Text                        as Text
import qualified Luna.Build.Dependency.Constraint as Constraint
import qualified Luna.Build.Dependency.Version    as Version

import Data.SBV ((.==), (.>), (.<), (.<=), (.>=))
import Luna.Build.Dependency.Constraint (Constraint(Constraint))
import Luna.Build.Dependency.Version (Version(Version))

-- TODO [Ara] Move most of this into internal.

solverConfig :: SBV.SMTConfig
solverConfig = SBV.defaultSMTCfg

data SolverFailure
    = UnavailablePackages [Text]
    | UnsatisfiableConstraints [Text]
    | UnknownSolution Text
    deriving (Eq, Generic, Ord, Show)

data SVersion = SVersion
    { __major             :: SBV.SWord64
    , __minor             :: SBV.SWord64
    , __patch             :: SBV.SWord64
    , __prerelease        :: SBV.SWord64
    , __prereleaseVersion :: SBV.SWord64
    } deriving (Eq, Generic)
makeLenses ''SVersion

instance Show SVersion where
    show _ = "<symbolic> :: SVersion"

instance SBV.Mergeable SVersion

instance SBV.EqSymbolic SVersion where
    SVersion a1 b1 c1 d1 e1 .== SVersion a2 b2 c2 d2 e2 =
        (a1, b1, c1, d1, e1) .== (a2, b2, c2, d2, e2)

instance SBV.OrdSymbolic SVersion where
    (SVersion majorL minorL patchL prereleaseL prereleaseVersionL) .<
        (SVersion majorR minorR patchR prereleaseR prereleaseVersionR) =
        SBV.ite (majorL             .< majorR)             SBV.true  $
        SBV.ite (majorL             .> majorR)             SBV.false $
        SBV.ite (minorL             .< minorR)             SBV.true  $
        SBV.ite (minorL             .> minorR)             SBV.false $
        SBV.ite (patchL             .< patchR)             SBV.true  $
        SBV.ite (patchL             .> patchR)             SBV.false $
        SBV.ite (prereleaseL        .< prereleaseR)        SBV.true  $
        SBV.ite (prereleaseL        .> prereleaseR)        SBV.false $
        SBV.ite (prereleaseVersionL .< prereleaseVersionR) SBV.true SBV.false

instance (SBV.Provable p) => SBV.Provable (SVersion -> p) where
    forAll_ f    = SBV.forAll_ (\(a,b,c,d,e)    -> f (SVersion a b c d e))
    forAll ns f  = SBV.forAll ns (\(a,b,c,d,e)  -> f (SVersion a b c d e))
    forSome_ f   = SBV.forSome_ (\(a,b,c,d,e)   -> f (SVersion a b c d e))
    forSome ns f = SBV.forSome ns (\(a,b,c,d,e) -> f (SVersion a b c d e))

sVersion :: String -> SBV.Symbolic SVersion
sVersion name = do
    a <- SBV.free $ name <> ":major"
    b <- SBV.free $ name <> ":minor"
    c <- SBV.free $ name <> ":patch"
    d <- SBV.free $ name <> ":prerelease"
    e <- SBV.free $ name <> ":prereleaseVersion"
    pure $ SVersion a b c d e

literalSVersion :: Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> SVersion
literalSVersion a b c d e = SVersion
    (SBV.literal a) (SBV.literal b) (SBV.literal c) (SBV.literal d)
    (SBV.literal e)

versionToSVersion :: Version.Version -> SVersion
versionToSVersion (Version a b c pre) = literalSVersion a b c d e
    where (d, e) = case pre of
            Nothing -> (3, 0)
            Just (Version.Prerelease ty ver) -> (Version.prereleaseTyToNum ty, ver)

extractSVersion :: SVersion -> SBV.Query Version
extractSVersion name = Version <$> SBV.getValue (__major name)
                               <*> SBV.getValue (__minor name)
                               <*> SBV.getValue (__patch name)
                               <*> mkPrerelease
    where mkPrerelease = do
            pre  <- SBV.getValue (__prerelease name)
            preV <- SBV.getValue (__prereleaseVersion name)
            if pre >= 3 then pure $ Nothing
            else pure $ Just (Version.Prerelease (Version.numToPrereleaseTy pre) preV)

makeRestriction :: (SVersion, Constraint) -> SBV.SBool
makeRestriction (pkg, Constraint ty ver) = pkg `op` versionToSVersion ver
    where op = case ty of
                    Constraint.EQ -> (.==)
                    Constraint.GT -> (.>)
                    Constraint.LT -> (.<)
                    Constraint.LE -> (.<=)
                    Constraint.GE -> (.>=)

genNamedConstraint :: String -> SBV.SBool -> SBV.Symbolic ()
genNamedConstraint name con = void $ SBV.namedConstraint name con

constraintQuery :: Constraint.Constraints -> Constraint.Versions
                -> SBV.Symbolic (Either SolverFailure Constraint.PackageSet)
constraintQuery constraints versions = do
    let packageNames        = Map.keys constraints
        constraintLists     = Map.elems constraints
        requiredPrereleases = fmap (fmap (\(Constraint _ ver) -> ver))
                            $ (filter Constraint.isEQPrerelease)
                           <$> constraintLists
        filteredVersions    = (\(r, a) ->
                                filter (\x ->
                                    (not $ Version.isPrerelease x) || x `elem` r)
                                a)
                           <$> zip requiredPrereleases (Map.elems versions)

    -- Make a symbol for each package name
    packageSyms <- sequence $ sVersion . Text.unpack <$> packageNames

    -- Restrict symbols by version bounds
    let nameSymConstraints       = zip3 packageNames packageSyms constraintLists
        triple (name, sym, vers) = (\x -> (name, sym, x)) <$> vers
        triples                  = concat $ triple <$> nameSymConstraints
        nameConstraintPairs      = (\(name, sym, ver) ->
                                    ( (Text.unpack name) <> " " <> show ver
                                    , makeRestriction (sym, ver) ))
                                <$> triples

    sequence_ $ uncurry genNamedConstraint <$> nameConstraintPairs

    -- Restrict symbols by available versions
    let mkEqs (pkg, ver)   = (\x -> pkg .== versionToSVersion x) <$> ver
        packageEqualities  = mkEqs <$> zip packageSyms filteredVersions
        packageDisjunction = SBV.bOr <$> packageEqualities
        namedDisjunctions  = zip ((\nm ->
                                    "Available " <> Text.unpack nm)
                                    <$> packageNames)
                                 packageDisjunction

    sequence_ $ uncurry genNamedConstraint <$> namedDisjunctions

    -- TODO [Ara] Ensure the solver gets the maximum of each Package version

    -- Set solver options prior to calling `checkSat`
    SBV.setOption $ SBV.ProduceUnsatCores True

    -- Extract the results
    SBV.query $ do
        satResult <- SBV.checkSat
        case satResult of
            SBV.Unsat -> do
                core <- SBV.getUnsatCore
                pure $ Left $ UnsatisfiableConstraints $ Text.pack <$> core
            SBV.Unk   -> do
                reason <- SBV.getUnknownReason
                pure $ Left $ UnknownSolution $ Text.pack reason
            SBV.Sat   -> do
                concreteVersions <- sequence $ extractSVersion <$> packageSyms
                pure $ Right $ Map.fromList $ zip packageNames concreteVersions

solveConstraints :: (MonadIO m) => Constraint.Constraints -> Constraint.Versions
                 -> m (Either SolverFailure Constraint.PackageSet)
solveConstraints constraints versions = do
    if (List.sort $ Map.keys constraints) /= (List.sort $ Map.keys versions) then do
        let missingPackages = filter (\x -> x `notElem` Map.keys versions)
                            $ Map.keys constraints
        pure $ Left $ UnavailablePackages missingPackages
    else do
        liftIO $ SBV.runSMTWith solverConfig
               $ constraintQuery constraints versions

