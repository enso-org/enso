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

solverConfig :: SMTConfig
solverConfig = defaultSMTCfg

data SolverFailure
    = UnavailablePackages [Text]
    | UnsatisfiableConstraints [Text]
    | UnknownSolution Text
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

genNamedConstraint :: (String, SBool) -> Symbolic ()
genNamedConstraint (name, con) = namedConstraint name con >> pure ()

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

    _ <- sequence $ genNamedConstraint <$> nameConstraintPairs

    -- Restrict symbols by available versions
    let mkEqs (pkg, ver)   = (\x -> pkg .== versionToSVersion x) <$> ver
        packageEqualities  = mkEqs <$> zip packageSyms filteredVersions
        packageDisjunction = bOr <$> packageEqualities
        namedDisjunctions  = zip ((\nm ->
                                    "Available " <> Text.unpack nm)
                                    <$> packageNames)
                                 packageDisjunction

    _ <- sequence $ genNamedConstraint <$> namedDisjunctions

    -- Ensure the solver gets the maximum of each Package version

    -- Set solver options prior to calling `checkSat`
    setOption $ ProduceUnsatCores True

    -- Extract the results
    query $ do
        satResult <- checkSat
        case satResult of
            Unsat -> do
                core <- getUnsatCore
                pure $ Left $ UnsatisfiableConstraints $ Text.pack <$> core
            Unk   -> do
                reason <- getUnknownReason
                pure $ Left $ UnknownSolution $ Text.pack reason
            Sat   -> do
                concreteVersions <- sequence $ extractSVersion <$> packageSyms
                pure $ Right $ M.fromList $ zip packageNames concreteVersions

-- TODO [Ara] Is it possible to generate these as individual named constraints?
-- TODO [Ara] Want to provide the maximal package version in the bounds.
solveConstraints :: (MonadIO m) => Constraints -> Versions
                 -> m (Either SolverFailure PackageSet)
solveConstraints constraints versions = do
    if (L.sort $ M.keys constraints) /= (L.sort $ M.keys versions) then do
        let missingPackages = filter (\x -> x `notElem` M.keys versions)
                            $ M.keys constraints
        pure $ Left $ UnavailablePackages missingPackages
    else do
        liftIO $ runSMTWith solverConfig $ constraintQuery constraints versions

