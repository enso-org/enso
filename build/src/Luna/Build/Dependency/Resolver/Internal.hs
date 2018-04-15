module Luna.Build.Dependency.Resolver.Internal where

import Prologue hiding (Constraint, Constraints)

import qualified Data.Map.Strict                  as Map
import qualified Data.SBV                         as SBV
import qualified Data.SBV.Control                 as SBV hiding (Version)
import qualified Luna.Build.Dependency.Constraint as Constraint
import qualified Luna.Build.Dependency.Version    as Version

import Data.Either                      (lefts, rights)
import Data.SBV                         ((.==), (.>), (.<), (.<=), (.>=))
import Luna.Build.Dependency.Constraint (Constraint(Constraint))
import Luna.Build.Dependency.Version    (Version(Version))

-----------------------
-- === Utilities === --
-----------------------

data SolverError
    = UnavailablePackages [Text]
    | UnsatisfiableConstraints [Text]
    | UnknownSolution Text
    | BadOptimisation Text
    | ExtensionField
    | SolverError [Text]
    | MissingVariables [Text]
    deriving (Eq, Generic, Ord, Show)

data OptTag = Optimize | NoOptimize deriving (Eq, Show)

solverConfig :: SBV.SMTConfig
solverConfig = SBV.defaultSMTCfg

nameConnector :: String
nameConnector = ":"

optTag :: String
optTag = "max"

majorTag :: String
majorTag = "major"

minorTag :: String
minorTag = "minor"

patchTag :: String
patchTag = "patch"

preTag :: String
preTag = "prerelease"

preVTag :: String
preVTag = "prereleaseVersion"



----------------------------
-- === Solver Version === --
----------------------------

-- === Definition == ---

data SVersion = SVersion
    { __major             :: SBV.SWord64
    , __minor             :: SBV.SWord64
    , __patch             :: SBV.SWord64
    , __prerelease        :: SBV.SWord64
    , __prereleaseVersion :: SBV.SWord64
    } deriving (Eq, Generic)
makeLenses ''SVersion


-- === API === ---

-- TODO [Ara] Have this take a parameter dictating whether or not it maximizes
mkSymbolicSVersion :: String -> OptTag -> SBV.Symbolic SVersion
mkSymbolicSVersion name tag = do
    let genOptName component = optTag <> nameConnector <> name <> nameConnector
                             <> component

    a <- SBV.free $ name <> nameConnector <> majorTag
    b <- SBV.free $ name <> nameConnector <> minorTag
    c <- SBV.free $ name <> nameConnector <> patchTag
    d <- SBV.free $ name <> nameConnector <> preTag
    e <- SBV.free $ name <> nameConnector <> preVTag

    -- Add optimisation constraints for the names
    when (tag == Optimize) $ do
        SBV.maximize (genOptName majorTag) a
        SBV.maximize (genOptName minorTag) b
        SBV.maximize (genOptName patchTag) c
        SBV.maximize (genOptName preTag) d
        SBV.maximize (genOptName preVTag) e

    pure $ SVersion a b c d e

literalSVersion :: Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> SVersion
literalSVersion a b c d e = SVersion
    (SBV.literal a) (SBV.literal b) (SBV.literal c) (SBV.literal d)
    (SBV.literal e)

versionToSVersion :: Version.Version -> SVersion
versionToSVersion (Version a b c pre) = literalSVersion a b c d e where
    (d, e) = case pre of
        Nothing -> (Version.noPrereleaseNum, 0)
        Just (Version.Prerelease ty ver) -> (Version.prereleaseTyToNum ty, ver)

extractSVersion :: SVersion -> SBV.Query Version
extractSVersion name = Version <$> SBV.getValue (__major name)
                               <*> SBV.getValue (__minor name)
                               <*> SBV.getValue (__patch name)
                               <*> mkPrerelease
    where mkPrerelease = do
            pre  <- SBV.getValue (__prerelease name)
            preV <- SBV.getValue (__prereleaseVersion name)
            if pre >= Version.noPrereleaseNum then pure Nothing
            else pure
                $ Just (Version.Prerelease (Version.numToPrereleaseTy pre) preV)

extractOptSVersion :: [Word64] -> Version
extractOptSVersion [a, b, c, d, e] = Version a b c prerelease where
    prerelease = if d == Version.noPrereleaseNum then Nothing else
        Just (Version.Prerelease (Version.numToPrereleaseTy d) e)
extractOptSVersion _ = Version 0 0 0 Nothing

-- === Instances === --

instance Show SVersion where
    show _ = "SVersion"

instance SBV.Mergeable SVersion

instance SBV.EqSymbolic SVersion where
    SVersion a1 b1 c1 d1 e1 .== SVersion a2 b2 c2 d2 e2 =
        (a1, b1, c1, d1, e1) .== (a2, b2, c2, d2, e2)

instance SBV.OrdSymbolic SVersion where
    (SVersion majorL minorL patchL prereleaseL prereleaseVersionL) .<
        (SVersion majorR minorR patchR prereleaseR prereleaseVersionR)
        = SBV.ite (majorL             .< majorR)             SBV.true
        . SBV.ite (majorL             .> majorR)             SBV.false
        . SBV.ite (minorL             .< minorR)             SBV.true
        . SBV.ite (minorL             .> minorR)             SBV.false
        . SBV.ite (patchL             .< patchR)             SBV.true
        . SBV.ite (patchL             .> patchR)             SBV.false
        . SBV.ite (prereleaseL        .< prereleaseR)        SBV.true
        . SBV.ite (prereleaseL        .> prereleaseR)        SBV.false
        $ SBV.ite (prereleaseVersionL .< prereleaseVersionR) SBV.true SBV.false

instance (SBV.Provable p) => SBV.Provable (SVersion -> p) where
    forAll_ f    = SBV.forAll_    (\(a,b,c,d,e) -> f (SVersion a b c d e))
    forAll ns f  = SBV.forAll ns  (\(a,b,c,d,e) -> f (SVersion a b c d e))
    forSome_ f   = SBV.forSome_   (\(a,b,c,d,e) -> f (SVersion a b c d e))
    forSome ns f = SBV.forSome ns (\(a,b,c,d,e) -> f (SVersion a b c d e))



------------------------------
-- === Solver Utilities === --
------------------------------

-- === API === --

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

genOptNames :: String -> [String]
genOptNames name =
    [ optTag <> nameConnector <> name <> nameConnector <> majorTag
    , optTag <> nameConnector <> name <> nameConnector <> minorTag
    , optTag <> nameConnector <> name <> nameConnector <> patchTag
    , optTag <> nameConnector <> name <> nameConnector <> preTag
    , optTag <> nameConnector <> name <> nameConnector <> preVTag ]



---------------------------
-- === Solver Script === --
---------------------------

-- === API === --

extractVersions :: Constraint.Constraints -> Constraint.Versions
                -> SBV.OptimizeResult
                -> IO (Either SolverError Constraint.PackageSet)
extractVersions constraints versions solverResult = case solverResult of
    SBV.ParetoResult _                -> pure . Left
        $ BadOptimisation "Pareto model found."
    SBV.IndependentResult _           -> pure . Left
        $ BadOptimisation "Independent model found."
    SBV.LexicographicResult smtResult -> case smtResult of
        SBV.Satisfiable _ _     -> do
            let modelObjectives = SBV.getModelObjectives smtResult
                packageNames    = Map.keys constraints
                packageStrs     = convert <$> packageNames
                optNames        = genOptNames <$> packageStrs

                getValue :: String -> Either SolverError Word64
                getValue tag = case tag `Map.lookup` modelObjectives of
                    Nothing -> Left . MissingVariables $ convert tag
                    Just (SBV.ExtendedCW _) -> Left ExtensionField
                    Just (SBV.RegularCW  w) -> case SBV.parseCWs [w] of
                        Just (i, []) -> Right i
                        _            -> Left . MissingVariables $ convert tag

                valueTuples = (\xs -> getValue <$> xs) <$> optNames
                pkgValues = rights <$> valueTuples

            if null . concat $ lefts <$> valueTuples then do
                let solvedVersions = extractOptSVersion <$> pkgValues
                    pairs          = zip packageNames solvedVersions

                pure . Right $ Map.fromList pairs
            else pure . Left . SolverError
                      $ ["Critical error when extracting variables."]

        SBV.SatExtField _ _     -> pure . Left $ ExtensionField
        SBV.Unsatisfiable _     -> runNonOptQuery constraints versions
        SBV.Unknown _ reasonStr -> pure . Left . UnknownSolution
                                        $ convert reasonStr
        SBV.ProofError _ errors -> pure . Left . SolverError
                                        $ convert <$> errors

constraintScript :: Constraint.Constraints -> Constraint.Versions -> OptTag
                 -> SBV.Symbolic ()
constraintScript constraints versions tag = do
    let packageNames        = Map.keys constraints
        constraintLists     = Map.elems constraints
        requiredPrereleases = fmap (fmap (\(Constraint _ ver) -> ver))
                            $ filter Constraint.isEQPrerelease
                           <$> constraintLists
        filteredVersions    = (\(requiredReleases, availableReleases) ->
                                filter (\x ->
                                    not (Version.isPrerelease x)
                                    || x `elem` requiredReleases)
                                availableReleases)
                           <$> zip requiredPrereleases (Map.elems versions)

    -- Make a symbol for each package name and initialise optimisation goals
    packageSyms <- sequence $ (`mkSymbolicSVersion` tag) . convert
                           <$> packageNames

    -- Restrict symbols by version bounds
    let nameSymConstraints       = zip3 packageNames packageSyms
                                    constraintLists
        triple (name, sym, vers) = (\x -> (name, sym, x)) <$> vers
        triples                  = concat $ triple <$> nameSymConstraints
        nameConstraintPairs      = (\(name, sym, ver) ->
                                    ( convert $ name <> " "
                                                     <> prettyShow ver
                                    , makeRestriction (sym, ver) ))
                                <$> triples

    sequence_ $ uncurry genNamedConstraint <$> nameConstraintPairs

    -- Restrict symbols by available versions
    let mkEqs (pkg, ver)   = (\x -> pkg .== versionToSVersion x) <$> ver
        packageEqualities  = mkEqs <$> zip packageSyms filteredVersions
        packageDisjunction = SBV.bOr <$> packageEqualities
        genPackageName nm  = "Available" <> convert nm
        namedDisjunctions  = zip (genPackageName <$> packageNames)
                                 packageDisjunction

    sequence_ $ uncurry genNamedConstraint <$> namedDisjunctions

constraintQuery :: Constraint.Constraints -> Constraint.Versions
                -> IO (Either SolverError Constraint.PackageSet)
constraintQuery constraints versions = extractVersions constraints versions
    =<< SBV.optimizeWith solverConfig SBV.Lexicographic
        (constraintScript constraints versions Optimize)

nonOptQuery :: Constraint.Constraints -> Constraint.Versions
            -> SBV.Symbolic (Either SolverError Constraint.PackageSet)
nonOptQuery constraints versions = do
    constraintScript constraints versions NoOptimize

    -- Set solver options prior to calling `checkSat`
    SBV.setOption $ SBV.ProduceUnsatCores True

    -- Extract the results
    SBV.query $ SBV.checkSat >>= \case
        SBV.Unsat -> do
            core <- SBV.getUnsatCore
            pure . (Left . UnsatisfiableConstraints) $ convert <$> core
        SBV.Unk   -> pure . Left $ SolverError ["Could not solve constraints."]
        SBV.Sat   -> pure . Left $ SolverError ["Inconsistent solver results."]

runNonOptQuery :: Constraint.Constraints -> Constraint.Versions
               -> IO (Either SolverError Constraint.PackageSet)
runNonOptQuery constraints versions = SBV.runSMTWith solverConfig
    $ nonOptQuery constraints versions

