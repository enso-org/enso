module Luna.Build.Dependency.Resolver.Internal where

import Prologue hiding (Constraint, Constraints)

import qualified Data.Map.Strict                  as Map
import qualified Data.SBV                         as SBV
import qualified Data.SBV.Control                 as SBV hiding (Version)
import qualified Luna.Build.Dependency.Constraint as Constraint
import qualified Luna.Build.Dependency.Version    as Version

import Data.SBV                         ((.==), (.>), (.<), (.<=), (.>=))
import Luna.Build.Dependency.Constraint (Constraint(Constraint))
import Luna.Build.Dependency.Version    (Version(Version))

import Debug.Trace

-----------------------
-- === Utilities === --
-----------------------

data SolverError
    = UnavailablePackages [Text]
    | UnsatisfiableConstraints [Text]
    | UnknownSolution Text
    deriving (Eq, Generic, Ord, Show)

solverConfig :: SBV.SMTConfig
solverConfig = SBV.defaultSMTCfg



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

mkSymbolicSVersion :: String -> SBV.Symbolic SVersion
mkSymbolicSVersion name = do
    let nameConnector = ":"
    a <- SBV.free $ name <> nameConnector <> "major"
    b <- SBV.free $ name <> nameConnector <> "minor"
    c <- SBV.free $ name <> nameConnector <> "patch"
    d <- SBV.free $ name <> nameConnector <> "prerelease"
    e <- SBV.free $ name <> nameConnector <> "prereleaseVersion"
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



-- TODO [Ara] Want to maximize ALL the major versions first, and then so on

---------------------------
-- === Solver Script === --
---------------------------
data FooGen a = Foo { v1 :: a
                    , v2 :: a
                    , v3 :: a
                    , v4 :: a
                    , v5 :: a
                    }
                    deriving Show

type SFoo = FooGen SBV.SWord64
type Foo  = FooGen Word64

sFoo :: String -> SBV.Symbolic SFoo
sFoo nm = Foo <$> SBV.free_
              <*> SBV.free_
              <*> SBV.free_
              <*> SBV.free_
              <*> SBV.free_
-- === API === --

extractVersions :: SBV.OptimizeResult -> IO Foo
extractVersions r = do
    let m = case r of
                SBV.LexicographicResult lr -> SBV.getModelObjectives lr
                _                      -> error "Non-lexicographic optimization, need more code here!"
        grab s = case s `Map.lookup` m of
                     Nothing             -> error $ "Cannot find " <> s <> " in the optimal model!"
                     Just (SBV.RegularCW  w) -> case SBV.parseCWs [w] of
                                              Just (i, []) -> pure i
                                              _            -> error $ "Couldn't extract optimal value for field " <> s
                     Just (SBV.ExtendedCW v) -> error $ "Optimal value is in an extension field for " <> s <> ": " <> show v

    Foo <$> grab "min-v1"
        <*> grab "min-v2"
        <*> grab "min-v3"
        <*> grab "min-v4"
        <*> grab "min-v5"

constraintQuery :: Constraint.Constraints -> Constraint.Versions
                -> IO Foo
constraintQuery constraints versions
    = extractVersions =<< SBV.optimize SBV.Lexicographic constraintScript where
    constraintScript = do
        Foo{v1, v2, v3, v4, v5} <- sFoo "myVersion"

        -- some arbitrary constraints:
        SBV.constrain $ v1 .> 5
        SBV.constrain $ v2 .> 10
        SBV.constrain $ v3 .== v4 + v5
        SBV.constrain $ v3 .> 15
        SBV.constrain $ v1 .>= 0 SBV.&&& v1 .< 20
        SBV.constrain $ v2 .>= 0 SBV.&&& v2 .< 30
        SBV.constrain $ v3 .>= 4
        SBV.constrain $ v4 .>= 12
        SBV.constrain $ v5 .>= 0
        SBV.constrain $ v3 .<= 40

        -- Minimize each component:
        SBV.minimize "min-v1" v1
        SBV.minimize "min-v2" v2
        SBV.minimize "min-v3" v3
        SBV.minimize "min-v4" v4
        SBV.minimize "min-v5" v5

{- constraintQuery :: Constraint.Constraints -> Constraint.Versions -}
                {- -> SBV.Symbolic (Either SolverError Constraint.PackageSet) -}
{- constraintQuery constraints versions = do -}
    {- let packageNames        = Map.keys constraints -}
        {- constraintLists     = Map.elems constraints -}
        {- requiredPrereleases = fmap (fmap (\(Constraint _ ver) -> ver)) -}
                            {- $ filter Constraint.isEQPrerelease -}
                           {- <$> constraintLists -}
        {- filteredVersions    = (\(requiredReleases, availableReleases) -> filter -}
                                {- (\x -> -}
                                    {- not (Version.isPrerelease x) -}
                                    {- || x `elem` requiredReleases) -}
                                {- availableReleases) -}
                           {- <$> zip requiredPrereleases (Map.elems versions) -}
{-  -}
    {- -- Make a symbol for each package name -}
    {- packageSyms <- sequence $ mkSymbolicSVersion . convert <$> packageNames -}
{-  -}
    {- -- Restrict symbols by version bounds -}
    {- let nameSymConstraints       = zip3 packageNames packageSyms constraintLists -}
        {- triple (name, sym, vers) = (\x -> (name, sym, x)) <$> vers -}
        {- triples                  = concat $ triple <$> nameSymConstraints -}
        {- nameConstraintPairs      = (\(name, sym, ver) -> -}
                                    {- ( convert $ name <> " " <> prettyShow ver -}
                                    {- , makeRestriction (sym, ver) )) -}
                                {- <$> triples -}
{-  -}
    {- sequence_ $ uncurry genNamedConstraint <$> nameConstraintPairs -}
{-  -}
    {- -- Restrict symbols by available versions -}
    {- let mkEqs (pkg, ver)   = (\x -> pkg .== versionToSVersion x) <$> ver -}
        {- packageEqualities  = mkEqs <$> zip packageSyms filteredVersions -}
        {- packageDisjunction = SBV.bOr <$> packageEqualities -}
        {- genPackageName nm  = "Available" <> convert nm -}
        {- namedDisjunctions  = zip (genPackageName <$> packageNames) -}
                                 {- packageDisjunction -}
{-  -}
    {- sequence_ $ uncurry genNamedConstraint <$> namedDisjunctions -}
{-  -}
    {- -- TODO [Ara] Ensure the solver gets the maximum of each Package version -}
{-  -}
    {- -- Set solver options prior to calling `checkSat` -}
    {- SBV.setOption $ SBV.ProduceUnsatCores True -}
{-  -}
    {- -- Extract the results -}
    {- SBV.query $ SBV.checkSat >>= \case -}
        {- SBV.Unsat -> do -}
            {- core <- SBV.getUnsatCore -}
            {- pure . (Left . UnsatisfiableConstraints) $ convert <$> core -}
        {- SBV.Unk   -> do -}
            {- reason <- SBV.getUnknownReason -}
            {- pure . (Left . UnknownSolution) $ convert reason -}
        {- SBV.Sat   -> do -}
            {- concreteVersions <- sequence $ extractSVersion <$> packageSyms -}
            {- pure . (Right . Map.fromList) $ zip packageNames concreteVersions -}

