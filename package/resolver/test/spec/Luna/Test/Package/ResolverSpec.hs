module Luna.Test.Package.ResolverSpec where

import Prologue hiding (Constraint, Constraints, set)

import qualified Data.Map.Strict         as Map
import qualified Luna.Package.Constraint as Constraint

import Luna.Package.Constraint ( Constraint(Constraint)
                               , Constraints
                               , Versions
                               , PackageSet )
import Luna.Package.Resolver   ( solveConstraints, SolverError(..))
import Luna.Package.Version    ( Version(Version)
                               , Prerelease(Prerelease)
                               , PrereleaseType(Alpha, Beta, RC) )

import Test.Hspec (Spec, Expectation, it, describe, shouldBe)

shouldSolveAs :: Constraints -> Versions -> PackageSet -> Expectation
shouldSolveAs constraints versions set = do
    solverResult <- solveConstraints constraints versions
    solverResult `shouldBe` Right set

shouldNotSolveAs :: Constraints -> Versions -> SolverError -> Expectation
shouldNotSolveAs constraints versions failure = do
    solverResult <- solveConstraints constraints versions
    solverResult `shouldBe` Left failure

basicConstraints :: Constraints
basicConstraints = Map.fromList
    [ ("Foo",
        [ Constraint Constraint.LT (Version 1 3 1 Nothing)
        , Constraint Constraint.EQ (Version 1 0 0 (Just (Prerelease Beta 3))) ])
    , ("Bar", [ Constraint Constraint.GT (Version 1 3 0 Nothing) ])
    , ("Baz",
        [ Constraint Constraint.EQ (Version 2 0 0 (Just (Prerelease RC 1)))
        , Constraint Constraint.GT (Version 1 3 2 Nothing) ]) ]

unsatConstraints :: Constraints
unsatConstraints = Map.fromList
    [ ("Foo",
        [ Constraint Constraint.EQ (Version 1 3 1 Nothing)
        , Constraint Constraint.EQ (Version 1 0 0 (Just (Prerelease Beta 3))) ])
    , ("Bar", [ Constraint Constraint.GT (Version 1 3 0 Nothing) ])
    , ("Baz",
        [ Constraint Constraint.LT (Version 2 0 0 Nothing)
        , Constraint Constraint.GT (Version 1 3 2 Nothing) ]) ]

packageWithNoConstraints :: Constraints
packageWithNoConstraints = Map.fromList
    [ ("Foo", [])
    , ("Bar", [Constraint Constraint.GT (Version 1 1 0 Nothing)])
    , ("Baz", [Constraint Constraint.EQ (Version 1 3 0 Nothing)]) ]

noExplicitPrereleases :: Constraints
noExplicitPrereleases = Map.fromList
    [ ("Foo", [Constraint Constraint.LT (Version 1 1 0 Nothing)])
    , ("Bar", [Constraint Constraint.LE (Version 1 5 0 Nothing)])
    , ("Baz", [Constraint Constraint.GE (Version 1 3 0 Nothing)]) ]

hardEQConstraints :: Constraints
hardEQConstraints = Map.fromList
    [ ("Foo", [Constraint Constraint.EQ (Version 1 2 5 Nothing)])
    , ("Bar", [Constraint Constraint.EQ (Version 1 0 0 Nothing)])
    , ("Baz", [Constraint Constraint.EQ (Version 1 4 0 Nothing)]) ]

lessConstraintsThanVersions :: Constraints
lessConstraintsThanVersions = Map.fromList
    [ ("Foo", [Constraint Constraint.GT (Version 1 0 0 Nothing)])
    , ("Bar", [Constraint Constraint.GT (Version 1 1 0 Nothing)]) ]

basicVersions :: Versions
basicVersions = Map.fromList
    [ ("Foo",
        [ Version 1 0 0 (Just (Prerelease Beta 3))
        , Version 1 0 0 Nothing
        , Version 1 0 1 Nothing
        , Version 1 1 0 (Just (Prerelease Alpha 1))
        , Version 1 1 0 Nothing
        , Version 1 2 0 Nothing
        , Version 1 2 5 Nothing
        , Version 1 3 2 Nothing ])
    , ("Bar",
        [ Version 1 1 0 Nothing
        , Version 1 2 0 Nothing
        , Version 1 3 0 Nothing
        , Version 1 4 0 Nothing
        , Version 1 5 0 Nothing
        , Version 1 6 0 Nothing ])
    , ("Baz",
        [ Version 1 3 0 Nothing
        , Version 1 4 0 Nothing
        , Version 1 5 0 Nothing
        , Version 1 7 0 Nothing
        , Version 2 0 0 (Just (Prerelease Alpha 1))
        , Version 2 0 0 (Just (Prerelease RC 1)) ]) ]

missingPackages :: Versions
missingPackages = Map.fromList [ ("Foo",
        [ Version 1 0 0 (Just (Prerelease Beta 3))
        , Version 1 3 2 Nothing ])
    , ("Baz",
        [ Version 1 3 0 Nothing
        , Version 2 0 0 (Just (Prerelease RC 1)) ]) ]

missingAllPackages :: Versions
missingAllPackages = Map.fromList []

spec :: Spec
spec = do
    describe "Unavailable packages" $ do
        it "No available releases for `Bar`"
            . shouldNotSolveAs basicConstraints missingPackages
            $ UnavailablePackages ["Bar"]
        it "No available releases at all"
            . shouldNotSolveAs basicConstraints missingAllPackages
            $ UnavailablePackages ["Bar", "Baz", "Foo"]
        it "Less constraints than versions"
            . shouldNotSolveAs lessConstraintsThanVersions basicVersions
            $ PackagesNotProvided ["Baz"]

    describe "Satisfiable package sets" $ do
        it "Basic package set" . shouldSolveAs basicConstraints basicVersions
            $ Map.fromList [ ("Bar", Version 1 6 0 Nothing)
                           , ("Baz", Version 2 0 0 (Just (Prerelease RC 1)))
                           , ("Foo", Version 1 0 0 (Just (Prerelease Beta 3))) ]
        it "Set without constraints for a package"
            . shouldSolveAs packageWithNoConstraints basicVersions
            $ Map.fromList [ ("Bar", Version 1 6 0 Nothing)
                           , ("Baz", Version 1 3 0 Nothing)
                           , ("Foo", Version 1 3 2 Nothing) ]
        it "Should not select prereleases by default"
            . shouldSolveAs noExplicitPrereleases basicVersions
            $ Map.fromList [ ("Bar", Version 1 5 0 Nothing)
                           , ("Baz", Version 1 7 0 Nothing)
                           , ("Foo", Version 1 0 1 Nothing) ]

    describe "Unsatisfiable package sets" $ do
        it "Basic unsatisfiable set"
            . shouldNotSolveAs unsatConstraints basicVersions
            $ UnsatisfiableConstraints ["Foo == 1.0.0-beta.3", "Foo == 1.3.1"]
        it "EQ constraint on unavailable version"
            . shouldNotSolveAs hardEQConstraints basicVersions
            $ UnsatisfiableConstraints ["Available:Bar", "Bar == 1.0.0"]

