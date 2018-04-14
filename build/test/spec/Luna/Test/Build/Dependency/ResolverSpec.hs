module Luna.Test.Build.Dependency.ResolverSpec where

import Prologue hiding (Constraint, Constraints)

import qualified Data.Map.Strict as Map

import Luna.Build.Dependency.Resolver
import Luna.Build.Dependency.Version
import Luna.Build.Dependency.Constraint as Constraint

import Test.Hspec (Spec, Expectation, it, describe, shouldBe)

shouldSolveAs :: Constraints -> Versions -> PackageSet -> Expectation
shouldSolveAs constraints versions set = do
    solverResult <- solveConstraints constraints versions
    solverResult `shouldBe` (Right set)

shouldNotSolveAs :: Constraints -> Versions -> SolverError -> Expectation
shouldNotSolveAs constraints versions failure = do
    solverResult <- solveConstraints constraints versions
    solverResult `shouldBe` (Left failure)

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
missingPackages = Map.fromList
    [ ("Foo",
        [ Version 1 0 0 (Just (Prerelease Beta 3))
        , Version 1 3 2 Nothing ])
    , ("Baz",
        [ Version 1 3 0 Nothing
        , Version 2 0 0 (Just (Prerelease RC 1)) ]) ]

-- TODO [Ara] These tests will be significantly fleshed out once I implement the
-- optimisation goal.
-- FIXME [Ara] These tests are VERY brittle while the optimisation goal is not
-- in place.
spec :: Spec
spec = do
    describe "Unavailable packages" $ do
        it "No available releases for `Bar`"
            $ shouldNotSolveAs basicConstraints missingPackages
            $ UnavailablePackages ["Bar"]

    describe "Satisfiable package sets" $ do
        it "Basic package set" $ shouldSolveAs basicConstraints basicVersions
            $ Map.fromList [ ("Bar", Version 1 6 0 Nothing)
                         , ("Baz", Version 2 0 0 (Just (Prerelease RC 1)))
                         , ("Foo", Version 1 0 0 (Just (Prerelease Beta 3))) ]

    describe "Unsatisfiable package sets" $ do
        it "Basic unsatisfiable set"
            $ shouldNotSolveAs unsatConstraints basicVersions
            $ UnsatisfiableConstraints ["Foo == 1.0.0-beta.3", "Foo == 1.3.1"]

