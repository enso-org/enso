module Luna.Test.Build.Dependency.ResolverSpec where

import Luna.Build.Dependency.Resolver
import Luna.Build.Dependency.Version
import Luna.Build.Dependency.Constraint

import Prologue hiding (Constraint, Constraints)

import qualified Data.Map.Strict as M (fromList)

import Test.Hspec

shouldSolveAs :: Constraints -> Versions -> PackageSet -> Expectation
shouldSolveAs constraints versions set = do
    solverResult <- solveConstraints constraints versions
    solverResult `shouldBe` (Right set)

shouldNotSolveAs :: Constraints -> Versions -> SolverFailure -> Expectation
shouldNotSolveAs constraints versions failure = do
    solverResult <- solveConstraints constraints versions
    let solved = case solverResult of
            Left returnedFailure -> failure /= returnedFailure
            Right _  -> True
    solved `shouldBe` False

basicConstraints :: Constraints
basicConstraints = M.fromList
    [ ("Foo",
        [ Constraint ConstraintEQ (Version 1 3 1 Nothing) -- TODO Change me back to ConstraintLT
        , Constraint ConstraintEQ (Version 1 0 0 (Just (Prerelease Beta 3))) ])
    , ("Bar", [ Constraint ConstraintGT (Version 1 3 0 Nothing) ])
    , ("Baz",
        [ Constraint ConstraintEQ (Version 2 0 0 (Just (Prerelease RC 1)))
        , Constraint ConstraintGT (Version 1 3 2 Nothing) ]) ]

basicVersions :: Versions
basicVersions = M.fromList
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
missingPackages = M.fromList
    [ ("Foo",
        [ Version 1 0 0 (Just (Prerelease Beta 3))
        , Version 1 3 2 Nothing ])
    , ("Baz",
        [ Version 1 3 0 Nothing
        , Version 2 0 0 (Just (Prerelease RC 1)) ]) ]

spec :: Spec
spec = do
    describe "Unavailable packages" $ do
        it "No available releases for `Bar`"
            $ shouldNotSolveAs basicConstraints missingPackages
            $ UnavailablePackages ["Bar"]

    describe "Satisfiable package sets" $ do
        it "Basic package set" $ shouldSolveAs basicConstraints basicVersions
            $ M.fromList [ ("Bar", Version 1 6 0 Nothing)
                         , ("Baz", Version 2 0 0 (Just (Prerelease RC 1)))
                         , ("Foo", Version 1 0 0 (Just (Prerelease Beta 3))) ]

    describe "Unsatisfiable package sets" $ do
        it "temp" $ True

