module Luna.Test.Build.Dependency.ResolverSpec where

import Luna.Build.Dependency.Resolver
import Luna.Build.Dependency.Version
import Luna.Build.Dependency.Constraint

import Prologue hiding (Constraint, Constraints)

import qualified Data.Map.Strict as M (fromList)

import Test.Hspec

shouldSolve :: Constraints -> Versions -> Expectation
shouldSolve = undefined

shouldNotSolve :: Constraints -> Versions -> Expectation
shouldNotSolve = undefined

shouldSolveAs :: Constraints -> Versions -> PackageSet -> Expectation
shouldSolveAs = undefined

testConstraints :: Constraints
testConstraints = M.fromList
    [ ("foo",
        [ Constraint ConstraintLE (Version 1 3 1 Nothing)
        , Constraint ConstraintEQ (Version 1 0 0 (Just (Prerelease Beta 3))) ])
    , ("bar", [ Constraint ConstraintGT (Version 1 3 0 Nothing) ])
    , ("baz",
        [ Constraint ConstraintEQ (Version 2 0 0 (Just (Prerelease RC 1)))
        , Constraint ConstraintGT (Version 1 3 2 Nothing) ]) ]

testVersions :: Versions
testVersions = M.fromList
    [ ("foo",
        [ Version 1 0 0 Nothing
        , Version 1 0 1 Nothing
        , Version 1 1 0 (Just (Prerelease Alpha 1))
        , Version 1 1 0 Nothing
        , Version 1 2 0 Nothing
        , Version 1 2 5 Nothing
        , Version 1 3 2 Nothing ])
    , ("bar",
        [ Version 1 1 0 Nothing
        , Version 1 2 0 Nothing
        , Version 1 3 0 Nothing
        , Version 1 4 0 Nothing ])
    , ("baz",
        [ Version 1 3 0 Nothing
        , Version 1 4 0 Nothing
        , Version 1 5 0 Nothing
        , Version 1 7 0 Nothing
        , Version 2 0 0 (Just (Prerelease Alpha 1))
        , Version 2 0 0 (Just (Prerelease RC 1)) ]) ]

spec :: Spec
spec = do
    describe "testing" $ do
        it "test" $ do
            result <- solveConstraints testConstraints testVersions
            result `shouldBe` (Right 1)

