module Luna.Test.Build.Dependency.ResolverSpec where

import Luna.Build.Dependency.Resolver
import Luna.Build.Dependency.Version
import Luna.Build.Dependency.Constraint

import Prologue

import qualified Data.Map.Strict as M (fromList)

import Test.Hspec

shouldSolve :: ConstraintMap -> Expectation
shouldSolve = undefined

shouldNotSolve :: ConstraintMap -> Expectation
shouldNotSolve = undefined

shouldSolveAs :: ConstraintMap -> ConstraintMap -> Expectation
shouldSolveAs = undefined

testConstraints :: ConstraintMap
testConstraints = M.fromList
    [ ("foo",
        [ Constraint ConstraintLE (Version 1 3 1 Nothing)
        , Constraint ConstraintEQ (Version 1 0 0 (Just (Prerelease Beta 3))) ])
    , ("bar", [ Constraint ConstraintGT (Version 1 3 0 Nothing) ])
    , ("baz",
        [ Constraint ConstraintEQ (Version 2 0 0 (Just (Prerelease RC 1)))
        , Constraint ConstraintGT (Version 1 3 2 Nothing) ]) ]

spec :: Spec
spec = do
    describe "testing" $ do
        it "test" $ do
            result <- solveConstraints testConstraints
            result `shouldBe` (Just 1)

