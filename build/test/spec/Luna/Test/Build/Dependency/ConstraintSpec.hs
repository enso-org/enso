module Luna.Test.Build.Dependency.ConstraintSpec where

import Luna.Build.Dependency.Constraint
import Luna.Build.Dependency.Version

import Luna.Test.Build.Dependency.ParserTestUtils

import Prologue

import Test.Hspec

spec :: Spec
spec = do
    describe "Parsing of constraint types" $ do
        it "equal"            $ shouldParseTo "==" operator ConstraintEQ
        it "greater-than"     $ shouldParseTo ">"  operator ConstraintGT
        it "less-than"        $ shouldParseTo "<"  operator ConstraintLT
        it "greater-or-equal" $ shouldParseTo ">=" operator ConstraintGE
        it "less-or-equal"    $ shouldParseTo "<=" operator ConstraintLE

    describe "Parsing of Single Constraints" $ do
        it "basic equal constraint" $ shouldParseTo "== 1.0.0" constraint
            (Constraint ConstraintEQ (Version 1 0 0 Nothing))
        it "basic constraint with no spacing" $ shouldParseTo "==2.7.1"
            constraint (Constraint ConstraintEQ (Version 2 7 1 Nothing))
        it "basic constraint with lots of spacing" $ shouldParseTo ">=    8.0"
            constraint (Constraint ConstraintGE (Version 8 0 0 Nothing))
        it "invalid constraint" $ constraint `shouldFailToParse` "=="

    describe "Parsing conjunctions of constraints" $ do
        it "no constraint" $ shouldParseTo "" constraints []
        it "conjunction" $ shouldParseTo ">= 1 && < 2.0" constraints
            [ (Constraint ConstraintGE (Version 1 0 0 Nothing))
            , (Constraint ConstraintLT (Version 2 0 0 Nothing)) ]
        it "multiple conjunction" $ shouldParseTo "> 1.3 && < 3.0 && == 2.3"
            constraints
            [ (Constraint ConstraintGT (Version 1 3 0 Nothing))
            , (Constraint ConstraintLT (Version 3 0 0 Nothing))
            , (Constraint ConstraintEQ (Version 2 3 0 Nothing)) ]

