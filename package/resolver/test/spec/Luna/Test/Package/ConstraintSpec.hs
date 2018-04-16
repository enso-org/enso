module Luna.Test.Package.ConstraintSpec where

import Prologue

import qualified Luna.Package.Constraint as Constraint

import Luna.Package.Constraint           (Constraint(Constraint))
import Luna.Package.Version              (Version(Version))
import Luna.Test.Package.ParserTestUtils ( shouldParseTo
                                         , shouldFailToParse )
import Test.Hspec                        (Spec, describe, it)

spec :: Spec
spec = do
    describe "Parsing of constraint types" $ do
        it "equal"            $ shouldParseTo "==" Constraint.operator
                                Constraint.EQ
        it "greater-than"     $ shouldParseTo ">"  Constraint.operator
                                Constraint.GT
        it "less-than"        $ shouldParseTo "<"  Constraint.operator
                                Constraint.LT
        it "greater-or-equal" $ shouldParseTo ">=" Constraint.operator
                                Constraint.GE
        it "less-or-equal"    $ shouldParseTo "<=" Constraint.operator
                                Constraint.LE

    describe "Parsing of Single Constraints" $ do
        it "basic equal constraint" $ shouldParseTo "== 1.0.0"
            Constraint.constraint
            (Constraint Constraint.EQ (Version 1 0 0 Nothing))
        it "basic constraint with no spacing" $ shouldParseTo "==2.7.1"
            Constraint.constraint
            (Constraint Constraint.EQ (Version 2 7 1 Nothing))
        it "basic constraint with lots of spacing" $ shouldParseTo ">=    8.0"
            Constraint.constraint
            (Constraint Constraint.GE (Version 8 0 0 Nothing))
        it "invalid constraint" $ Constraint.constraint `shouldFailToParse` "=="

    describe "Parsing conjunctions of constraints" $ do
        it "no constraint" $ shouldParseTo "" Constraint.constraints []
        it "conjunction" $ shouldParseTo ">= 1 && < 2.0" Constraint.constraints
            [ Constraint Constraint.GE (Version 1 0 0 Nothing)
            , Constraint Constraint.LT (Version 2 0 0 Nothing) ]
        it "multiple conjunction" $ shouldParseTo "> 1.3 && < 3.0 && == 2.3"
            Constraint.constraints
            [ Constraint Constraint.GT (Version 1 3 0 Nothing)
            , Constraint Constraint.LT (Version 3 0 0 Nothing)
            , Constraint Constraint.EQ (Version 2 3 0 Nothing) ]

