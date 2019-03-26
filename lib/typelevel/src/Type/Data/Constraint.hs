module Type.Data.Constraint (module Type.Data.Constraint, module X) where

import Data.Constraint as X

type family Constraints (cs :: [Constraint]) :: Constraint where
    Constraints '[]       = ()
    Constraints (c ': cs) = (c , Constraints cs)

