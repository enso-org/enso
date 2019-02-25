module Prologue.Data.Default1 where

import GHC.Exts (Constraint)

class Default1 t where
    def1 :: ∀ a. t a

type family Defaults1 lst :: Constraint where
    Defaults1 '[]       = ()
    Defaults1 (a ': as) = (Default1 a, Defaults1 as)
