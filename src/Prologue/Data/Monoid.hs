{-# LANGUAGE TypeFamilies #-}

module Prologue.Data.Monoid (module Prologue.Data.Monoid, module X) where

import GHC.Prim    (Constraint)
import Data.Monoid as X (Monoid, mappend, mconcat, mempty, (<>))

type family Monoids lst :: Constraint where
    Monoids '[]       = ()
    Monoids (a ': as) = (Monoid a, Monoids as)
