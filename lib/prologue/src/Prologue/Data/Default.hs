{-# LANGUAGE TypeFamilies #-}

module Prologue.Data.Default (module Prologue.Data.Default, module X) where

import GHC.Exts (Constraint)
import Data.Default as X

type family Defaults lst :: Constraint where
    Defaults '[]       = ()
    Defaults (a ': as) = (Default a, Defaults as)

