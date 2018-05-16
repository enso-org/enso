{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Luna.Pass.Basic where

import qualified OCI.Pass.State.Cache as Pass

import OCI.Pass.Definition.Declaration as X (Attrs, Elems, In, Out, Spec)

import Luna.IR
import Luna.IR.Term.Core ()



--------------------------------------
-- === Basic pass configuration === --
--------------------------------------

data BasicPass
type instance Spec BasicPass t = BasicPassSpec t
type family   BasicPassSpec  t where
    BasicPassSpec (In Elems) = '[Terms, Links]
    BasicPassSpec (In Terms) = '[Model, Type, Users]
    BasicPassSpec (In Links) = '[Source, Target]
    BasicPassSpec (In Attrs) = '[]
    BasicPassSpec (Out a)    = BasicPassSpec (In a)
    BasicPassSpec t          = '[]
