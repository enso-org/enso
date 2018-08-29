{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Luna.Pass.Basic where

import qualified Data.Graph.Data.Graph.Class     as Graph
import qualified Luna.IR                         as IR
import qualified OCI.Pass.Definition.Declaration as Pass
import qualified OCI.Pass.State.Cache            as Pass

import Luna.IR.Term.Core ()


-------------------------------
-- === Compilation stage === --
-------------------------------

data Compilation

type instance Graph.Components      Compilation          = '[IR.Terms, IR.Links]
type instance Graph.ComponentLayers Compilation IR.Links = '[IR.Target, IR.Source]
type instance Graph.ComponentLayers Compilation IR.Terms
   = '[IR.Users, IR.Model, IR.Type]



--------------------------------------
-- === Basic pass configuration === --
--------------------------------------

data BasicPass
type instance Pass.Spec BasicPass t = BasicPassSpec t
type family   BasicPassSpec t where
    BasicPassSpec (Pass.In Pass.Elems) = '[IR.Terms, IR.Links]
    BasicPassSpec (Pass.In IR.Terms)   = '[IR.Model, IR.Type, IR.Users]
    BasicPassSpec (Pass.In IR.Links)   = '[IR.Source, IR.Target]
    BasicPassSpec (Pass.In Pass.Attrs) = '[]
    BasicPassSpec (Pass.Out a)         = BasicPassSpec (Pass.In a)

