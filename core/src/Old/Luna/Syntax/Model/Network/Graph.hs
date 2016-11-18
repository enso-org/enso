{-# LANGUAGE UndecidableInstances #-}

module Old.Luna.Syntax.Model.Network.Graph (module Old.Luna.Syntax.Model.Network.Graph) where -- , module X) where

import Prelude.Luna

import           Luna.Runtime.Dynamics        (Dynamics_OLD)
import           Old.Luna.Syntax.Term.Class           (Input)
import           Old.Luna.Syntax.Model.Layer        ((:<:))
import           Data.Graph
import           Data.Layer_OLD.Cover_OLD


-- === Instances === --
-- All the instances below implement Network relations on general Graph type.
-- They should not be included in the Graph-like modules, because Graphs should not know
-- about these functionalities. Although these are orphans, they are imported by the most basic
-- modules of Network implementation.

type instance Input (Ref Node a) = Ref Node a

type instance Dynamics_OLD (Ref Node a) = Dynamics_OLD (Unlayered (Ref Node a))
type instance Dynamics_OLD (ls :<: a)   = Dynamics_OLD (Uncovered (ls :<: a))
