{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Model.Network.Graph (module Luna.Syntax.Model.Network.Graph) where -- , module X) where

import Prelude.Luna

import           Luna.Evaluation.Runtime        as Runtime
import           Luna.Syntax.AST.Term           (Input)
import           Luna.Syntax.Model.Layer        ((:<:))
import           Data.Graph

-- === Instances === --
-- All the instances below implement Network relations on general Graph type.
-- They should not be included in the Graph-like modules, because Graphs should not know
-- about these functionalities. Although these are orphans, they are imported by the most basic
-- modules of Network implementation.

type instance Input (Ref Node a) = Ref Node a

type instance Runtime.Model (Ref Node a) = Runtime.Model (Unlayered (Ref Node a))
type instance Runtime.Model (ls :<: a) = Runtime.Model (Uncovered (ls :<: a))
