module Luna.Pass.Sourcing.Data.ModulesMap where

import Prologue

import qualified Data.Map       as Map
import qualified Luna.IR        as IR
import qualified Luna.Pass.Attr as Attr

newtype ModulesMap = ModulesMap (Map.Map IR.Qualified (IR.Term IR.Unit))
type instance Attr.Type ModulesMap = Attr.Atomic
instance Default ModulesMap where
    def = ModulesMap def

makeLenses ''ModulesMap
