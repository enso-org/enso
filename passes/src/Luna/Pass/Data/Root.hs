module Luna.Pass.Data.Root where

import Prologue

import qualified Luna.IR        as IR
import qualified Luna.Pass.Attr as Attr

import Data.Graph.Data.Component.Class (unsafeNull)

newtype Root = Root (IR.SomeTerm)
type instance Attr.Type Root = Attr.Atomic
instance Default Root where
    def = Root unsafeNull

makeLenses ''Root
