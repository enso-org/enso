module Luna.Pass.Typing.Data.Target where

import Prologue

import qualified Luna.IR        as IR
import qualified Luna.Pass.Attr as Attr

data Target = Function IR.Qualified IR.Name
            | Method   IR.Qualified IR.Name IR.Name
            | Unknown
            deriving (Show, Eq)

type instance Attr.Type Target = Attr.Atomic
instance Default Target where
    def = Unknown
