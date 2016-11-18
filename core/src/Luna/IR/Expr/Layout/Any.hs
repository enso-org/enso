module Luna.IR.Expr.Layout.Any where

import Prelude.Luna

import Data.Property
import Luna.IR.Expr.Atom
import Luna.IR.Expr.Format
import Luna.IR.Expr.Layout.Class


------------------------
-- === Any layout === --
------------------------

-- === Definition === --

data Any = Any deriving (Show)


-- === Instances === --

type instance Access Atom Any = Any
type instance Atoms Any       = '[Star]
type instance Sub t Any       = Any
