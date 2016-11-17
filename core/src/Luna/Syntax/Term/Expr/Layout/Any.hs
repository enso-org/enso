module Luna.Syntax.Term.Expr.Layout.Any where

import Prelude.Luna

import Data.Property
import Luna.Syntax.Term.Expr.Atom
import Luna.Syntax.Term.Expr.Format
import Luna.Syntax.Term.Expr.Layout.Class


------------------------
-- === Any layout === --
------------------------

-- === Definition === --

data Any = Any deriving (Show)


-- === Instances === --

type instance Access Atom Any = Any
type instance Atoms Any       = '[Star]
type instance Sub t Any       = Any
