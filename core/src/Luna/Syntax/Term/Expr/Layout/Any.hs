module Luna.Syntax.Term.Expr.Layout.Any where

import Prelude.Luna

import Control.Lens.Property
import Luna.Syntax.Term.Expr.Atom
import Luna.Syntax.Term.Expr.Format
import Luna.Syntax.Term.Expr.Layout.Class


------------------------
-- === Any layout === --
------------------------

-- === Definition === --

data Any = Any deriving (Show)


-- === Instances === --

type instance Get Atom Any = Any
type instance Atoms Any    = '[Star]
type instance Sub t Any    = Any
