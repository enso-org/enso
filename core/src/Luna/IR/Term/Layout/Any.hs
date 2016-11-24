module Luna.IR.Term.Layout.Any where

import Luna.Prelude

import Data.Property
import Luna.IR.Term.Atom
import Luna.IR.Term.Format
import Luna.IR.Term.Layout.Class


------------------------
-- === Any layout === --
------------------------

-- === Definition === --

data Any = Any deriving (Show)


-- === Instances === --

type instance Access Atom Any = Any
type instance Atoms Any       = '[Star]
type instance Sub t Any       = Any

type instance Abstract  (Form a) = Any
