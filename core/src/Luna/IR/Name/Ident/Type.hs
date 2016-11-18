module Luna.IR.Name.Ident.Type where

import Prelude.Luna


-------------------------
-- === Ident types === --
-------------------------

type family IdentType a

data Var  = Var  deriving (Show, Eq, Ord)
data Type = Type deriving (Show, Eq, Ord)

