module Luna.Compilation.Error where

import Prelude.Luna

data TCError n = UnificationError n n
               | ImportError n String
               deriving (Show, Eq)

instance Castable n n' => Castable (TCError n) (TCError n') where
    cast (UnificationError n1 n2) = UnificationError (cast n1) (cast n2)
    cast (ImportError n s)        = ImportError (cast n) s
