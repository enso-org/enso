module Old.Luna.Compilation.Error where

import Luna.Prelude

data TCError n = UnificationError n
               | ImportError (Maybe n) String
               deriving (Show, Eq)

instance Castable n n' => Castable (TCError n) (TCError n') where
    cast (UnificationError n) = UnificationError (cast n)
    cast (ImportError n s)        = ImportError (cast <$> n) s
