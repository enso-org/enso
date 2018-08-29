{-# LANGUAGE NoStrict     #-}
{-# LANGUAGE NoStrictData #-}

module Luna.Pass.Resolve.Data.UnresolvedNegativeConstructors where

import Prologue

import qualified Luna.IR        as IR
import qualified Luna.Pass.Attr as Attr



--------------------------------------------
-- === UnresolvedNegativeConstructors === --
--------------------------------------------

-- === Definition === --

newtype UnresolvedNegativeConstructors
      = UnresolvedNegativeConstructors [IR.Term IR.Cons]
makeLenses ''UnresolvedNegativeConstructors


-- === Instances === --

type instance Attr.Type UnresolvedNegativeConstructors = Attr.Atomic
instance Default UnresolvedNegativeConstructors where
    def = UnresolvedNegativeConstructors def

