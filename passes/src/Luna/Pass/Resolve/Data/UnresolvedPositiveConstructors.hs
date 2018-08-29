{-# LANGUAGE NoStrict     #-}
{-# LANGUAGE NoStrictData #-}

module Luna.Pass.Resolve.Data.UnresolvedPositiveConstructors where

import Prologue

import qualified Luna.IR        as IR
import qualified Luna.Pass.Attr as Attr



--------------------------------------------
-- === UnresolvedPositiveConstructors === --
--------------------------------------------

-- === Definition === --

newtype UnresolvedPositiveConstructors
      = UnresolvedPositiveConstructors [IR.Term IR.Cons]
makeLenses ''UnresolvedPositiveConstructors


-- === Instances === --

type instance Attr.Type UnresolvedPositiveConstructors = Attr.Atomic
instance Default UnresolvedPositiveConstructors where
    def = UnresolvedPositiveConstructors def

