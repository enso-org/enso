{-# LANGUAGE NoStrict     #-}
{-# LANGUAGE NoStrictData #-}

module Luna.Pass.Resolve.Data.UnresolvedVariables where

import Prologue

import qualified Luna.IR        as IR
import qualified Luna.Pass.Attr as Attr



---------------------------------
-- === UnresolvedVariables === --
---------------------------------

-- === Definition === --

newtype UnresolvedVariables = UnresolvedVariables [IR.Term IR.Var]
makeLenses ''UnresolvedVariables


-- === Instances === --

type instance Attr.Type UnresolvedVariables = Attr.Atomic
instance Default UnresolvedVariables where
    def = UnresolvedVariables def

