{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Component.Term.Class where

import Prologue

import qualified Data.Tag         as Tag
import qualified OCI.IR.Component as Component
import qualified OCI.IR.Layer     as Layer
import qualified OCI.IR.Layout    as Layout


---------------------
-- === UniTerm === --
---------------------

-- | The implementation of Uni is delayed until we know
--   all possible Term constructors.
type family Uni :: Type -> Type

class IsUni t where
    toUni :: ∀ a. t a -> Uni a



------------------
-- === Term === --
------------------

-- === Definition === --

Component.define "Term"
Tag.family "TermTag"

type SomeTerm = Term ()


-- === Term Constructor === --

data family Constructor (term :: Type) (layout :: Type)

type family TagToCons t where
    TagToCons t = Constructor t

type family ConsToTag a where
    ConsToTag (Constructor t) = t


-- === Discovery === --

-- | IsTermTag is used to gather all IR terms when generating UniTerm in TH.
class IsTermTag (t :: Type)


-- === Instances === --

type instance Layout.Merge (TermTag a) (TermTag b) = Merge__ a b
type family Merge__ a b where
    Merge__ a a = TermTag a
    -- Merge__ a b = -- TODO: when needed
