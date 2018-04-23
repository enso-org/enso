{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Component.Term.Class where

import Prologue

import qualified Data.Tag         as Tag
import qualified OCI.IR.Component as Component
import qualified OCI.IR.Layout    as Layout
import qualified Type.Show        as Type



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

data family Constructor (tag :: Type) (layout :: Type)

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



---------------------
-- === TagShow === --
---------------------

data TagOnly = TagOnly
type ShowTag = StyledShow TagOnly
type instance StyledShowOutput TagOnly = Text

instance (Tag.Tag fam name ~ tag, Type.Show name)
    => StyledShow TagOnly (Constructor tag layout) where
    styledShow _ _ = convert $ Type.show @name

showTag :: ShowTag a => a -> Text
showTag = styledShow TagOnly ; {-# INLINE showTag #-}
