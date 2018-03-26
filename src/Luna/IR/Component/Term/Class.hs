{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Component.Term.Class where

import Prologue

import qualified Data.Tag              as Tag
import qualified OCI.IR.Component      as Component
import qualified OCI.IR.Layer.Internal as Layer
import qualified OCI.IR.Layout         as Layout


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
Tag.family "TermCons"

type SomeTerm = Term ()

type family TagToCons tag = (cons :: Type -> Type) | cons -> tag
type family ConsToTag (cons :: Type -> Type) = tag | tag  -> cons
type TagConsInvariant tag cons =
    ( cons ~ TagToCons tag
    , tag  ~ ConsToTag cons
    )

