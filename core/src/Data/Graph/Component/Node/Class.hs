{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Component.Node.Class where

import Prologue

import qualified Data.Graph.Data              as Component
import qualified Data.Graph.Data.Layer.Layout as Layout
import qualified Data.Tag                     as Tag
import qualified Type.Show                    as Type



------------------
-- === Node === --
------------------

-- === Definition === --

Component.define "Node"
Tag.family "NodeTag"
type Some = Component.Some Nodes


-- === Node Constructor === --

data family Constructor (tag :: Type) (layout :: Type)

-- FIXME: Remove
type family TagToCons t where
    TagToCons t = Constructor t

-- FIXME: Rename to ConstructorTag
type family ConsToTag a where
    ConsToTag (Constructor t) = t


-- === Instances === --

type instance Layout.Default Nodes = ()

type instance Layout.Merge (NodeTag a) (NodeTag b) = Merge__ a b
type family Merge__ a b where
    Merge__ a a = NodeTag a
    -- Merge__ a b = -- TODO: when needed


-- instance GTraversable (Component.Provider tag m) (Constructor t a)
--     => Component.Provider tag m (Constructor t a) where
--     gather = Component.ggather @tag  ; {-# INLINE gather #-}

-- instance GTraversable Component.DynamicProvider (Constructor t a)
--     => Component.DynamicProvider (Constructor t a) where
--     dynamicComponentsIO = Component.gdynamicComponents  ; {-# INLINE dynamicComponentsIO #-}



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



---------------------------

---------------------
-- === UniTerm === --
---------------------

-- | The implementation of Uni is delayed until we know
--   all possible Term constructors.
type family Uni :: Type -> Type

class IsUni t where
    toUni :: ∀ a. t a -> Uni a



-- === Discovery === --

-- | IsTermTag is used to gather all IR terms when generating UniNode in TH.
class IsTermTag (t :: Type)
