{-# LANGUAGE UndecidableInstances #-}

module Luna.Data.Name.Class where

import Prelude.Luna
import FastString


-- === Definitions === --

type Name = FastString

--type family Name a
class HasName a where name :: Lens' a Name


-- === Instances === --

-- Construction
instance IsString Name where fromString = fsLit    ; {-# INLINE fromString #-}
instance ToString Name where toString   = unpackFS ; {-# INLINE toString   #-}

-- Repr
instance Repr s String => Repr s Name where repr = repr ∘ toString
