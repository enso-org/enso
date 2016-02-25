{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Name.Class where

import Prelude.Luna
import FastString


-- === Definitions === --

type NameBase = FastString

type family Name a
class    HasName a where name :: Lens' a (Name a)

class NameGenerator m name where
    newName :: m name



-- === Instances === --

-- Construction
instance IsString NameBase where fromString = fsLit    ; {-# INLINE fromString #-}
instance ToString NameBase where toString   = unpackFS ; {-# INLINE toString   #-}

-- Repr
instance Repr s String => Repr s NameBase where repr = repr ∘ toString
