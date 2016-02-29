{-# LANGUAGE UndecidableInstances #-}

module Luna.Data.Name.Class where

import Prelude.Luna
import FastString


-- === Definitions === --

type Name = FastString

--type family Name a
class HasName a where name :: Lens' a Name


-- === FastString Instances === --

-- Monoid
instance Monoid FastString where
    mempty      = ""             ; {-# INLINE mempty  #-}
    mappend a b = concatFS [a,b] ; {-# INLINE mappend #-}

-- Construction
instance IsString FastString where fromString = convert ; {-# INLINE fromString #-}
instance ToString FastString where toString   = convert ; {-# INLINE toString   #-}

-- Repr
instance Repr s String => Repr s FastString where repr = repr ∘ toString

-- Conversions
instance Convertible FastString String     where convert = unpackFS ; {-# INLINE convert #-}
instance Convertible String     FastString where convert = fsLit    ; {-# INLINE convert #-}
