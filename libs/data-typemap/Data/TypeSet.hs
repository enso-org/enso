{-# LANGUAGE Strict #-}

module Data.TypeSet where

import Prologue



---------------------
-- === TypeSet === --
---------------------

-- === Definition === --

newtype TypeSet (ks :: [*]) = TypeSet (LstToRTuple ks)

type family LstToRTuple lst where
    LstToRTuple '[] = ()
    LstToRTuple (a ': as) = (a, LstToRTuple as)



-- === Construction === --

instance ks ~ '[] => Mempty (TypeSet ks) where
    mempty = TypeSet () ; {-# INLINE mempty #-}


-- === Modification === --
