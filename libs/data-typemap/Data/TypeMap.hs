module Data.TypeMap where

import Prologue




newtype TypeMap (ks :: [*]) = TypeMap (LstToRTuple ks)


type family LstToRTuple lst where
    LstToRTuple '[] = ()
    LstToRTuple (a ': as) = (a, LstToRTuple as)


instance ks ~ '[] => Mempty (TypeMap ks) where
    mempty = TypeMap () ; {-# INLINE mempty #-}
