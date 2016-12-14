{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Prologue.Data.Typeable where

import Prelude
import Data.Typeable
import Data.Kind
import Control.Lens.Utils


----------------------
-- === Typeable === --
----------------------

typeRep' :: forall a t. (Typeable a, IsTypeRep t) => t
typeRep' = typeRep (Proxy :: Proxy a) ^. from asTypeRep ; {-# INLINE typeRep' #-}

typeReps' :: forall (ls :: [*]) t. (Typeables ls, IsTypeRep t) => [t]
typeReps' = view (from asTypeRep) <$> typeReps'_ @ls ; {-# INLINE typeReps' #-}

class                                  Typeables (ls :: [*]) where typeReps'_ :: [TypeRep]
instance (Typeable l, Typeables ls) => Typeables (l ': ls)   where typeReps'_ = typeRep' @l : typeReps' @ls ; {-# INLINE typeReps'_ #-}
instance                               Typeables '[]         where typeReps'_ = []                          ; {-# INLINE typeReps'_ #-}


class IsTypeRep a where
    asTypeRep :: Iso' a TypeRep
    default asTypeRep :: (Wrapped a, Unwrapped a ~ TypeRep) =>  Iso' a TypeRep
    asTypeRep = wrapped' ; {-# INLINE asTypeRep #-}

instance IsTypeRep TypeRep where asTypeRep = id ; {-# INLINE asTypeRep #-}


switchedRep :: (IsTypeRep a, IsTypeRep b) => Lens' a b
switchedRep = asTypeRep . from asTypeRep ; {-# INLINE switchedRep #-}

switchRep :: (IsTypeRep a, IsTypeRep b) => a -> b
switchRep = view switchedRep ; {-# INLINE switchRep #-}
