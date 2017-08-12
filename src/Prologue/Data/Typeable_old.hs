{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Prologue.Data.Typeable_old where

import Prelude
import Data.Kind
import Control.Lens.Utils
import Data.Convert
import Data.Typeable


----------------------
-- === Typeable === --
----------------------

typeRep'_ :: forall a t. Typeable a => TypeRep
typeRep'_ = typeRep (Proxy :: Proxy a) ; {-# INLINE typeRep'_ #-}

typeRep' :: forall a t. (Typeable a, FromTypeRep t) => t
typeRep' = convert' $ typeRep'_ @a ; {-# INLINE typeRep' #-}

typeReps' :: forall (ls :: [*]) t. (Typeables ls, FromTypeRep t) => [t]
typeReps' = convert' <$> typeReps'_ @ls ; {-# INLINE typeReps' #-}

class                                  Typeables (ls :: [*]) where typeReps'_ :: [TypeRep]
instance (Typeable l, Typeables ls) => Typeables (l ': ls)   where typeReps'_ = typeRep' @l : typeReps' @ls ; {-# INLINE typeReps'_ #-}
instance                               Typeables '[]         where typeReps'_ = []                          ; {-# INLINE typeReps'_ #-}


type FromTypeRep a = Convertible' TypeRep a
type ToTypeRep   a = Convertible' a TypeRep

type IsTypeRep a = BiConvertible' TypeRep a

asTypeRep :: IsTypeRep a => Iso' a TypeRep
asTypeRep = converted'



instance Convertible TypeRep String where convert = show


switchedRep :: (IsTypeRep a, IsTypeRep b) => Lens' a b
switchedRep = asTypeRep . from asTypeRep ; {-# INLINE switchedRep #-}

switchRep :: (IsTypeRep a, IsTypeRep b) => a -> b
switchRep = view switchedRep ; {-# INLINE switchRep #-}
