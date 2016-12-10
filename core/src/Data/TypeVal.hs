{-# LANGUAGE UndecidableInstances #-}

module Data.TypeVal (module Data.TypeVal, module X) where

import Prologue
import Data.Reflection
import Data.Typeable as X



-- === TypeVal === --

newtype TypeVal = TypeVal TypeRep deriving (Show, Eq, Ord)
instance IsTypeRep TypeVal
makeWrapped ''TypeVal


-- === Reifying === --

newtype TypeProxy s = TypeProxy (Proxy s)
type    TypeReify s = Reifies s TypeVal
makeWrapped ''TypeProxy

reifyKnownType :: forall r. (forall s. TypeReify s => TypeProxy s -> r) -> TypeVal -> r
reifyKnownType f a = reify a $ f . TypeProxy ; {-# INLINE reifyKnownType #-}


-- === KnownType === --

class KnownType a where typeVal :: a -> TypeVal

instance {-# OVERLAPPABLE #-}
         Typeable  a => KnownType a             where typeVal = const $ typeRep' @a ; {-# INLINE typeVal #-}
instance Typeable  s => KnownType (Proxy     s) where typeVal = const $ typeRep' @s ; {-# INLINE typeVal #-}
instance TypeReify s => KnownType (TypeProxy s) where typeVal = reflect . unwrap'   ; {-# INLINE typeVal #-}
instance                KnownType TypeVal       where typeVal = id                  ; {-# INLINE typeVal #-}

typeVal' :: forall a. Typeable a => TypeVal
typeVal' = typeVal (Proxy :: Proxy a) ; {-# INLINE typeVal' #-}
