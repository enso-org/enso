{-# LANGUAGE UndecidableInstances #-}

module Data.TypeVal (module Data.TypeVal, module X) where

import Prologue
import Data.Reflection
import Data.Typeable as X



-- === Reflection utils === --

reflect' :: forall s a. Reifies s a => a
reflect' = reflect (Proxy :: Proxy s) ; {-# INLINE reflect' #-}


-- === TypeVal === --

newtype TypeVal = TypeVal TypeRep deriving (Show, Eq, Ord)
instance IsTypeRep TypeVal
makeWrapped ''TypeVal


-- === Reifying === --

data TypeRef   s
type TypeProxy s = Proxy (TypeRef s)
type TypeReify s = Reifies s TypeVal

reproxyTypeRef :: Proxy s -> TypeProxy s
reproxyTypeRef _ = Proxy ; {-# INLINE reproxyTypeRef #-}

reifyKnownType :: forall r. (forall s. TypeReify s => TypeProxy s -> r) -> TypeVal -> r
reifyKnownType f a = reify a $ f . reproxyTypeRef ; {-# INLINE reifyKnownType #-}


-- === KnownType === --

class KnownType a where typeVal' :: TypeVal

instance {-# OVERLAPPABLE #-}
         Typeable  a => KnownType a           where typeVal' = typeRep' @a ; {-# INLINE typeVal' #-}
instance TypeReify s => KnownType (TypeRef s) where typeVal' = reflect' @s ; {-# INLINE typeVal' #-}

typeVal :: forall a proxy. KnownType a => proxy a -> TypeVal
typeVal _ = typeVal' @a ; {-# INLINE typeVal #-}
