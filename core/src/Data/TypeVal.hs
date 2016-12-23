{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.TypeVal (module Data.TypeVal, module X) where

import Data.Typeable.Proxy.Abbr
import Prologue hiding (Simple)
import Data.Reflection
import Data.Typeable as X
import Data.Kind

----------------------------------
-- === Reflection etensions === --
----------------------------------

newtype MagicT t a r = MagicT (forall (s :: *). Reifies (t s) a => Proxy s -> r)

reifyT :: forall t a r. a -> (forall (s :: *). Reifies (t s) a => Proxy s -> r) -> r
reifyT a k = unsafeCoerce (MagicT k :: MagicT t a r) (const a) Proxy

reflect' :: forall s a. Reifies s a => a
reflect' = reflect (p :: P s) ; {-# INLINE reflect' #-}


---------------------
-- === TypeVal === --
---------------------

-- === Reifying === --

data TypeRef   (s :: *)
type TypeProxy s = Proxy (TypeRef s)
type TypeReify s = Reifies s TypeRep

reifyKnownType :: forall r. (forall (s :: *). TypeReify s => TypeProxy s -> r) -> TypeRep -> r
reifyKnownType f a = reify a $ f . reproxyTypeRef ; {-# INLINE reifyKnownType #-}

reifyKnownTypeT :: forall t r. (forall (s :: *). TypeReify (t s) => TypeProxy s -> r) -> TypeRep -> r
reifyKnownTypeT f a = reifyT @t a $ f . reproxyTypeRef ; {-# INLINE reifyKnownTypeT #-}

reproxyTypeRef :: P s -> TypeProxy s
reproxyTypeRef _ = p ; {-# INLINE reproxyTypeRef #-}


-- === KnownType === --


-- tyConVals

tyConVals :: KnownDType a => Proxy a -> (TyCon, [TypeRep])
tyConVals = fmap reverse . tyConValsR ; {-# INLINE tyConVals #-}

class                                       KnownDType (a :: k) where tyConValsR :: Proxy a -> (TyCon, [TypeRep])
instance {-# OVERLAPPABLE #-} Typeable a => KnownDType a        where tyConValsR   = (,[]) . typeRepTyCon . typeRep            ; {-# INLINE tyConValsR #-}
instance (KnownType a, KnownDType t)     => KnownDType (t a)    where tyConValsR _ = (typeVal' @a :) <$> tyConValsR (p :: P t) ; {-# INLINE tyConValsR #-}

-- KnownType

class                                         KnownType a           where typeVal_ :: Proxy a -> TypeRep
instance {-# OVERLAPPABLE #-} KnownDType a => KnownType a           where typeVal_   = uncurry mkTyConApp . tyConVals ; {-# INLINE typeVal_ #-}
instance                      TypeReify  s => KnownType (TypeRef s) where typeVal_ _ = reflect (p :: P s)             ; {-# INLINE typeVal_ #-}

typeVal'_ :: forall a. KnownType a => TypeRep
typeVal'_ = typeVal_ (Proxy :: Proxy a) ; {-# INLINE typeVal'_ #-}

typeVal :: (KnownType a, IsTypeRep t) => Proxy a -> t
typeVal = view (from asTypeRep) . typeVal_ ; {-# INLINE typeVal #-}

typeVal' :: forall a t. (KnownType a, IsTypeRep t) => t
typeVal' = typeVal (Proxy :: Proxy a) ; {-# INLINE typeVal' #-}

-- KnownTypes

class                                    KnownTypes ls        where typeVals_ :: [TypeRep]
instance (KnownType l, KnownTypes ls) => KnownTypes (l ': ls) where typeVals_ = typeVal' @l : typeVals_ @ls ; {-# INLINE typeVals_ #-}
instance                                 KnownTypes '[]       where typeVals_ = []                            ; {-# INLINE typeVals_ #-}

typeVals' :: forall ls t. (KnownTypes ls, IsTypeRep t) => [t]
typeVals' = view (from asTypeRep) <$> typeVals_ @ls ; {-# INLINE typeVals' #-}
