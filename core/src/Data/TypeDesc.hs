{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.TypeDesc (module Data.TypeDesc, module X) where

import Data.Typeable.Proxy.Abbr
import Prologue hiding (Simple)
import Data.Reflection
import Data.Typeable as X
import Data.Kind



class TypeShow2 a where
    showTypeComponents :: Proxy a -> [String] -> [String]

-- instance {-# OVERLAPPABLE #-} Typeable a => TypeShow2 a where
--     showTypeComponents a = (show (typeRep a) :)

showType2 :: TypeShow2 a => Proxy a -> [String] -> String
showType2 = undefined

----------------------------------
-- === Reflection etensions === --
----------------------------------

newtype MagicT t a r = MagicT (forall (s :: *). Reifies (t s) a => Proxy s -> r)

reifyT :: forall t a r. a -> (forall (s :: *). Reifies (t s) a => Proxy s -> r) -> r
reifyT a k = unsafeCoerce (MagicT k :: MagicT t a r) (const a) Proxy

reflect' :: forall s a. Reifies s a => a
reflect' = reflect (p :: P s) ; {-# INLINE reflect' #-}


---------------------
-- === TypeDesc === --
---------------------

data TypeDesc = TypeDesc { _rep  :: TypeRep
                         , _desc :: String
                         }
makeLenses ''TypeDesc

instance Show TypeDesc where
    show = show . view rep

class IsTypeDesc a where
    asTypeDesc :: Iso' a TypeDesc

instance IsTypeDesc TypeDesc where
    asTypeDesc = id ; {-# INLINE asTypeDesc #-}


-- === Reifying === --

data TypeRef   (s :: *)
type TypeProxy s = Proxy (TypeRef s)
type TypeReify s = Reifies s TypeDesc

reifyKnownType :: forall r. (forall (s :: *). TypeReify s => TypeProxy s -> r) -> TypeDesc -> r
reifyKnownType f a = reify a $ f . reproxyTypeRef ; {-# INLINE reifyKnownType #-}

reifyKnownTypeT :: forall t r. (forall (s :: *). TypeReify (t s) => TypeProxy s -> r) -> TypeDesc -> r
reifyKnownTypeT f a = reifyT @t a $ f . reproxyTypeRef ; {-# INLINE reifyKnownTypeT #-}

reproxyTypeRef :: P s -> TypeProxy s
reproxyTypeRef _ = p ; {-# INLINE reproxyTypeRef #-}


-- === KnownType === --


-- tyConVals

tyConVals :: KnownDType a => Proxy a -> (TyCon, [TypeDesc])
tyConVals = fmap reverse . tyConValsR ; {-# INLINE tyConVals #-}

class                                       KnownDType (a :: k) where tyConValsR :: Proxy a -> (TyCon, [TypeDesc])
instance {-# OVERLAPPABLE #-} Typeable a => KnownDType a        where tyConValsR   = (,[]) . typeRepTyCon . typeRep            ; {-# INLINE tyConValsR #-}
instance (KnownType a, KnownDType t)     => KnownDType (t a)    where tyConValsR _ = (typeDesc' @a :) <$> tyConValsR (p :: P t) ; {-# INLINE tyConValsR #-}

-- KnownType

class                                         KnownType a           where typeDesc_ :: Proxy a -> TypeDesc
instance                      TypeReify  s => KnownType (TypeRef s) where typeDesc_ _ = reflect (p :: P s)             ; {-# INLINE typeDesc_ #-}
instance {-# OVERLAPPABLE #-} (KnownDType a, TypeShow2 a) => KnownType a where
    typeDesc_ p = TypeDesc (mkTyConApp t $ view rep <$> ds) (showType2 (Proxy :: Proxy a) $ view desc <$> ds) where
        (t, ds) = tyConVals p

        -- TypeDesc (uncurry mkTyConApp $ tyConVals p) (showType p) ; {-# INLINE typeDesc_ #-}

typeDesc'_ :: forall a. KnownType a => TypeDesc
typeDesc'_ = typeDesc_ (Proxy :: Proxy a) ; {-# INLINE typeDesc'_ #-}

typeDesc :: (KnownType a, IsTypeDesc t) => Proxy a -> t
typeDesc = view (from asTypeDesc) . typeDesc_ ; {-# INLINE typeDesc #-}

typeDesc' :: forall a t. (KnownType a, IsTypeDesc t) => t
typeDesc' = typeDesc (Proxy :: Proxy a) ; {-# INLINE typeDesc' #-}

-- KnownTypes

class                                    KnownTypes ls        where typeDescs_ :: [TypeDesc]
instance (KnownType l, KnownTypes ls) => KnownTypes (l ': ls) where typeDescs_ = typeDesc' @l : typeDescs_ @ls ; {-# INLINE typeDescs_ #-}
instance                                 KnownTypes '[]       where typeDescs_ = []                            ; {-# INLINE typeDescs_ #-}

typeDescs' :: forall ls t. (KnownTypes ls, IsTypeDesc t) => [t]
typeDescs' = view (from asTypeDesc) <$> typeDescs_ @ls ; {-# INLINE typeDescs' #-}
