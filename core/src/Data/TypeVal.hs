{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.TypeVal (module Data.TypeVal, module X) where

import Data.Typeable.Proxy.Abbr
import Prologue hiding (Simple)
import Data.Reflection
import Data.Typeable as X
import Data.Kind




type family BaseType a where
    BaseType (Proxy (t a)) = BaseType (Proxy t)
    BaseType (Proxy a)     = Proxy a

baseType :: (BaseType (Proxy a) ~ Proxy t) => Proxy a -> Proxy t
baseType _ = Proxy



class TypeShow2 a where
    showTypeComponents :: Proxy a -> [String] -> [String]

instance {-# OVERLAPPABLE #-} (Typeable t, BaseType (Proxy a) ~ Proxy t) => TypeShow2 a where
    showTypeComponents a = (show (typeRep $ baseType a) :)

showType2 :: TypeShow2 a => Proxy a -> [String] -> String
showType2 p cs = fmt $ intercalate " " ccs where
    ccs = showTypeComponents p cs
    fmt = if length ccs > 1 then (\s -> "(" <> s <> ")")
                            else id

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

data TypeDesc = TypeDesc { _descTypeRep :: TypeRep
                         , _descStrRep  :: String
                         }
makeLenses ''TypeDesc

instance Show TypeDesc where
    show = view descStrRep

class IsTypeDesc a where
    asTypeDesc :: Iso' a TypeDesc
    default asTypeDesc :: (Wrapped a, Unwrapped a ~ TypeDesc) => Iso' a TypeDesc
    asTypeDesc = wrapped' ; {-# INLINE asTypeDesc #-}

instance IsTypeDesc TypeDesc where
    asTypeDesc = id ; {-# INLINE asTypeDesc #-}

instance Eq TypeDesc where
    (==) = (==) `on` view descTypeRep ; {-# INLINE (==) #-}

instance Ord TypeDesc where
    compare = compare `on` view descTypeRep ; {-# INLINE compare #-}


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
instance (KnownType a, KnownDType t)     => KnownDType (t a)    where tyConValsR _ = (typeVal' @a :) <$> tyConValsR (p :: P t) ; {-# INLINE tyConValsR #-}

-- KnownType

class                                         KnownType a           where typeVal_ :: Proxy a -> TypeDesc
instance                      TypeReify  s => KnownType (TypeRef s) where typeVal_ _ = reflect (p :: P s)             ; {-# INLINE typeVal_ #-}
instance {-# OVERLAPPABLE #-} (KnownDType a, TypeShow2 a) => KnownType a where
    typeVal_ p = TypeDesc (mkTyConApp t $ view descTypeRep <$> ds) (showType2 (Proxy :: Proxy a) $ view descStrRep <$> ds) where
        (t, ds) = tyConVals p


        -- TypeDesc (uncurry mkTyConApp $ tyConVals p) (showType p) ; {-# INLINE typeVal_ #-}

typeVal'_ :: forall a. KnownType a => TypeDesc
typeVal'_ = typeVal_ (Proxy :: Proxy a) ; {-# INLINE typeVal'_ #-}

typeVal :: (KnownType a, IsTypeDesc t) => Proxy a -> t
typeVal = view (from asTypeDesc) . typeVal_ ; {-# INLINE typeVal #-}

typeVal' :: forall a t. (KnownType a, IsTypeDesc t) => t
typeVal' = typeVal (Proxy :: Proxy a) ; {-# INLINE typeVal' #-}

-- KnownTypes

class                                    KnownTypes ls        where typeVals_ :: [TypeDesc]
instance (KnownType l, KnownTypes ls) => KnownTypes (l ': ls) where typeVals_ = typeVal' @l : typeVals_ @ls ; {-# INLINE typeVals_ #-}
instance                                 KnownTypes '[]       where typeVals_ = []                            ; {-# INLINE typeVals_ #-}

typeVals' :: forall ls t. (KnownTypes ls, IsTypeDesc t) => [t]
typeVals' = view (from asTypeDesc) <$> typeVals_ @ls ; {-# INLINE typeVals' #-}



fromTypeDesc :: IsTypeDesc a => TypeDesc -> a
fromTypeDesc = view $ from asTypeDesc ; {-# INLINE fromTypeDesc #-}

switchedTypeDesc :: (IsTypeDesc a, IsTypeDesc b) => Lens' a b
switchedTypeDesc = asTypeDesc . from asTypeDesc ; {-# INLINE switchedTypeDesc #-}

switchTypeDesc :: (IsTypeDesc a, IsTypeDesc b) => a -> b
switchTypeDesc = view switchedTypeDesc ; {-# INLINE switchTypeDesc #-}



-- ----------------------------------
-- -- === Reflection etensions === --
-- ----------------------------------
--
-- newtype MagicT t a r = MagicT (forall (s :: *). Reifies (t s) a => Proxy s -> r)
--
-- reifyT :: forall t a r. a -> (forall (s :: *). Reifies (t s) a => Proxy s -> r) -> r
-- reifyT a k = unsafeCoerce (MagicT k :: MagicT t a r) (const a) Proxy
--
-- reflect' :: forall s a. Reifies s a => a
-- reflect' = reflect (p :: P s) ; {-# INLINE reflect' #-}
--
--
-- ---------------------
-- -- === TypeVal === --
-- ---------------------
--
-- -- === Reifying === --
--
-- data TypeRef   (s :: *)
-- type TypeProxy s = Proxy (TypeRef s)
-- type TypeReify s = Reifies s TypeRep
--
-- reifyKnownType :: forall r. (forall (s :: *). TypeReify s => TypeProxy s -> r) -> TypeRep -> r
-- reifyKnownType f a = reify a $ f . reproxyTypeRef ; {-# INLINE reifyKnownType #-}
--
-- reifyKnownTypeT :: forall t r. (forall (s :: *). TypeReify (t s) => TypeProxy s -> r) -> TypeRep -> r
-- reifyKnownTypeT f a = reifyT @t a $ f . reproxyTypeRef ; {-# INLINE reifyKnownTypeT #-}
--
-- reproxyTypeRef :: P s -> TypeProxy s
-- reproxyTypeRef _ = p ; {-# INLINE reproxyTypeRef #-}
--
--
-- -- === KnownType === --
--
--
-- -- tyConVals
--
-- tyConVals :: KnownDType a => Proxy a -> (TyCon, [TypeRep])
-- tyConVals = fmap reverse . tyConValsR ; {-# INLINE tyConVals #-}
--
-- class                                       KnownDType (a :: k) where tyConValsR :: Proxy a -> (TyCon, [TypeRep])
-- instance {-# OVERLAPPABLE #-} Typeable a => KnownDType a        where tyConValsR   = (,[]) . typeRepTyCon . typeRep            ; {-# INLINE tyConValsR #-}
-- instance (KnownType a, KnownDType t)     => KnownDType (t a)    where tyConValsR _ = (typeVal' @a :) <$> tyConValsR (p :: P t) ; {-# INLINE tyConValsR #-}
--
-- -- KnownType
--
-- class                                         KnownType a           where typeVal_ :: Proxy a -> TypeRep
-- instance {-# OVERLAPPABLE #-} KnownDType a => KnownType a           where typeVal_   = uncurry mkTyConApp . tyConVals ; {-# INLINE typeVal_ #-}
-- instance                      TypeReify  s => KnownType (TypeRef s) where typeVal_ _ = reflect (p :: P s)             ; {-# INLINE typeVal_ #-}
--
-- typeVal'_ :: forall a. KnownType a => TypeRep
-- typeVal'_ = typeVal_ (Proxy :: Proxy a) ; {-# INLINE typeVal'_ #-}
--
-- typeVal :: (KnownType a, IsTypeRep t) => Proxy a -> t
-- typeVal = view (from asTypeRep) . typeVal_ ; {-# INLINE typeVal #-}
--
-- typeVal' :: forall a t. (KnownType a, IsTypeRep t) => t
-- typeVal' = typeVal (Proxy :: Proxy a) ; {-# INLINE typeVal' #-}
--
-- -- KnownTypes
--
-- class                                    KnownTypes ls        where typeVals_ :: [TypeRep]
-- instance (KnownType l, KnownTypes ls) => KnownTypes (l ': ls) where typeVals_ = typeVal' @l : typeVals_ @ls ; {-# INLINE typeVals_ #-}
-- instance                                 KnownTypes '[]       where typeVals_ = []                            ; {-# INLINE typeVals_ #-}
--
-- typeVals' :: forall ls t. (KnownTypes ls, IsTypeRep t) => [t]
-- typeVals' = view (from asTypeRep) <$> typeVals_ @ls ; {-# INLINE typeVals' #-}
