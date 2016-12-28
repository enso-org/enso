{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.TypeVal (module Data.TypeVal, module X) where

import Data.Typeable.Proxy.Abbr
import Prologue hiding (Simple)
import Data.Reflection
import Data.Typeable as X
import Data.Kind



newtype TypeRepT t = TypeRepT TypeRep deriving (Ord, Eq)
makeWrapped ''TypeRepT

instance IsTypeRep (TypeRepT t)
instance Show (TypeRepT t) where
    show = show . unwrap' ; {-# INLINE show #-}






------------


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


----------------------
-- === TypeDesc === --
----------------------

-- === TypeDesc === --

data TypeDesc = TypeDesc { _descTypeRep :: TypeRep
                         , _descStrRep  :: String
                         }
makeLenses ''TypeDesc

instance Show TypeDesc where
    show = view descStrRep


instance Eq TypeDesc where
    (==) = (==) `on` view descTypeRep ; {-# INLINE (==) #-}

instance Ord TypeDesc where
    compare = compare `on` view descTypeRep ; {-# INLINE compare #-}



-- === TypeDescT === --

newtype TypeDescT t = TypeDescT TypeDesc deriving (Ord, Eq)
makeWrapped ''TypeDescT

instance Show (TypeDescT t) where
    show = show . unwrap' ; {-# INLINE show #-}


-- === IsTypeDesc === --

class IsTypeDesc a where
    typeDesc :: Iso' a TypeDesc
    default typeDesc :: (Wrapped a, Unwrapped a ~ TypeDesc) => Iso' a TypeDesc
    typeDesc = wrapped' ; {-# INLINE typeDesc #-}

instance IsTypeDesc TypeDesc where
    typeDesc = id ; {-# INLINE typeDesc #-}

instance IsTypeDesc (TypeDescT t)



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

tyConDescs :: KnownDType a => Proxy a -> (TyCon, [TypeDesc])
tyConDescs = fmap reverse . tyConDescsR ; {-# INLINE tyConDescs #-}

class                                       KnownDType (a :: k) where tyConDescsR :: Proxy a -> (TyCon, [TypeDesc])
instance {-# OVERLAPPABLE #-} Typeable a => KnownDType a        where tyConDescsR   = (,[]) . typeRepTyCon . typeRep               ; {-# INLINE tyConDescsR #-}
instance (KnownType a, KnownDType t)     => KnownDType (t a)    where tyConDescsR _ = (getTypeDesc @a :) <$> tyConDescsR (p :: P t) ; {-# INLINE tyConDescsR #-}

-- KnownType

class                                                        KnownType a           where getTypeDesc'_ :: Proxy a -> TypeDesc
instance                      TypeReify s                 => KnownType (TypeRef s) where getTypeDesc'_ _ = reflect (p :: P s) ; {-# INLINE getTypeDesc'_ #-}
instance {-# OVERLAPPABLE #-} (KnownDType a, TypeShow2 a) => KnownType a where
    getTypeDesc'_ p = TypeDesc (mkTyConApp t $ view descTypeRep <$> ds) (showType2 (Proxy :: Proxy a) $ view descStrRep <$> ds) where
        (t, ds) = tyConDescs p


        -- TypeDesc (uncurry mkTyConApp $ tyConDescs p) (showType p) ; {-# INLINE getTypeDesc_ #-}

getTypeDesc_ :: forall a. KnownType a => TypeDesc
getTypeDesc_ = getTypeDesc'_ (Proxy :: Proxy a) ; {-# INLINE getTypeDesc_ #-}

getTypeDesc' :: (KnownType a, IsTypeDesc t) => Proxy a -> t
getTypeDesc' = view (from typeDesc) . getTypeDesc'_ ; {-# INLINE getTypeDesc' #-}

getTypeDesc :: forall a t. (KnownType a, IsTypeDesc t) => t
getTypeDesc = getTypeDesc' (Proxy :: Proxy a) ; {-# INLINE getTypeDesc #-}

-- KnownTypes

class                                    KnownTypes ls        where getTypeDescs_ :: [TypeDesc]
instance (KnownType l, KnownTypes ls) => KnownTypes (l ': ls) where getTypeDescs_ = getTypeDesc @l : getTypeDescs_ @ls ; {-# INLINE getTypeDescs_ #-}
instance                                 KnownTypes '[]       where getTypeDescs_ = []                                 ; {-# INLINE getTypeDescs_ #-}

getTypeDescs :: forall ls t. (KnownTypes ls, IsTypeDesc t) => [t]
getTypeDescs = view (from typeDesc) <$> getTypeDescs_ @ls ; {-# INLINE getTypeDescs #-}



fromTypeDesc :: IsTypeDesc a => TypeDesc -> a
fromTypeDesc = view $ from typeDesc ; {-# INLINE fromTypeDesc #-}

switchedTypeDesc :: (IsTypeDesc a, IsTypeDesc b) => Lens' a b
switchedTypeDesc = typeDesc . from typeDesc ; {-# INLINE switchedTypeDesc #-}

switchTypeDesc :: (IsTypeDesc a, IsTypeDesc b) => a -> b
switchTypeDesc = view switchedTypeDesc ; {-# INLINE switchTypeDesc #-}


getTypeVal'_ :: KnownType a => Proxy a -> TypeRep
getTypeVal'_ = view descTypeRep . getTypeDesc'_ ; {-# INLINE getTypeVal'_ #-}

getTypeVal_ :: forall a. KnownType a => TypeRep
getTypeVal_ = getTypeVal'_ (Proxy :: Proxy a) ; {-# INLINE getTypeVal_ #-}

getTypeVal' :: (KnownType a, IsTypeRep t) => Proxy a -> t
getTypeVal' = view (from asTypeRep) . getTypeVal'_ ; {-# INLINE getTypeVal' #-}

getTypeVal :: forall a t. (KnownType a, IsTypeRep t) => t
getTypeVal = getTypeVal' (Proxy :: Proxy a) ; {-# INLINE getTypeVal #-}



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
-- instance (KnownType a, KnownDType t)     => KnownDType (t a)    where tyConValsR _ = (getTypeVal' @a :) <$> tyConValsR (p :: P t) ; {-# INLINE tyConValsR #-}
--
-- -- KnownType
--
-- class                                         KnownType a           where getTypeVal_ :: Proxy a -> TypeRep
-- instance {-# OVERLAPPABLE #-} KnownDType a => KnownType a           where getTypeVal_   = uncurry mkTyConApp . tyConVals ; {-# INLINE getTypeVal_ #-}
-- instance                      TypeReify  s => KnownType (TypeRef s) where getTypeVal_ _ = reflect (p :: P s)             ; {-# INLINE getTypeVal_ #-}
--
-- getTypeVal'_ :: forall a. KnownType a => TypeRep
-- getTypeVal'_ = getTypeVal_ (Proxy :: Proxy a) ; {-# INLINE getTypeVal'_ #-}
--
-- getTypeVal :: (KnownType a, IsTypeRep t) => Proxy a -> t
-- getTypeVal = view (from asTypeRep) . getTypeVal_ ; {-# INLINE getTypeVal #-}
--
-- getTypeVal' :: forall a t. (KnownType a, IsTypeRep t) => t
-- getTypeVal' = getTypeVal (Proxy :: Proxy a) ; {-# INLINE getTypeVal' #-}
--
-- -- KnownTypes
--
-- class                                    KnownTypes ls        where typeValsOf_ :: [TypeRep]
-- instance (KnownType l, KnownTypes ls) => KnownTypes (l ': ls) where typeValsOf_ = getTypeVal' @l : typeValsOf_ @ls ; {-# INLINE typeValsOf_ #-}
-- instance                                 KnownTypes '[]       where typeValsOf_ = []                            ; {-# INLINE typeValsOf_ #-}
--
-- typeValsOf' :: forall ls t. (KnownTypes ls, IsTypeRep t) => [t]
-- typeValsOf' = view (from asTypeRep) <$> typeValsOf_ @ls ; {-# INLINE typeValsOf' #-}
