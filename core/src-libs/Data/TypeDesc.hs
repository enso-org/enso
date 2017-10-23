{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.TypeDesc (module Data.TypeDesc, module X) where

import Data.Typeable.Proxy.Abbr
import Prologue_old hiding (Simple)
import Data.Reflection
import Data.Typeable as X (TypeRep, TyCon, tyConFingerprint, typeRepTyCon)
import Data.Kind
import GHC.Fingerprint (Fingerprint, fingerprintFingerprints)
import Data.Typeable.Internal (TypeRep(TypeRep))



---------------------------------
-- === Type pretty printer === --
---------------------------------

-- === Definition === --

class TypePretty a where
    formatType :: [String] -> [String]

instance {-# OVERLAPPABLE #-} (Typeable t, BaseType (Proxy a) ~ Proxy t) => TypePretty a where
    formatType = (show (typeRep $ baseType (Proxy :: Proxy a)) :) ; {-# INLINE formatType #-}

type family BaseType a where
    BaseType (Proxy (t a)) = BaseType (Proxy t)
    BaseType (Proxy a)     = Proxy a


-- === Utils === --

baseType :: (BaseType (Proxy a) ~ Proxy t) => Proxy a -> Proxy t
baseType _ = Proxy

showFormattedType :: forall a. TypePretty a => [String] -> String
showFormattedType cs = fmt $ intercalate " " ccs where
    ccs = formatType @a cs
    fmt = if length ccs > 1 then (\s -> "(" <> s <> ")")
                            else id



----------------------
-- === TypeDesc === --
----------------------

-- === Definition === --

-- | TypeDesc is extended version of Data.Typeable.TypeRep, keeping additional pretty type representation.

type Pretty   = String
type KindDesc = TypeDesc

data TypeDesc = TypeDesc { _fingerprint :: !Fingerprint
                         , _tyCon       :: TyCon
                         , _kindReps    :: [KindDesc]
                         , _subDescs    :: [TypeDesc]
                         , _pretty      :: Pretty
                         }
makeLenses ''TypeDesc

instance Show TypeDesc where show = view pretty                ; {-# INLINE show #-}
instance Eq   TypeDesc where (==) = (==) `on` view fingerprint ; {-# INLINE (==) #-}
instance Ord  TypeDesc where (<=) = (<=) `on` view fingerprint ; {-# INLINE (<=) #-}


-- === Utils === --

toTypeRep :: TypeDesc -> TypeRep
toTypeRep (TypeDesc f t k s _) = TypeRep f t (toTypeRep <$> k) (toTypeRep <$> s) ; {-# INLINE toTypeRep #-}

mkPolyTyConApp :: TyCon -> [KindDesc] -> [TypeDesc] -> Pretty -> TypeDesc
{-# INLINE mkPolyTyConApp #-}
mkPolyTyConApp tc kinds types pr = TypeDesc (fingerprintFingerprints sub_fps) tc kinds types pr
  where !kt_fps = typeRepFingerprints kinds types
        sub_fps = tyConFingerprint tc : kt_fps
        typeRepFingerprints ks ts = (view fingerprint <$> ks) <> (view fingerprint <$> ts)

mkTyConApp  :: TyCon -> [TypeDesc] -> Pretty -> TypeDesc
mkTyConApp tc = mkPolyTyConApp tc [] ; {-# INLINE mkTyConApp #-}

splitTyConApp :: TypeDesc -> (TyCon,[TypeDesc])
splitTyConApp t = (t ^. tyCon, t ^. subDescs) ; {-# INLINE splitTyConApp #-}


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


-- === KnownType === --

tyConDescs :: forall a. KnownDType a => Proxy a -> (TyCon, [TypeDesc])
tyConDescs _ = fmap reverse $ tyConDescsR @a ; {-# INLINE tyConDescs #-}

class                                       KnownDType a     where tyConDescsR :: (TyCon, [TypeDesc])
instance {-# OVERLAPPABLE #-} Typeable a => KnownDType a     where tyConDescsR = (,[]) . typeRepTyCon $ typeRep' @a    ; {-# INLINE tyConDescsR #-}
instance (KnownType a, KnownDType t)     => KnownDType (t a) where tyConDescsR = (getTypeDesc @a :) <$> tyConDescsR @t ; {-# INLINE tyConDescsR #-}

class                                        KnownType a           where getTypeDesc'_ :: Proxy a -> TypeDesc
instance {-# OVERLAPPING #-} TypeReify s  => KnownType (TypeRef s) where getTypeDesc'_ _ = reflect (p :: P s) ; {-# INLINE getTypeDesc'_ #-}
instance (KnownDType a, TypePretty a)     => KnownType a           where getTypeDesc'_ p = mkTyConApp t ds . showFormattedType @a $ view pretty <$> ds
                                                                                           where (t, ds) = tyConDescs p
                                                                         {-# INLINE getTypeDesc'_ #-}


-- === Utils === --

getTypeDesc_ :: forall a. KnownType a => TypeDesc
getTypeDesc_ = getTypeDesc'_ (Proxy :: Proxy a) ; {-# INLINE getTypeDesc_ #-}

getTypeDesc' :: (KnownType a, IsTypeDesc t) => Proxy a -> t
getTypeDesc' = view (from typeDesc) . getTypeDesc'_ ; {-# INLINE getTypeDesc' #-}

getTypeDesc :: forall a t. (KnownType a, IsTypeDesc t) => t
getTypeDesc = getTypeDesc' (Proxy :: Proxy a) ; {-# INLINE getTypeDesc #-}

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
getTypeVal'_ = toTypeRep . getTypeDesc'_ ; {-# INLINE getTypeVal'_ #-}

getTypeVal_ :: forall a. KnownType a => TypeRep
getTypeVal_ = getTypeVal'_ (Proxy :: Proxy a) ; {-# INLINE getTypeVal_ #-}

getTypeVal' :: (KnownType a, IsTypeRep t) => Proxy a -> t
getTypeVal' = view (from asTypeRep) . getTypeVal'_ ; {-# INLINE getTypeVal' #-}

getTypeVal :: forall a t. (KnownType a, IsTypeRep t) => t
getTypeVal = getTypeVal' (Proxy :: Proxy a) ; {-# INLINE getTypeVal #-}



-------------------------
-- === Reification === --
-------------------------

-- === Reflection etensions === --

newtype MagicT t a r = MagicT (forall (s :: *). Reifies (t s) a => Proxy s -> r)

reifyT :: forall t a r. a -> (forall (s :: *). Reifies (t s) a => Proxy s -> r) -> r
reifyT a k = unsafeCoerce (MagicT k :: MagicT t a r) (const a) Proxy

reflect' :: forall s a. Reifies s a => a
reflect' = reflect (p :: P s) ; {-# INLINE reflect' #-}


-- === Definitions === --

data TypeRef   (s :: *)
type TypeProxy s = Proxy (TypeRef s)
type TypeReify s = Reifies s TypeDesc

reifyKnownType :: forall r. (forall (s :: *). TypeReify s => TypeProxy s -> r) -> TypeDesc -> r
reifyKnownType f a = reify a $ f . reproxyTypeRef ; {-# INLINE reifyKnownType #-}

reifyKnownTypeT :: forall t r. (forall (s :: *). TypeReify (t s) => TypeProxy s -> r) -> TypeDesc -> r
reifyKnownTypeT f a = reifyT @t a $ f . reproxyTypeRef ; {-# INLINE reifyKnownTypeT #-}

reproxyTypeRef :: P s -> TypeProxy s
reproxyTypeRef _ = p ; {-# INLINE reproxyTypeRef #-}
