{-# LANGUAGE NoStrict             #-}
{-# LANGUAGE NoStrictData         #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.TypeMap.Strict where

import Prologue hiding (empty)

import qualified Data.Tuple.Strict as Tuple



---------------------
-- === TypeMap === --
---------------------

-- === Definition === --

newtype TypeMap (ts :: [Type]) = TypeMap (TypeMapData ts)
type TypeMapData ts = Tuple.FromList ts
makeLenses ''TypeMap

type ElemGetter el ts = Tuple.ElemGetter         el (TypeMapData ts)
type ElemSetter el ts = Tuple.ElemSetterKeepType el (TypeMapData ts)
type ElemEditor el ts = (ElemGetter el ts, ElemSetter el ts)

type Prependable t ts = ( Tuple.Prepended t (Tuple.FromList ts)
                        ~ Tuple.FromList (t : ts)
                        , Tuple.Prependable t (Tuple.FromList ts)
                        )

type SplitHead t ts =
    ( Tuple.Head (Tuple.FromList (t ': ts)) ~ t
    , Tuple.Tail (Tuple.FromList (t ': ts)) ~ Tuple.FromList ts
    , Tuple.HeadGetter (Tuple.FromList (t ': ts))
    , Tuple.TailGetter (Tuple.FromList (t ': ts))
    )


-- === API === --

getElem :: ∀ el ts. ElemGetter el ts => TypeMap ts -> el
getElem t = Tuple.getElem (unwrap t)
{-# INLINE getElem #-}

setElem :: ∀ el ts. ElemSetter el ts => el -> TypeMap ts -> TypeMap ts
setElem v = wrapped %~ (Tuple.setElemKeepType @el v)
{-# INLINE setElem #-}

modifyElem_ :: ∀ el ts. ElemEditor el ts
            => (el -> el) -> TypeMap ts -> TypeMap ts
modifyElem_ f tm = setElem @el el' tm where
    !el' = f $! getElem @el tm
{-# INLINE modifyElem_ #-}

empty :: TypeMap '[]
empty = wrap Tuple.T0
{-# INLINE empty #-}

prepend :: Prependable t ts => t -> TypeMap ts -> TypeMap (t ': ts)
prepend t tm = wrap $ Tuple.prepend t (unwrap tm)
{-# INLINE prepend #-}

prependDef :: forall t ts . (Default t, Prependable t ts) => TypeMap ts
    -> TypeMap (t ': ts)
prependDef tm = wrap $ Tuple.prepend (def @t) (unwrap tm)
{-# INLINE prependDef #-}

splitHead :: SplitHead t ts => TypeMap (t ': ts) -> (t, TypeMap ts)
splitHead = \tm -> wrap <$> Tuple.splitHead (unwrap tm)
{-# INLINE splitHead #-}


-- === SetElemsFromList === --

class SetElemsFromList (els :: [Type]) t ts where
    setElemsFromList :: [t] -> TypeMap ts -> TypeMap ts

instance SetElemsFromList '[] t ts where
    setElemsFromList _ = id ; {-# INLINE setElemsFromList #-}

-- FIXME [Ara, WD]: unsafeCoerce is very unsafe here. We should consider making
-- this safer. We cannot use `coerce` because it requires having far too many
-- constructors in scope in client code.
instance ( ElemSetter s ts
         , SetElemsFromList ss t ts
         ) => SetElemsFromList (s ': ss) t ts where
    setElemsFromList lst t = case lst of
        []     -> impossible
        (s:ss) -> setElemsFromList @ss ss
                $ setElem (unsafeCoerce s :: s) t
    {-# INLINE setElemsFromList #-}


-- === MakeDefault === --

class MakeDefault (ts :: [Type]) where
    makeDefault :: TypeMap ts

instance (MakeDefault ts, Prependable t ts, Default t) =>
    MakeDefault ((t ': ts) :: [Type]) where
    makeDefault = prepend (def @t) $ makeDefault @ts
    {-# INLINE makeDefault #-}

instance MakeDefault ('[] :: [Type]) where
    makeDefault = empty
    {-# INLINE makeDefault #-}


-- === Instances === --

deriving instance Show    (TypeMapData ts) => Show    (TypeMap ts)
deriving instance Mempty  (TypeMapData ts) => Mempty  (TypeMap ts)
deriving instance Default (TypeMapData ts) => Default (TypeMap ts)



---------------------
-- === Encoder === --
---------------------

-- === Definition === --

class Encoder fields a m where
    encode :: a -> m (TypeMap fields)

class FieldEncoder field a m where
    encodeField :: a -> m field


-- === Instances === --

instance Applicative m
      => Encoder '[] a m where
    encode _ = pure empty
    {-# INLINE encode #-}

instance (Applicative m, Encoder ts a m, FieldEncoder t a m, Prependable t ts)
      => Encoder (t ': ts) a m where
    encode a = prepend <$> encodeField @t a <*> encode @ts a
    {-# INLINE encode #-}

instance {-# OVERLAPPABLE #-} Applicative m
      => FieldEncoder a a m where
    encodeField = pure ; {-# INLINE encodeField #-}

