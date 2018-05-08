{-# LANGUAGE UndecidableInstances #-}

module Data.TypeMap.Strict where

import Prologue

import qualified Data.Tuple.Strict as Tuple



---------------------
-- === TypeMap === --
---------------------

-- === Definition === --

newtype TypeMap (ts :: [Type]) = TypeMap (TypeMapData ts)
type TypeMapData ts = Tuple.FromList ts
makeLenses ''TypeMap


-- === API === --

type ElemGetter el ts = Tuple.ElemGetter         el (TypeMapData ts)
type ElemSetter el ts = Tuple.ElemSetterKeepType el (TypeMapData ts)

getElem :: ∀ el ts. ElemGetter el ts => TypeMap ts -> el
getElem t = Tuple.getElem (unwrap t) ; {-# INLINE getElem #-}

setElem :: ∀ el ts. ElemSetter el ts => el -> TypeMap ts -> TypeMap ts
setElem v = wrapped %~ (Tuple.setElemKeepType @el v) ; {-# INLINE setElem #-}

empty :: TypeMap '[]
empty = wrap Tuple.T0 ; {-# INLINE empty #-}


type Prependable t ts = ( Tuple.Prepended t (Tuple.FromList ts)
                        ~ Tuple.FromList (t : ts)
                        , Tuple.Prependable t (Tuple.FromList ts)
                        )
prepend :: Prependable t ts => t -> TypeMap ts -> TypeMap (t ': ts)
prepend t tm = wrap $ Tuple.prepend t (unwrap tm) ; {-# INLINE prepend #-}


-- === SetElemsFromList === --

class SetElemsFromList (els :: [Type]) t ts where
    setElemsFromList :: [t] -> TypeMap ts -> TypeMap ts

instance SetElemsFromList '[] t ts where
    setElemsFromList _ = id ; {-# INLINE setElemsFromList #-}

instance ( ElemSetter s ts
         , SetElemsFromList ss t ts
         ) => SetElemsFromList (s ': ss) t ts where
    setElemsFromList lst t = case lst of
        []     -> impossible
        (s:ss) -> setElemsFromList @ss ss
                $ setElem (unsafeCoerce s :: s) t
-- FIXME[WD]: unsafeCoerce is very unsafe here. We should consider making
--            it safer. We cannot use `coerce` because it requires having
--            a lot of constructors in scope in clients code.
    {-# INLINE setElemsFromList #-}



-- === Instances === --

deriving instance Show    (TypeMapData ts) => Show    (TypeMap ts)
deriving instance Default (TypeMapData ts) => Default (TypeMap ts)
