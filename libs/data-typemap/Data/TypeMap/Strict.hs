{-# LANGUAGE UndecidableInstances #-}

module Data.TypeMap.Strict where

import Prologue

import qualified Data.Tuple.Strict as Tuple



---------------------
-- === TypeMap === --
---------------------

-- === Definition === --

newtype TypeMap (ts :: [Type]) = TypeMap (TypeMapData ts)
type    TypeMapData ts = Tuple.FromList ts
makeLenses ''TypeMap


-- === API === --

type ElemGetter el ts = Tuple.ElemGetter         el (TypeMapData ts)
type ElemSetter el ts = Tuple.ElemSetterKeepType el (TypeMapData ts)

getElem :: ∀ el ts. ElemGetter el ts => TypeMap ts -> el
getElem t = Tuple.getElem (unwrap t) ; {-# INLINE getElem #-}

setElem :: ∀ el ts. ElemSetter el ts => el -> TypeMap ts -> TypeMap ts
setElem v = wrapped %~ (Tuple.setElemKeepType @el v) ; {-# INLINE setElem #-}



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
