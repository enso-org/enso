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
         , Coercible t s
         ) => SetElemsFromList (s ': ss) t ts where
    setElemsFromList lst t = case lst of
        []     -> impossible
        (s:ss) -> setElemsFromList @ss ss
                $ setElem (coerce s :: s) t
    {-# INLINE setElemsFromList #-}





-- === Instances === --

deriving instance Show (TypeMapData ts) => Show (TypeMap ts)
