{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Data.Tuple.Strict (module Data.Tuple.Strict, module X) where

import Data.Tuple.Utils.Class as X

import Data.Tuple.Utils.TH
import Prologue            hiding (FromList, ToList)

import qualified Type.Data.List as List



-- >> data T2 a b = T2 !a !b deriving (Show)
genStrictTupDecls

-- >> type family FromList where ... FromList '[t1, t2]  = T2 t1 t2
-- >> type family ToList   where ... ToList   (T2 t1 t2) = '[t1, t2]
genFromList
genToList

-- >> type instance Tail (T3 t1 t2 t3) = T2 t2 t3
-- >> type instance Head (T3 t1 t2 t3) = t1
genHead
genTail

-- >> instance HeadGetter (T3 t1 t2 t3) where
-- >>     head (T3 !t1 !t2 !t3) = t1 ; {-# INLINE head #-}
genHeadGetter

-- >> instance TailGetter (T3 t1 t2 t3) where
-- >>     tail (T3 !t1 !t2 !t3) = T2 t2 t3 ; {-# INLINE tail #-}
genTailGetter

-- >> type instance GetElemAt 0 (T2 t1 t2) = t1
-- >> type instance SetElemAt 0 v (T2 t1 t2) = T2 v t2
genGetElemAt
genSetElemAt

-- >> instance IxElemGetter 0 (T3 t1 t2 t3) where
-- >>     getElemAt (T3 !t1 !t2 !t3) = t1 ; {-# INLINE getElemAt #-}
-- >> instance IxElemSetter 0 (T3 t1 t2 t3) where
-- >>     setElemAt v (T3 !t1 !t2 !t3) = T3 v t2 t3 ; {-# INLINE setElemAt #-}
genIxElemGetters
genIxElemSetters

-- >> instance (Default t1, Default t2) => Default (T2 t1 t2) where
-- >>     def = T2 def def ; {-# INLINE def #-}
genDefaultInstances

-- >> instance (Mempty t1, Mempty t2) => Mempty (T2 t1 t2) where
-- >>     mempty = T2 mempty mempty ; {-# INLINE mempty #-}
genMemptyInstances

-- >> type instance Prepended t (T2 t1 t2) = T3 t t1 t2
genPrepended

-- >> instance Prependable t (T3 t1 t2 t3) where
-- >>     prepend t (T3 !t1 !t2 !t3) = T4 t t1 t2 t3 ; {-# INLINE prepend #-}
genPrependable


type ElemIndex  el t = (List.ElemIndex' el (ToList t))
type ElemSetter el t = IxElemSetter (ElemIndex el t) t
type ElemGetter el t =
    ( GetElemAt    (ElemIndex el t) t ~ el
    , IxElemGetter (ElemIndex el t) t
    )
type ElemSetterKeepType el t =
    ( ElemSetter el t
    , SetElemAt (ElemIndex el t) el t ~ t
    )

getElem :: ∀ el t. ElemGetter el t => t -> el
getElem = getElemAt @(ElemIndex el t) ; {-# INLINE getElem #-}

setElem :: ∀ el v t. ElemSetter el t => v -> t -> SetElemAt (ElemIndex el t) v t
setElem = setElemAt @(ElemIndex el t) ; {-# INLINE setElem #-}

-- | Like 'setElem', but does not modify the result type
setElemKeepType :: ∀ el t. ElemSetterKeepType el t => el -> t -> t
setElemKeepType = setElem @el ; {-# INLINE setElemKeepType #-}

