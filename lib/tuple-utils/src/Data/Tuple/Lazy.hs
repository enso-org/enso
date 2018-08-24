{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Data.Tuple.Lazy (module Data.Tuple.Lazy, module X) where

import Data.Tuple.Utils.Class as X

import Data.Tuple.Utils.TH
import Prologue            hiding (FromList, ToList)


-- >> type T2 = (,)
-- >> pattern T2 :: t1 -> t2 -> (t1,t2)
-- >> pattern T2 {t1,t2} = (t1,t2)
genTupleSyn

-- >> FromList '[t1, t2] = T2 t1 t2
-- >> ToList   T2 t1 t2  = '[t1, t2]
genFromList
genToList

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
