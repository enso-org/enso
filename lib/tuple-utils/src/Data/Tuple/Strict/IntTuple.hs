{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Data.Tuple.Strict.IntTuple (module Data.Tuple.Strict.IntTuple) where

import Data.Tuple.Utils.TH
import Prologue            hiding (FromList)



class IxElemGetter (idx :: Nat) tup where getElemAt :: tup -> Int
class IxElemSetter (idx :: Nat) tup where setElemAt :: Int -> tup -> tup


-- >> data T2 = T2 {-# UNPACK #-} !Int {-# UNPACK #-} !Int deriving (Show)
genStrictIntTupDecls

-- >> FromNat 3 = T3
genIntTupleFromNat

-- >> instance IxElemGetter 0 T3 where
-- >>     getElemAt (T3 !t1 !t2 !t3) = t1 ; {-# INLINE getElemAt #-}
-- >> instance IxElemSetter 0 T3 where
-- >>     setElemAt v (T3 !t1 !t2 !t3) = T3 v t2 t3 ; {-# INLINE setElemAt #-}
genIntTupleIxElemGetters
genIntTupleIxElemSetters
