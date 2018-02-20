module Data.Tuple.Strict.IntTuple (module Data.Tuple.Strict.IntTuple) where

import Prologue hiding (FromList)
import Data.Tuple.Utils.TH



class ElemGetter (idx :: Nat) tup where getElemAt :: tup -> Int
class ElemSetter (idx :: Nat) tup where setElemAt :: Int -> tup -> tup


-- >> data T2 = T2 {-# UNPACK #-} !Int {-# UNPACK #-} !Int deriving (Show)
genStrictIntTupDecls

-- >> FromNat 3 = T3
genIntTupleFromNat


-- >> instance ElemGetter 0 T3 where getElemAt (T3 !t1 !t2 !t3) = t1 ; {-# INLINE getElemAt #-}
-- >> instance ElemSetter 0 T3 where setElemAt v (T3 !t1 !t2 !t3) = T3 v t2 t3 ; {-# INLINE setElemAt #-}
genIntTupleElemGetters
genIntTupleElemSetters
