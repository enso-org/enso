module Luna.Prim.CTypes where

import Prologue
import Foreign

newtype CInt8 = CInt8 Int8
    deriving (Eq,Ord,Num,Enum,Storable,Real,Bounded,Integral,Bits,FiniteBits)

instance Show CInt8 where
    show (CInt8 a) = show a

instance Read CInt8 where
    readsPrec i a = map (over _1 CInt8) (readsPrec i a)

newtype CInt16 = CInt16 Int16
    deriving (Eq,Ord,Num,Enum,Storable,Real,Bounded,Integral,Bits,FiniteBits)

instance Show CInt16 where
    show (CInt16 a) = show a

instance Read CInt16 where
    readsPrec i a = map (over _1 CInt16) (readsPrec i a)

newtype CInt32 = CInt32 Int32
    deriving (Eq,Ord,Num,Enum,Storable,Real,Bounded,Integral,Bits,FiniteBits)

instance Show CInt32 where
    show (CInt32 a) = show a

instance Read CInt32 where
    readsPrec i a = map (over _1 CInt32) (readsPrec i a)

newtype CInt64 = CInt64 Int64
    deriving (Eq,Ord,Num,Enum,Storable,Real,Bounded,Integral,Bits,FiniteBits)

instance Show CInt64 where
    show (CInt64 a) = show a

instance Read CInt64 where
    readsPrec i a = map (over _1 CInt64) (readsPrec i a)

newtype CUInt8 = CUInt8 Word8
    deriving (Eq,Ord,Num,Enum,Storable,Real,Bounded,Integral,Bits,FiniteBits)

instance Show CUInt8 where
    show (CUInt8 a) = show a

instance Read CUInt8 where
    readsPrec i a = map (over _1 CUInt8) (readsPrec i a)

newtype CUInt16 = CUInt16 Word16
    deriving (Eq,Ord,Num,Enum,Storable,Real,Bounded,Integral,Bits,FiniteBits)

instance Show CUInt16 where
    show (CUInt16 a) = show a

instance Read CUInt16 where
    readsPrec i a = map (over _1 CUInt16) (readsPrec i a)

newtype CUInt32 = CUInt32 Word32
    deriving (Eq,Ord,Num,Enum,Storable,Real,Bounded,Integral,Bits,FiniteBits)

instance Show CUInt32 where
    show (CUInt32 a) = show a

instance Read CUInt32 where
    readsPrec i a = map (over _1 CUInt32) (readsPrec i a)

newtype CUInt64 = CUInt64 Word64
    deriving (Eq,Ord,Num,Enum,Storable,Real,Bounded,Integral,Bits,FiniteBits)

instance Show CUInt64 where
    show (CUInt64 a) = show a

instance Read CUInt64 where
    readsPrec i a = map (over _1 CUInt64) (readsPrec i a)
