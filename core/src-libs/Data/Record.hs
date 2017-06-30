{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE TypeApplications     #-}

module Data.Record where

import Luna.Prelude    hiding (Enum)

import Data.Bits         (Bits, FiniteBits, finiteBitSize, testBit, setBit, zeroBits)
import Unsafe.Coerce     (unsafeCoerce)
import Data.Int          (Int64)
import GHC.Exts          (Any)
import Type.Promotion    (KnownNats, natVals)

import Type.Maybe (FromJust)
import Data.RTuple (TMap, Assoc(..))
import qualified Data.RTuple as List
import Data.Property as Prop
import Type.Relation (SemiSuper)

--------------------
-- === Encode === --
--------------------

type family Encode t a :: Maybe Nat

type family MapEncode t fmts where
    MapEncode t '[]       = '[]
    MapEncode t (f ': fs) = FromJust (Encode t f) ': MapEncode t fs

encodeType :: forall t a. KnownNat (FromJust (Encode t a)) => Int
encodeType = fromIntegral $ natVal (p :: P (FromJust (Encode t a)))
{-# INLINE encodeType #-}

-------------------
-- === Store === --
-------------------

-- === Definitions === --

newtype Store (slots :: [Assoc * *]) = Store (TMap slots)

-- === Classes === --

class EncodeStore slots a m where
    encodeStore :: a -> m (Store slots)

class EncodeSlot t a m s where
    encodeSlot :: a -> m s


instance Monad m => EncodeStore '[] a m where
    encodeStore _ = return $ Store List.empty
    {-# INLINE encodeStore #-}

instance (EncodeSlot t a m s, EncodeStore ss a m, Monad m)
      => EncodeStore ((t ':= s) ': ss) a m where
    encodeStore a = Store <$> (List.prepend2 <$> (encodeSlot @t) a <*> (unwrap' <$> (encodeStore a :: m (Store ss))))

-- === Instances === --

makeWrapped ''Store

deriving instance Show (Unwrapped (Store slots)) => Show (Store slots)

type instance Access t (Store slots) = Access t (Unwrapped (Store slots))

instance Accessor t (Unwrapped (Store slots)) => Accessor t (Store slots) where
    access = access @t . unwrap' ; {-# INLINE access #-}

-----------------
-- === Raw === --
-----------------

-- === Definition === --

newtype Raw = Raw Any
makeWrapped ''Raw

-- === Instances === --

instance Show Raw where show _ = "Raw" ; {-# INLINE show #-}

instance (Monad m, Accessor t a)
      => EncodeSlot t a m Raw where
    encodeSlot a = return $ Raw $ unsafeCoerce $ access @t a
    {-# INLINE encodeSlot #-}

------------------
-- === Enum === --
------------------

-- === Definition === --

newtype Enum = Enum Int deriving (Show, Num)
makeWrapped ''Enum


-- === Instances === --

instance (Monad m, x ~ Access t a, KnownNat (FromJust (Encode t x)))
      => EncodeSlot t a m Enum where
    encodeSlot a = return $ Enum nat where
        nat = encodeType @t @x
    {-# INLINE encodeSlot #-}

------------------
-- === Mask === --
------------------

-- === Definition === --

newtype Mask = Mask Int64 deriving (Generic, NFData, Eq, Num, Bits, FiniteBits)
makeWrapped ''Mask

-- === Instances === --

instance Show Mask where
    show m = show (catMaybes (testBit' m <$> [0 .. finiteBitSize m - 1])) where
        testBit' m b = if testBit m b then Just b else Nothing

instance (Monad m, nats ~ MapEncode t (SemiSuper (Access t a)), KnownNats nats)
      => EncodeSlot t a m Mask where
    encodeSlot a = return $ foldl' setBit zeroBits bits where
        bits = fromIntegral <$> natVals (Proxy :: Proxy nats)
    {-# INLINE encodeSlot #-}
