{-# LANGUAGE UndecidableInstances #-}

module Data.Record.Model.Masked where

import Prelude.Luna
import Prologue.Unsafe (error)

import Data.Bits         (Bits, FiniteBits, finiteBitSize, testBit, setBit, zeroBits)
import Unsafe.Coerce     (unsafeCoerce)
import Data.Int          (Int64)
import GHC.Prim          (Any)
import Type.Map          (MapLookup)
import Type.Promotion    (KnownNats, natVals)
import Data.Result       (Ok(Ok))
import Data.Record.Class ( Encode, Decode, Encoder(encode), EncodeMap, UnsafeExtract(unsafeExtract), UnsafeInsert(unsafeInsert), CheckMatch(checkMatch)
                         , Variant, Layout, IsRecord, HasRecord, RecordOf, Group, DecodeMap, Props, asRecord, record
                         )


-------------------
-- === Store === --
-------------------

newtype Store = Store Any
makeWrapped ''Store


-- === Rebuilder === --

type LayoutProxy l = Proxy (l :: [*])
class    MaskRebuilder oldLayout newLayout where rebuildMask :: LayoutProxy oldLayout -> LayoutProxy newLayout -> Mask -> Mask
instance MaskRebuilder layout    layout    where rebuildMask _ _ = id ; {-# INLINE rebuildMask #-}


-- === Utils === --

unsafeStore :: a -> Store
unsafeStore = Store ∘ unsafeCoerce
{-# INLINE unsafeStore #-}

unsafeRestore :: Store -> a
unsafeRestore = unsafeCoerce ∘ unwrap'
{-# INLINE unsafeRestore #-}


-- === Instances === --

instance Show   Store where show _ = "Store"
instance NFData Store where rnf  _ = ()



------------------
-- === Mask === --
------------------

newtype Mask = Mask Int64 deriving (Generic, NFData, Eq, Num, Bits, FiniteBits)
makeWrapped ''Mask

class    HasMask a    where mask :: Lens' a Mask
instance HasMask Mask where mask = id ; {-# INLINE mask #-}


-- === Instances === --

instance Show Mask where
    show m = show (catMaybes (testBit' m <$> [0 .. finiteBitSize m - 1])) where
        testBit' m b = if testBit m b then Just b else Nothing



-----------------------------
-- === Data definition === --
-----------------------------

data Data = Data !Mask !Store deriving (Generic, Show)
data Data2 = Data2 !Mask !Store deriving (Generic)

-- === Instances === --

-- Show
instance Show Data2 where
    showsPrec d (Data2 mask _) = showParen (d > app_prec) $
            showString "Data " . showsPrec (app_prec+1) mask
         where app_prec = 10

-- Normal Form
instance NFData Data
instance NFData Data2



---------------------------------
-- === AST Data definition === --
---------------------------------

-- FIXME[WD]: NFData jest tu zle zaimplementowana bo nie uwzglednia prwadziwych danych i jest przerucana na Data, ktra jest Storem z GHC.Any!
-- FIXME[WD]: The `t` argument should be refactored out cause it is strictly term-related
newtype VGRecord  (groups :: [*]) (variants :: [*]) t d = VGRecord  d deriving (Generic, NFData, Show)
type TermRecord  gs vs t = VGRecord gs vs t Data


-- === Instances === --

type instance Props Variant (VGRecord gs vs t d) = vs
type instance Props Group   (VGRecord gs vs t d) = gs

type instance RecordOf  (VGRecord gs vs t d) = VGRecord gs vs t d
instance      IsRecord  (VGRecord gs vs t d) where asRecord = id ; {-# INLINE asRecord #-}
instance      HasRecord (VGRecord gs vs t d) where record   = id ; {-# INLINE record   #-}

-- Wrappers
makeWrapped ''VGRecord
type instance Unlayered (VGRecord gs vs t d) = Unwrapped (VGRecord gs vs t d)
instance      Layered   (VGRecord gs vs t d)

-- Conversions
instance Castable    (VGRecord gs vs t d) d
instance Convertible (VGRecord gs vs t d) d where convert = unwrap' ; {-# INLINE convert #-}
instance Castable  d (VGRecord gs vs t d)   where cast    = wrap'   ; {-# INLINE cast    #-}


-- === AST Data encoder === --

-- Data encoding

instance ( bits ~ MapLookup v emap
         , emap ~ EncodeMap (rec Data)
         , KnownNats bits
         , Wrapped   (rec Data)
         , Unwrapped (rec Data) ~ Data
         ) => Encoder Variant v Ok (rec Data) where
    encode _ v = Ok $ wrap' $ Data mask $ unsafeStore v where
        bits    = fromIntegral <$> natVals (p :: P bits)
        mask    = foldl' setBit zeroBits bits
    {-# INLINE encode #-}

instance ( MaskRebuilder layout layout'
         , layout  ~ Layout (rec  Data)
         , layout' ~ Layout (rec' Data)
         , Unwrapped (rec  Data) ~ Data
         , Unwrapped (rec' Data) ~ Data
         , Wrapped   (rec  Data)
         , Wrapped   (rec' Data)

         , IsRecord r
         , RecordOf r ~ rec Data
         ) => Encoder Group r Ok (rec' Data) where
    encode _ r = Ok $ wrap' $ Data mask' var where
        Data mask var = unwrap' $ view asRecord r
        mask' = rebuildMask (p :: P layout) (p :: P layout') mask
    {-# INLINE encode #-}

-- Data extraction / insertion

instance {-# OVERLAPPABLE #-} (Unwrapped (r Data) ~ Data, Wrapped (r Data)) => UnsafeExtract Variant (r Data) Ok a where unsafeExtract _ (unwrap' -> Data _ v) = Ok $ unsafeRestore v                                         ; {-# INLINE unsafeExtract #-}
instance {-# OVERLAPPABLE #-} (Unwrapped (r Data) ~ Data, Wrapped (r Data)) => UnsafeInsert  Variant (r Data) Ok a where unsafeInsert  _ a r                   = Ok $ r & wrapped' %~ (\(Data m s) -> Data m (unsafeStore a)) ; {-# INLINE unsafeInsert #-}

-- Pattern matching

instance ( rec  ~ r Data
         , dmap ~ DecodeMap rec
         , nat  ~ MapLookup g dmap
         , KnownNat  nat
         , Wrapped   rec
         , Unwrapped rec ~ Data
         ) => CheckMatch t g (r Data) where
    checkMatch _ _ (unwrap' -> Data mask _) = match where
        bit   = fromIntegral $ natVal (p :: P nat)
        match = testBit mask bit
    {-# INLINE checkMatch #-}






------------------
-- VGRECORD v.2 --
------------------

newtype VGRecord2 (groups :: [*]) (variants :: [*])   d = VGRecord2 d deriving (Generic, NFData, Show)
type TermRecord2 gs vs = VGRecord2 gs vs Data2

-- === Instances === --

type instance Props Variant (VGRecord2 gs vs d) = vs
type instance Props Group   (VGRecord2 gs vs d) = gs

type instance RecordOf  (VGRecord2 gs vs d) = VGRecord2 gs vs d
instance      IsRecord  (VGRecord2 gs vs d) where asRecord = id ; {-# INLINE asRecord #-}
instance      HasRecord (VGRecord2 gs vs d) where record   = id ; {-# INLINE record   #-}

-- Wrappers
makeWrapped ''VGRecord2
type instance Unlayered (VGRecord2 gs vs d) = Unwrapped (VGRecord2 gs vs d)
instance      Layered   (VGRecord2 gs vs d)

-- Conversions
instance Castable    (VGRecord2 gs vs d) d
instance Convertible (VGRecord2 gs vs d) d where convert = unwrap' ; {-# INLINE convert #-}
instance Castable  d (VGRecord2 gs vs d)   where cast    = wrap'   ; {-# INLINE cast    #-}

-- === AST Data encoder === --

-- Data encoding

instance ( bits ~ Encode (rec Data2) v
         , KnownNats bits
         , Wrapped   (rec Data2)
         , Unwrapped (rec Data2) ~ Data2
         ) => Encoder Variant v Ok (rec Data2) where
    encode _ v = Ok $ wrap' $ Data2 mask $ unsafeStore v where
        bits    = fromIntegral <$> natVals (p :: P bits)
        mask    = foldl' setBit zeroBits bits
    {-# INLINE encode #-}

instance ( -- MaskRebuilder layout layout'
        --  , layout  ~ Layout (rec  Data)
        --  , layout' ~ Layout (rec' Data)
        --  , Unwrapped (rec  Data) ~ Data
        --  , Unwrapped (rec' Data) ~ Data
        --  , Wrapped   (rec  Data)
        --  , Wrapped   (rec' Data)
         --
        --  , IsRecord r
        --  , RecordOf r ~ rec Data
         ) => Encoder Group r Ok (rec' Data2) where
    encode _ r = error "data error 1" -- Ok $ wrap' $ Data mask' var where
    --     Data mask var = unwrap' $ view asRecord r
    --     mask' = rebuildMask (p :: P layout) (p :: P layout') mask
    -- {-# INLINE encode #-}

-- Data extraction / insertion

instance {-# OVERLAPPABLE #-} (Unwrapped (r Data2) ~ Data2, Wrapped (r Data2)) => UnsafeExtract Variant (r Data2) Ok a where unsafeExtract _ (unwrap' -> Data2 _ v) = Ok $ unsafeRestore v                                           ; {-# INLINE unsafeExtract #-}
instance {-# OVERLAPPABLE #-} (Unwrapped (r Data2) ~ Data2, Wrapped (r Data2)) => UnsafeInsert  Variant (r Data2) Ok a where unsafeInsert  _ a r                    = Ok $ r & wrapped' %~ (\(Data2 m s) -> Data2 m (unsafeStore a)) ; {-# INLINE unsafeInsert #-}

-- Pattern matching

instance ( rec  ~ r Data2
         , nat ~ Decode rec v
         , KnownNat  nat
         , Wrapped   rec
         , Unwrapped rec ~ Data2
         ) => CheckMatch t v (r Data2) where
    checkMatch _ _ (unwrap' -> Data2 mask _) = match where
        bit   = fromIntegral $ natVal (p :: P nat)
        match = testBit mask bit
    {-# INLINE checkMatch #-}



-- API3 vvv

encodeData2 :: KnownNats (Encode Char v) => v -> Data2
encodeData2 (v :: v) = Data2 mask $ unsafeStore v where
    bits    = fromIntegral <$> natVals (p :: P (Encode Char v))
    mask    = foldl' setBit zeroBits bits
{-# INLINE encodeData2 #-}
