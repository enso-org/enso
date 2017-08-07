{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}

module Data.VectorText where

import qualified Prelude as P
import           Prologue hiding (length, splitHead, concat, take, drop)

import           Control.Monad.ST            (runST)
import           Data.Binary                 (Binary)
import           Data.Vector.Binary          ()
import           Data.Vector.Unboxed         (Vector)
import qualified Data.Vector.Unboxed         as Vector
import qualified Data.Vector.Unboxed.Mutable as MVector
import           Data.VectorText.Mutable     (MVectorText(MVectorText))
import qualified Data.VectorText.Mutable     as Mutable


-- FIXME[WD]: remove when we hit next LTS stage
instance MVector.Unbox a => Semigroup (Vector a) where (<>) = P.mappend


------------------------
-- === VectorText === --
------------------------

-- === Definition === --

newtype VectorText = VectorText (Vector Char) deriving (Generic, Mempty, NFData, Semigroup, P.Monoid, Eq, Ord, Binary)
makeLenses ''VectorText


-- === Info === --

-- | O(1)
length :: VectorText -> Int
length = Vector.length . unwrap ; {-# INLINE length #-}


-- === Construction === --

-- | O(1)
singleton :: Char -> VectorText
singleton = wrap . Vector.singleton ; {-# INLINE singleton #-}

-- | O(1)
uninitialized :: Int -> VectorText
uninitialized i = runST $ unsafeFreeze =<< Mutable.new i ; {-# INLINE uninitialized #-}

-- | O(n)
replicate :: Int -> Char -> VectorText
replicate = wrap .: Vector.replicate ; {-# INLINE replicate #-}

-- | O(n)
generate :: Int -> (Int -> Char) -> VectorText
generate = wrap .: Vector.generate ; {-# INLINE generate #-}

-- | O(n)
concat :: [VectorText] -> VectorText
concat ts = wrap $ Vector.concat (unwrap <$> ts) ; {-# INLINE concat #-}


-- === Deconstruction === --

-- | O(1)
index :: VectorText -> Int -> Maybe Char
index = (Vector.!?) . unwrap ; {-# INLINE index #-}

-- | O(1)
head :: VectorText -> Maybe Char
head = fmap2 fst splitHead ; {-# INLINE head #-}

-- | O(1)
tail :: VectorText -> Maybe VectorText
tail t = justIf (length t > 0) $ unsafeTail t ; {-# INLINE tail #-}

-- | O(1)
splitHead :: VectorText -> Maybe (Char, VectorText)
splitHead t = justIf (length t > 0) $ unsafeSplitHead t ; {-# INLINE splitHead #-}

-- | O(1)
unsafeIndex :: VectorText -> Int -> Char
unsafeIndex = Vector.unsafeIndex . unwrap ; {-# INLINE unsafeIndex #-}

-- | O(1)
unsafeHead :: VectorText -> Char
unsafeHead = Vector.unsafeHead . unwrap ; {-# INLINE unsafeHead #-}

-- | O(1)
unsafeTail :: VectorText -> VectorText
unsafeTail = wrap . Vector.unsafeTail . unwrap ; {-# INLINE unsafeTail #-}

-- | O(1)
unsafeSplitHead :: VectorText -> (Char, VectorText)
unsafeSplitHead t = (unsafeHead t, unsafeTail t) ; {-# INLINE unsafeSplitHead #-}

-- | O(s)
takeWhile :: (Char -> Bool) -> VectorText -> VectorText
takeWhile f = wrap . Vector.takeWhile f . unwrap ; {-# INLINE takeWhile #-}

-- | O(s)
dropWhile :: (Char -> Bool) -> VectorText -> VectorText
dropWhile f = wrap . Vector.dropWhile f . unwrap ; {-# INLINE dropWhile #-}


-- === Modification === --

-- | O(1)
init :: VectorText -> VectorText
init = wrapped %~ Vector.init ; {-# INLINE init #-}

-- | O(1)
take :: Int -> VectorText -> VectorText
take i = wrapped %~ Vector.take i ; {-# INLINE take #-}

-- | O(1)
drop :: Int -> VectorText -> VectorText
drop i = wrapped %~ Vector.drop i ; {-# INLINE drop #-}

-- | O(n)
map :: (Char -> Char) -> VectorText -> VectorText
map f = wrapped %~ Vector.map f ; {-# INLINE map #-}

-- | O(n)
imap :: (Int -> Char -> Char) -> VectorText -> VectorText
imap f = wrapped %~ Vector.imap f ; {-# INLINE imap #-}

-- | O(n)
concatMap :: (Char -> VectorText) -> VectorText -> VectorText
concatMap f = wrapped %~ Vector.concatMap (unwrap . f) ; {-# INLINE concatMap #-}

replace :: Int -> Int -> VectorText -> (VectorText -> VectorText)
replace begin end new t = concat [take begin t, new, drop end t] ; {-# INLINE replace #-}


-- === Indexing === --

-- | O(1)
type instance Index   VectorText = Int
type instance IxValue VectorText = Char
instance Ixed VectorText where ix = wrapped .: ix ; {-# INLINE ix #-}


-- === Slicing === --

-- | O(1)
slice :: Int -> Int -> VectorText -> VectorText
slice idx len t = if lenDiff > 0 then unsafeSlice idx' len'' t else mempty where
    (idx', len') = if idx < 0 then (0,len + idx) else (idx,len)
    lenDiff      = length t - idx'
    len''        = min len' lenDiff
{-# INLINE slice #-}

-- | O(1)
unsafeSlice :: Int -> Int -> VectorText -> VectorText
unsafeSlice idx len = wrapped %~ Vector.unsafeSlice idx len ; {-# INLINE unsafeSlice #-}

-- | O(1)
splitAt :: Int -> VectorText -> (VectorText, VectorText)
splitAt i = over both wrap . Vector.splitAt i . unwrap ; {-# INLINE splitAt #-}


-- === Mutable conversions === --

-- | O(n)
freeze :: PrimMonad m => MVectorText (PrimState m) -> m VectorText
thaw   :: PrimMonad m => VectorText -> m (MVectorText (PrimState m))
freeze t = wrap <$> Vector.freeze (unwrap t) ; {-# INLINE freeze #-}
thaw   t = wrap <$> Vector.thaw   (unwrap t) ; {-# INLINE thaw   #-}

-- | O(1)
unsafeFreeze :: PrimMonad m => MVectorText (PrimState m) -> m VectorText
unsafeThaw   :: PrimMonad m => VectorText -> m (MVectorText (PrimState m))
unsafeFreeze t = wrap <$> Vector.unsafeFreeze (unwrap t) ; {-# INLINE unsafeFreeze #-}
unsafeThaw   t = wrap <$> Vector.unsafeThaw   (unwrap t) ; {-# INLINE unsafeThaw   #-}


-- === Memory management === --

-- | O(n)
force :: VectorText -> VectorText
force = wrapped %~ Vector.force ; {-# INLINE force #-}


-- === Conversions === --

type instance Item VectorText = Char

instance Convertible Char   VectorText where convert = singleton              ; {-# INLINE convert #-}
instance Convertible [Char] VectorText where convert = wrap . Vector.fromList ; {-# INLINE convert #-}
instance Convertible VectorText [Char] where convert = Vector.toList . unwrap ; {-# INLINE convert #-}
instance Convertible Text   VectorText where convert = convertVia @[Char]     ; {-# INLINE convert #-}

instance FromList VectorText where fromList   = convert ; {-# INLINE fromList   #-}
instance ToList   VectorText where toList     = convert ; {-# INLINE toList     #-}
instance IsString VectorText where fromString = convert ; {-# INLINE fromString #-}



-- === Instances === --

instance Show VectorText where show = show . toList ; {-# INLINE show #-}
