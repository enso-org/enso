{-# LANGUAGE OverloadedStrings #-}

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

newtype VectorText = VectorText (Vector Char) deriving (Mempty, Semigroup, P.Monoid, Eq, Ord, Binary)
makeLenses ''VectorText


-- === Info === --

-- | O(1)
length :: VectorText -> Int
length = Vector.length . unwrap


-- === Construction === --

-- | O(1)
singleton :: Char -> VectorText
singleton = wrap . Vector.singleton

-- | O(1)
uninitialized :: Int -> VectorText
uninitialized i = runST $ unsafeFreeze =<< Mutable.new i

-- | O(n)
replicate :: Int -> Char -> VectorText
replicate = wrap .: Vector.replicate

-- | O(n)
generate :: Int -> (Int -> Char) -> VectorText
generate = wrap .: Vector.generate

-- | O(n)
concat :: [VectorText] -> VectorText
concat ts = wrap $ Vector.concat (unwrap <$> ts)


-- === Deconstruction === --

-- | O(1)
splitHead :: VectorText -> Maybe (Char, VectorText)
splitHead t = justIf (length t > 0) (Vector.unsafeHead v, wrap $ Vector.unsafeTail v) where
    v = unwrap t

-- | O(1)
head :: VectorText -> Maybe Char
head = fmap2 fst splitHead


-- === Modification === --

-- | O(1)
init :: VectorText -> VectorText
init = wrapped %~ Vector.init

-- | O(1)
tail :: VectorText -> VectorText
tail = wrapped %~ Vector.tail

-- | O(1)
take :: Int -> VectorText -> VectorText
take i = wrapped %~ Vector.take i

-- | O(1)
drop :: Int -> VectorText -> VectorText
drop i = wrapped %~ Vector.drop i

-- | O(n)
map :: (Char -> Char) -> VectorText -> VectorText
map f = wrapped %~ Vector.map f

-- | O(n)
imap :: (Int -> Char -> Char) -> VectorText -> VectorText
imap f = wrapped %~ Vector.imap f

-- | O(n)
concatMap :: (Char -> VectorText) -> VectorText -> VectorText
concatMap f = wrapped %~ Vector.concatMap (unwrap . f)

replace :: Int -> Int -> VectorText -> (VectorText -> VectorText)
replace begin end new t = concat [take begin t, new, drop end t]


-- === Indexing === --

-- | O(1)
type instance Index   VectorText = Int
type instance IxValue VectorText = Char
instance Ixed VectorText where ix = wrapped .: ix


-- === Slicing === --

-- | O(1)
slice :: Int -> Int -> VectorText -> VectorText
slice idx len t = if lenDiff > 0 then unsafeSlice idx' len'' t else mempty where
    (idx', len') = if idx < 0 then (0,len + idx) else (idx,len)
    lenDiff      = length t - idx'
    len''        = min len' lenDiff

-- | O(1)
unsafeSlice :: Int -> Int -> VectorText -> VectorText
unsafeSlice idx len = wrapped %~ Vector.unsafeSlice idx len


-- === Mutable conversions === --

-- | O(n)
freeze :: PrimMonad m => MVectorText (PrimState m) -> m VectorText
thaw   :: PrimMonad m => VectorText -> m (MVectorText (PrimState m))
freeze t = wrap <$> Vector.freeze (unwrap t)
thaw   t = wrap <$> Vector.thaw   (unwrap t)

-- | O(1)
unsafeFreeze :: PrimMonad m => MVectorText (PrimState m) -> m VectorText
unsafeThaw   :: PrimMonad m => VectorText -> m (MVectorText (PrimState m))
unsafeFreeze t = wrap <$> Vector.unsafeFreeze (unwrap t)
unsafeThaw   t = wrap <$> Vector.unsafeThaw   (unwrap t)


-- === Memory management === --

-- | O(n)
force :: VectorText -> VectorText
force = wrapped %~ Vector.force


-- === Conversions === --

type instance Item VectorText = Char

instance Convertible Char   VectorText   where convert = singleton
instance Convertible [Char] VectorText   where convert = wrap . Vector.fromList
instance Convertible VectorText   [Char] where convert = Vector.toList . unwrap

instance FromList VectorText where fromList   = convert
instance ToList   VectorText where toList     = convert
instance IsString VectorText where fromString = convert



-- === Instances === --

instance Show VectorText where show = show . toList
