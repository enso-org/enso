{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Strict               #-}

module Data.Container.Vector (module Data.Container.Vector, module X) where

import qualified Prelude as P
import qualified Data.Text as UTF16
import           Prologue hiding (Text, length, splitHead, concat, take, drop, index)

import           Control.Monad.ST            (runST)
import           Data.Binary                 (Binary)
import           Data.Vector.Binary          ()
import qualified Data.Vector.Unboxed         as Unboxed
import qualified GHC.Exts                    as Exts
import           GHC.Exts                    (IsList)
import Data.Vector.Generic hiding (convert)
import Data.Vector.Generic as X (
    -- Info
    length, null,
    -- Construction
    singleton, cons, snoc, concat,
    -- Indexing
    unsafeIndex, unsafeHead, unsafeLast, unsafeTail, unsafeInit,
    -- Slicing
    unsafeSlice, unsafeInit, unsafeTail, unsafeTake, unsafeDrop, splitAt,
    -- Partitioning
    span,
    -- Mapping
    map, imap, concatMap
 )
import qualified Data.Vector.Generic         as V
import qualified Data.Vector.Generic.Mutable as M


--------------------
-- === Vector === --
--------------------

-- === Construction === --

-- | O(1)
alloc :: Vector v a => Int -> v a
alloc i = runST $ unsafeFreeze =<< M.new i ; {-# INLINE alloc #-}

-- | O(n)
replicate' :: Vector v a => Int -> v a -> v a
replicate' i = concat . P.replicate i ; {-# INLINE replicate' #-}


-- -- === Deconstruction === --

-- | O(1)
index, (!) :: Vector v a => v a -> Int -> Maybe a
index = (V.!?) ; {-# INLINE index #-}
(!)   = index  ; {-# INLINE (!)   #-}
infixl 9 !

-- | O(1)
(!!) :: Vector v a => v a -> Int -> a
(!!) = unsafeIndex ; {-# INLINE (!!) #-}
infixl 9 !!

-- | O(1)
head, last :: Vector v a => v a -> Maybe a
head = fmap2 fst splitHead ; {-# INLINE head #-}
last = fmap2 fst splitLast ; {-# INLINE last #-}

-- | O(1)
init, tail :: Vector v a => v a -> Maybe (v a)
init = fmap2 snd splitLast ; {-# INLINE init #-}
tail = fmap2 snd splitHead ; {-# INLINE tail #-}

-- | O(1)
splitHead, splitLast :: Vector v a => v a -> Maybe (a, v a)
splitHead t = justIf (length t > 0) $ unsafeSplitHead t ; {-# INLINE splitHead #-}
splitLast t = justIf (length t > 0) $ unsafeSplitLast t ; {-# INLINE splitLast #-}

-- | O(1)
unsafeSplitHead, unsafeSplitLast :: Vector v a => v a -> (a, v a)
unsafeSplitHead t = (unsafeHead t, unsafeTail t) ; {-# INLINE unsafeSplitHead #-}
unsafeSplitLast t = (unsafeLast t, unsafeInit t) ; {-# INLINE unsafeSplitLast #-}

-- | O(s)
takeTill :: Vector v a => (a -> Bool) -> v a -> v a
takeTill f = \v -> maybe v (flip unsafeTake v) $ findIndex f v ; {-# INLINE takeTill #-}

-- | O(s)
takeWhile :: Vector v a => (a -> Bool) -> v a -> v a
takeWhile = takeTill . fmap not ; {-# INLINE takeWhile #-}

-- | O(s) Just like takeWhile, but uses streaming instead of slicing.
takeWhileStream :: Vector v a => (a -> Bool) -> v a -> v a
takeWhileStream = V.takeWhile ; {-# INLINE takeWhileStream #-}

-- | O(s)
dropWhileStream :: Vector v a => (a -> Bool) -> v a -> v a
dropWhileStream = V.dropWhile ; {-# INLINE dropWhileStream #-}

-- | O(n)
breakAll :: Vector v a => (a -> Bool) -> v a -> [v a]
breakAll f = go where
    go v = case findIndex f v of
        Nothing -> [v]
        Just i  -> unsafeTake i v : go (unsafeDrop (i+1) v)
{-# INLINE breakAll #-}

-- | O(n)
replaceUsing :: Vector v a => (a -> Bool) -> (a -> v a) -> v a -> v a
replaceUsing f rf = concat . go where
    go v = case findIndex f v of
        Nothing -> [v]
        Just i  -> unsafeTake i v : rf (unsafeIndex v i) : go (unsafeDrop (i+1) v)
{-# INLINE replaceUsing #-}

-- | O(n)
replace :: (Vector v a, Eq a) => a -> v a -> v a -> v a
replace a v = replaceUsing (a ==) (const v) ; {-# INLINE replace #-}

-- replace :: Vector v a => a -> v a -> v a -> v a
-- replace


-- -- === Modification === --

-- slice :: :: Vector v a => Int -> Int -> v a	-> v a
-- slice
--
--
-- -- | O(1)
-- take :: Int -> Text -> Text
-- take i = wrapped %~ Vector.take i ; {-# INLINE take #-}
--
-- -- | O(1)
-- drop :: Int -> Text -> Text
-- drop i = wrapped %~ Vector.drop i ; {-# INLINE drop #-}
--
-- -- | O(n)
-- map :: (Char -> Char) -> Text -> Text
-- map f = wrapped %~ Vector.map f ; {-# INLINE map #-}
--
-- -- | O(n)
-- imap :: (Int -> Char -> Char) -> Text -> Text
-- imap f = wrapped %~ Vector.imap f ; {-# INLINE imap #-}
--
-- -- | O(n)
-- concatMap :: (Char -> Text) -> Text -> Text
-- concatMap f = wrapped %~ Vector.concatMap (unwrap . f) ; {-# INLINE concatMap #-}
--
-- -- | O(n)
-- replace :: Int -> Int -> Text -> (Text -> Text)
-- replace begin end new t = concat [take begin t, new, drop end t] ; {-# INLINE replace #-}
--
-- -- | O(1)
-- unsafeDrop :: Int -> Text -> Text
-- unsafeDrop i = wrapped %~ Vector.unsafeDrop i ; {-# INLINE unsafeDrop #-}
--
--
-- -- === Indexing === --
--
-- -- | O(1)
-- type instance Index   Text = Int
-- type instance IxValue Text = Char
-- instance Ixed Text where ix = wrapped .: ix ; {-# INLINE ix #-}
--
-- findIndex :: (Char -> Bool) -> Text -> Maybe Int
-- findIndex f = Vector.findIndex f . unwrap ; {-# INLINE findIndex #-}
--
--
-- -- === Slicing === --
--
-- -- | O(1)
-- slice :: Int -> Int -> Text -> Text
-- slice idx len t = if lenDiff > 0 then unsafeSlice idx' len'' t else mempty where
--     (idx', len') = if idx < 0 then (0,len + idx) else (idx,len)
--     lenDiff      = length t - idx'
--     len''        = min len' lenDiff
-- {-# INLINE slice #-}
--
-- -- | O(1)
-- unsafeSlice :: Int -> Int -> Text -> Text
-- unsafeSlice idx len = wrapped %~ Vector.unsafeSlice idx len ; {-# INLINE unsafeSlice #-}
--
-- -- | O(1)
-- splitAt :: Int -> Text -> (Text, Text)
-- splitAt i = over both wrap . Vector.splitAt i . unwrap ; {-# INLINE splitAt #-}
--
-- -- | O(n)
-- span :: (Char -> Bool) -> Text -> (Text, Text)
-- span f v = over both wrap $ Vector.span f (unwrap v) ; {-# INLINE span #-}
--
--
-- -- === Mutable conversions === --
--
-- -- | O(n)
-- freeze :: PrimMonad m => MText (PrimState m) -> m Text
-- thaw   :: PrimMonad m => Text -> m (MText (PrimState m))
-- freeze t = wrap <$> Vector.freeze (unwrap t) ; {-# INLINE freeze #-}
-- thaw   t = wrap <$> Vector.thaw   (unwrap t) ; {-# INLINE thaw   #-}
--
-- -- | O(1)
-- unsafeFreeze :: PrimMonad m => MText (PrimState m) -> m Text
-- unsafeThaw   :: PrimMonad m => Text -> m (MText (PrimState m))
-- unsafeFreeze t = wrap <$> Vector.unsafeFreeze (unwrap t) ; {-# INLINE unsafeFreeze #-}
-- unsafeThaw   t = wrap <$> Vector.unsafeThaw   (unwrap t) ; {-# INLINE unsafeThaw   #-}
--
--
-- -- === Memory management === --
--
-- -- | O(n)
-- force :: Text -> Text
-- force = wrapped %~ Vector.force ; {-# INLINE force #-}
--
--
-- === Utils === --

commonPrefixes :: (Vector v a, Eq a) => v a -> v a -> Maybe (v a, v a, v a)
commonPrefixes v v' = go 0 where
    minlen = min (length v) (length v')
    go i | i < minlen && a == b = go (i+1)
         | i > 0                = Just (unsafeTake i v, unsafeDrop i v, unsafeDrop i v')
         | otherwise            = Nothing
         where a = unsafeIndex v  i
               b = unsafeIndex v' i
--
--
-- -- === Conversions === --
--
-- type instance Item Text = Char
--

-- FIXME[WD]: remove when we hit next LTS stage
instance Unboxed.Unbox a => Semigroup (Unboxed.Vector a) where (<>) = P.mappend

-- instance Convertible Char       Text       where convert = singleton              ; {-# INLINE convert #-}
instance (Vector Unboxed.Vector a, Convertible' Char a) => IsString         (Unboxed.Vector a) where fromString = convert              ; {-# INLINE fromString #-}
instance (Vector Unboxed.Vector a, Convertible' Char a) => Convertible Char (Unboxed.Vector a) where convert    = singleton . convert' ; {-# INLINE convert    #-}

instance (Vector Unboxed.Vector a, Convertible' t a) => Convertible [t] (Unboxed.Vector a) where convert = V.fromList . fmap convert' ; {-# INLINE convert #-}
instance (Vector Unboxed.Vector a, Convertible' a t) => Convertible (Unboxed.Vector a) [t] where convert = fmap convert' . V.toList   ; {-# INLINE convert #-}
-- instance Convertible Text       [Char]     where convert = Vector.toList . unwrap ; {-# INLINE convert #-}
instance (Vector Unboxed.Vector a, Convertible' Char a) => Convertible UTF16.Text         (Unboxed.Vector a) where convert = convertVia @[Char]     ; {-# INLINE convert #-}
instance (Vector Unboxed.Vector a, Convertible' a Char) => Convertible (Unboxed.Vector a) UTF16.Text         where convert = convertVia @[Char]     ; {-# INLINE convert #-}
--
-- instance FromList    Text where fromList   = convert ; {-# INLINE fromList   #-}
-- instance ToList      Text where toList     = convert ; {-# INLINE toList     #-}
-- instance IsString    Text where fromString = convert ; {-# INLINE fromString #-}
-- instance Exts.IsList Text where
--     type Item Text = Char
--     fromList = fromList ; {-# INLINE fromList #-}
--     toList   = toList   ; {-# INLINE toList   #-}
--
--
-- -- === Instances === --
--
-- instance Show Text where show = show . toList ; {-# INLINE show #-}
