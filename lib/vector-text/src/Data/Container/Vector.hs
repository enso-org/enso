{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Strict               #-}
{-# LANGUAGE CPP                  #-}

module Data.Container.Vector (module Data.Container.Vector, module X) where

import qualified Prelude as P
import qualified Data.Text as UTF16
import           Prologue hiding (Text, length, splitHead, concat, take, drop)

import           Control.Monad.ST            (runST)
import           Data.Vector.Binary          ()
import qualified Data.Vector.Unboxed         as Unboxed
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
unsafeSplitHead t = (V.unsafeHead t, V.unsafeTail t) ; {-# INLINE unsafeSplitHead #-}
unsafeSplitLast t = (V.unsafeLast t, V.unsafeInit t) ; {-# INLINE unsafeSplitLast #-}

-- | O(s)
takeTill :: Vector v a => (a -> Bool) -> v a -> v a
takeTill f = \v -> maybe v (flip V.unsafeTake v) $ findIndex f v ; {-# INLINE takeTill #-}

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



-- === Utils === --

commonPrefixes :: (Vector v a, Eq a) => v a -> v a -> Maybe (v a, v a, v a)
commonPrefixes v v' = go 0 where
    minlen = min (length v) (length v')
    go i | i < minlen && a == b = go (i+1)
         | i > 0                = Just (unsafeTake i v, unsafeDrop i v, unsafeDrop i v')
         | otherwise            = Nothing
         where a = unsafeIndex v  i
               b = unsafeIndex v' i


-- -- === Conversions === --


-- FIXME[WD]: remove when we hit next LTS stage
#if MIN_VERSION_vector(0,12,0)
-- instance present in vector
#else
instance Unboxed.Unbox a => Semigroup (Unboxed.Vector a) where (<>) = P.mappend
#endif

-- instance Convertible Char       Text       where convert = singleton              ; {-# INLINE convert #-}
instance (Vector Unboxed.Vector a, Convertible' Char a) => IsString         (Unboxed.Vector a) where fromString = convert              ; {-# INLINE fromString #-}
instance (Vector Unboxed.Vector a, Convertible' Char a) => Convertible Char (Unboxed.Vector a) where convert    = singleton . convert' ; {-# INLINE convert    #-}

-- | We cannot use automatic Convertible1 -> Convertible lifting, because converting unboxed Vectors constraints `a` to be unboxed as well.
instance {-# OVERLAPPABLE #-} (Vector Unboxed.Vector a, Convertible' t a) => Convertible [t] (Unboxed.Vector a) where convert = V.fromList . fmap convert' ; {-# INLINE convert #-}
instance                      (Vector Unboxed.Vector a)                   => Convertible [a] (Unboxed.Vector a) where convert = V.fromList                 ; {-# INLINE convert #-}
instance {-# OVERLAPPABLE #-} (Vector Unboxed.Vector a, Convertible' a t) => Convertible (Unboxed.Vector a) [t] where convert = fmap convert' . V.toList   ; {-# INLINE convert #-}
instance                      (Vector Unboxed.Vector a)                   => Convertible (Unboxed.Vector a) [a] where convert = V.toList                   ; {-# INLINE convert #-}

instance (Vector Unboxed.Vector a, Convertible' Char a) => Convertible UTF16.Text (Unboxed.Vector a) where convert = convertVia @[Char] ; {-# INLINE convert #-}
instance (Vector Unboxed.Vector a, Convertible' a Char) => Convertible (Unboxed.Vector a) UTF16.Text where convert = convertVia @[Char] ; {-# INLINE convert #-}
