{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Mutable.Storable.SmallSet
    (module Data.Mutable.Storable.SmallSet, module X) where
import Data.Mutable.Class as X

import Prologue hiding (FromList, Read, ToList, fromList, length, mapM, toList,
                 unsafeRead)

import qualified Data.Construction                     as Data
import qualified Data.Mutable.Class2                   as Mutable
import qualified Data.Mutable.Plain                    as Data
import qualified Data.Mutable.Storable.SmallAutoVector as SmallVector
import qualified Data.Storable                         as Struct
import qualified Foreign.Storable                      as StdStorable
import qualified Foreign.Storable.Class                as Storable
import qualified Memory                                as Memory
import qualified Type.Known                            as Type

import Data.Mutable.Storable.SmallAutoVector (MemChunk, UnmanagedSmallVectorA)
import Foreign.Storable.Class                (Copy, Storable, View)
import System.IO.Unsafe                      (unsafeDupablePerformIO)



----------------------
-- === SmallSet === --
----------------------

-- === Definition === --

type    SmallSet = SmallSetA Memory.StdAllocator
newtype SmallSetA (alloc :: Memory.Allocator) (n :: Nat) a
      = SmallSet (SmallSet__ alloc n a)
    deriving (Eq, Ord, NFData, Free m)
type SmallSet__ = UnmanagedSmallVectorA
makeLenses ''SmallSetA

type instance Item (SmallSetA alloc n a) = a
type instance Memory.Management (SmallSetA alloc n a) = 'Memory.Unmanaged


-- === Utils === --

type LookupAndApp m alloc n a = (MonadIO m, Ord a, Read m (SmallSetA alloc n a))

lookupAndApp :: LookupAndApp m alloc n a
    => (Int -> m ()) -> (Int -> m ()) -> SmallSetA alloc n a -> a -> m ()
lookupAndApp = \ffound fmissing s a -> do
    siz <- size s
    if siz == 0 then fmissing 0
    else let min = 0
             max = siz - 1
             ix  = max `quot` 2
         in  lookupAndApp__ ffound fmissing s a min ix max
{-# INLINE lookupAndApp #-}

lookupAndApp__ :: LookupAndApp m alloc n a
    => (Int -> m ()) -> (Int -> m ())
    -> SmallSetA alloc n a -> a -> Int -> Int -> Int -> m ()
lookupAndApp__ ffound fmissing s a = go where
    go = \min ix max -> do
        ixVal <- unsafeRead s ix
        if max <= min then
             if      a < ixVal then fmissing ix
             else if a > ixVal then fmissing (ix + 1)
             else                   ffound   ix
        else if a < ixVal then
                 let max' = ix - 1
                     ix'  = (min + max') `quot` 2
                 in  go min ix' max'
             else if a > ixVal then
                 let min' = ix + 1
                     ix'  = (min' + max) `quot` 2
                 in  go min' ix' max
             else ffound ix
    {-# INLINABLE go #-}
{-# INLINE lookupAndApp__ #-}


-- === API Instances === --

deriving instance Data.CopyInitializer m (SmallSet__ alloc n a)
               => Data.CopyInitializer m (SmallSetA alloc n a)

deriving instance Mutable.Unswizzle m (SmallSet__ alloc n a)
               => Mutable.Unswizzle m (SmallSetA alloc n a)

instance Storable.KnownConstantSize (SmallSet__ alloc n a)
      => Storable.KnownConstantSize (SmallSetA alloc n a) where
    constantSize = Storable.constantSize @(SmallSet__ alloc n a)
    {-# INLINABLE constantSize #-}

instance Storable.KnownSize t m (SmallSet__ alloc n a)
      => Storable.KnownSize t m (SmallSetA alloc n a) where
    size = Storable.size @t . unwrap
    {-# INLINE size #-}

instance (PlacementNew m (SmallSet__ alloc n a), Functor m)
      => PlacementNew m (SmallSetA alloc n a) where
    placementNew = fmap wrap . placementNew . Memory.coercePtr
    {-# INLINE placementNew #-}

instance (New m (SmallSet__ alloc n a), Functor m)
      => New m (SmallSetA alloc n a) where
    new = wrap <$> new
    {-# INLINE new #-}

instance Size m (SmallSet__ alloc n a)
      => Size m (SmallSetA alloc n a) where
    size = size . unwrap
    {-# INLINE size #-}

instance Capacity m (SmallSet__ alloc n a)
      => Capacity m (SmallSetA alloc n a) where
    capacity = capacity . unwrap
    {-# INLINE capacity #-}

instance Read m (SmallSet__ alloc n a)
      => Read m (SmallSetA alloc n a) where
    unsafeRead = unsafeRead . unwrap
    {-# INLINE unsafeRead #-}

instance (InsertAt m (SmallSet__ alloc n a), LookupAndApp m alloc n a)
      => Insert m (SmallSetA alloc n a) where
    insert = \a v -> lookupAndApp (\_ -> pure ())
                     (\ix -> insertAt (unwrap a) ix v) a v
    {-# INLINE insert #-}

instance (RemoveAt m (SmallSet__ alloc n a), LookupAndApp m alloc n a)
      => Remove m (SmallSetA alloc n a) where
    remove = \a v -> lookupAndApp (removeAt (unwrap a))
                     (\_ -> pure ()) a v
    {-# INLINE remove #-}

instance (FromList m (SmallSet__ alloc n a), Functor m)
      => FromList m (SmallSetA alloc n a) where
    fromList = fmap wrap . fromList
    {-# INLINE fromList #-}

instance ToList m (SmallSet__ alloc n a)
      => ToList m (SmallSetA alloc n a) where
    toList = toList . unwrap
    {-# INLINE toList #-}

instance Map m (SmallSet__ alloc n a)
      => Map m (SmallSetA alloc n a) where
    mapM = \a f -> mapM (unwrap a) f
    {-# INLINE mapM #-}


-- === Debug Instances === --

instance Show (SmallSet__ alloc n a)
      => Show (SmallSetA alloc n a) where
    show = show . unwrap


-- === Deprecated Instances === --

deriving instance StdStorable.Storable (SmallSet__ alloc n a)
    => StdStorable.Storable (SmallSetA alloc n a)

-- WARNING: this instance is strange. It does not release self-memory,
--          because it is used for placement-new objects
instance (Data.Destructor1 m (SmallSet__ alloc n), Monad m)
      => Data.Destructor1 m (SmallSetA alloc n) where
    destruct1 = Data.destruct1 . unwrap
    {-# INLINE destruct1 #-}

