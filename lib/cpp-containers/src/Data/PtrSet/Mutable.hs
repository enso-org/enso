{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.PtrSet.Mutable where

import Prologue hiding (convert, convert', fromList, null, toList)

import qualified Data.Construction          as Data
import qualified Data.Convert2              as Convert
import qualified Foreign.DynamicStorable    as DynamicStorable
import qualified Foreign.Storable.Utils     as Storable
import qualified Foreign.Storable1.Deriving as Storable1

import Data.Convert2           (convert')
import Foreign.DynamicStorable (DynamicStorable)
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Ptr.Utils       (SomePtr, fromCBool)
import Foreign.Storable        (Storable)
import Foreign.Storable.Utils  (castPeekAndOffset, castPokeAndOffset)



type IsPtr a = Convert.Bi' SomePtr a



--------------------
-- === PtrSet === --
--------------------

-- === Definition === --

newtype UnmanagedPtrSet a = UnmanagedPtrSet SomePtr deriving (Storable, Eq)
makeLenses       ''UnmanagedPtrSet
Storable1.derive ''UnmanagedPtrSet

class IsPtrSet t where
    newIO  :: ∀ a. IO (t a)
    withIO :: ∀ a b. t a -> (UnmanagedPtrSet a -> IO b) -> IO b


-- === Foreign calls === --

foreign import ccall unsafe "c_set_create"
    c_createPtrSet :: IO (UnmanagedPtrSet a)

foreign import ccall unsafe "c_set_insert"
    c_insert :: SomePtr -> UnmanagedPtrSet a -> IO ()

foreign import ccall unsafe "c_set_member"
    c_member :: SomePtr -> UnmanagedPtrSet a -> IO Int

foreign import ccall unsafe "c_set_delete"
    c_delete :: SomePtr -> UnmanagedPtrSet a -> IO ()

foreign import ccall "c_set_delete_set"
    c_deleteSet :: UnmanagedPtrSet a -> IO ()

foreign import ccall unsafe "c_set_to_list"
    c_toList :: Ptr SomePtr -> UnmanagedPtrSet a -> IO ()

foreign import ccall unsafe "c_set_size"
    c_size :: UnmanagedPtrSet a -> IO Int

foreign import ccall unsafe "c_set_null"
    c_null :: UnmanagedPtrSet a -> IO Int



-- === API === --

-- | A utility function for easily using the C calls that use `Ptr ()`
--   with the PtrSet.
with :: (IsPtrSet s, MonadIO m) => s a -> (UnmanagedPtrSet a -> IO b) -> m b
with !s !f = liftIO $ withIO s f ; {-# INLINE with #-}

-- -- | A utility function to perform a foreign C call on the set and return
-- --   the resulting set.
-- map :: (IsPtrSet s, MonadIO m)
--     => s -> (UnmanagedPtrSet -> IO a) -> m s
-- map s f = with s f >> return s ; {-# INLINE map #-}

-- | Creates an `std::set` object in C++ and attaches a finalizer.
--   The memory will be automatically deallocated by Haskell's GC.
new :: (IsPtrSet s, MonadIO m) => m (s a)
new = liftIO newIO ; {-# INLINE new #-}

-- new' :: IsPtrSet s => (s a)
-- new' = unsafeIO new ; {-# NOINLINE new' #-}

free :: (IsPtrSet s, MonadIO m) => s a -> m ()
free s = with s c_deleteSet ; {-# INLINE free #-}

-- | Create a set with one element
singleton :: (IsPtr a, IsPtrSet s, MonadIO m) => a -> m (s a)
singleton e = do
    s <- new
    insert s e
    pure s
{-# INLINE singleton #-}

-- | Insert a single element into the set.
insert :: (IsPtr a, IsPtrSet s, MonadIO m) => s a -> a -> m ()
insert s e = with s $ c_insert (convert' e) ; {-# INLINE insert #-}

-- | Bulk-insert a list of elements into the set.
insertMany :: (IsPtr a, IsPtrSet s, MonadIO m) => s a -> [a] -> m ()
insertMany s es = mapM_ (insert s) es ; {-# INLINE insertMany #-}

-- | Check whether the set contains a given key.
--   Since the set can only contain 'Int' values,
--   This is also equivalent to the `find` function.
member :: (IsPtr a, IsPtrSet s, MonadIO m) => s a -> a -> m Bool
member s e = fromCBool =<< with s (c_member $ convert' e) ; {-# INLINE member #-}

-- | Remove an element from the set, using `std::set::erase`.
delete :: (IsPtr a, IsPtrSet s, MonadIO m) => s a -> a -> m ()
delete s e = with s $ c_delete $ convert' e ; {-# INLINE delete #-}

-- | Return the number of elements in the set.
size :: (IsPtrSet s, MonadIO m) => s a -> m Int
size s = with s c_size ; {-# INLINE size #-}

null :: (IsPtrSet s, MonadIO m) => s a -> m Bool
null s = fromCBool =<< with s c_null ; {-# INLINE null #-}

-- | Get all of the elements from the set (in ascending order).
--   Note: do not overuse this function, as it will not perform very well.
toList :: (IsPtr a, IsPtrSet s, MonadIO m) => s a -> m [a]
toList !s = do
    n <- size s
    with s $ \ptr ->
        allocaArray @SomePtr n $ \arr -> do
            c_toList arr ptr
            Convert.from' <<$>> peekArray n arr
{-# INLINE toList #-}

-- | A version of `toList` that fills a pre-allocated chunk instead
--   of allocating one by itself.
fillList :: (IsPtr a, IsPtrSet t, MonadIO m) => t a -> SomePtr -> m ()
fillList t arr = with t (c_toList $ castPtr arr)
{-# INLINE fillList #-}

-- | Convert a list to the set.
fromList :: (IsPtr a, IsPtrSet s, MonadIO m) => [a] -> m (s a)
fromList es = do
    s <- new
    insertMany s es
    pure s
{-# INLINE fromList #-}




-- === Instances === --


instance IsPtrSet UnmanagedPtrSet where
    newIO        = c_createPtrSet ; {-# INLINE newIO  #-}
    withIO !s !f = f s            ; {-# INLINE withIO #-}

-- type instance Set.Item (UnmanagedPtrSet a) = a
-- instance (IsPtr a, MonadIO m)
--       => Set.Set m (UnmanagedPtrSet a) where
--     new    = new    ; {-# INLINE new #-}
--     insert = insert ; {-# INLINE insert #-}
--     delete = delete ; {-# INLINE delete #-}
--     member = member ; {-# INLINE member #-}
--     size   = size   ; {-# INLINE size   #-}
--     null   = null   ; {-# INLINE null   #-}
--     toList = toList ; {-# INLINE toList #-}

instance MonadIO m => Data.Constructor1 m () UnmanagedPtrSet where
    construct1 _ = new ; {-# INLINE construct1 #-}

instance MonadIO m => Data.Destructor1 m UnmanagedPtrSet where
    destruct1 = free ; {-# INLINE destruct1 #-}

instance (Storable a, IsPtr a) => DynamicStorable (UnmanagedPtrSet a) where
    sizeOf = \a -> do
        elems <- size a
        let headerSize = Storable.sizeOf' @Int
            bodySize   = elems * Storable.sizeOf' @a
        pure $! headerSize + bodySize
    {-# INLINE sizeOf #-}

    peek = \ptr -> do
        (elems, srcBodyPtr) <- castPeekAndOffset @Int ptr
        list <- peekArray elems (Storable.castPtrTo @a srcBodyPtr)
        fromList list
    {-# INLINE peek #-}

    poke = \ptr a -> do
        elems <- size a
        let _ = elems * Storable.sizeOf' @a
        tgtBodyPtr <- castPokeAndOffset @Int ptr elems
        fillList a tgtBodyPtr
    {-# INLINE poke #-}

instance NFData (UnmanagedPtrSet a) where
    rnf _ = ()

