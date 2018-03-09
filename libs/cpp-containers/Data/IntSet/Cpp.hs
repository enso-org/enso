module Data.IntSet.Cpp where

import Control.Monad.IO.Class
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Prelude

import Foreign.Utils (fromCBool)


---------------------------------------------------
-- === Newtype wrappers for the set pointers === --
---------------------------------------------------

newtype StdSet = StdSet { toRawSet :: ForeignPtr StdSet }

newtype RawSetPtr = RawSetPtr { toRawPtr :: Ptr RawSetPtr }



---------------------------
-- === Foreign calls === --
---------------------------

foreign import ccall unsafe "c_set_create"
    c_createStdSet :: IO RawSetPtr

foreign import ccall unsafe "c_set_insert"
    c_insert :: Int -> RawSetPtr -> IO ()

foreign import ccall unsafe "c_set_member"
    c_member :: Int -> RawSetPtr -> IO Int

foreign import ccall unsafe "c_set_delete"
    c_delete :: Int -> RawSetPtr -> IO ()

foreign import ccall unsafe "&c_set_delete_set"
    c_deleteSet :: FunPtr (Ptr RawSetPtr -> IO ())

foreign import ccall unsafe "c_set_to_list"
    c_toList :: Ptr Int -> RawSetPtr -> IO ()

foreign import ccall unsafe "c_set_size"
    c_size :: RawSetPtr -> IO Int

foreign import ccall unsafe "c_set_null"
    c_null :: RawSetPtr -> IO Int



--------------------------------------------
-- === Haskell wrappers for C++ calls === --
--------------------------------------------

-- | A utility function for easily using the C calls that use `Ptr ()` with the StdSet.
withStdSet :: MonadIO m => StdSet -> (RawSetPtr -> IO a) -> m a
withStdSet s f = liftIO $ withForeignPtr (toRawSet s) (f . RawSetPtr . castPtr)
{-# INLINE withStdSet #-}

-- | A utility function to perform a foreign C call on the set and return the resulting set.
mapStdSet :: StdSet -> (RawSetPtr -> IO a) -> IO StdSet
mapStdSet s f = withStdSet s f >> return s
{-# INLINE mapStdSet #-}

-- | Creates an `std::set` object in C++ and attaches a finalizer.
--   The memory will be automatically deallocated by Haskell's GC.
createStdSet :: MonadIO m => m StdSet
createStdSet = liftIO $ do
    ptr        <- c_createStdSet
    foreignPtr <- newForeignPtr c_deleteSet (toRawPtr ptr)
    return . StdSet $ castForeignPtr foreignPtr
{-# INLINE createStdSet #-}

-- | Create an empty Cpp IntSet
empty :: MonadIO m => m StdSet
empty = createStdSet
{-# INLINE empty #-}

-- | Create a set with one element
singleton :: MonadIO m => Int -> m StdSet
singleton e = do
    s <- empty
    insert s e
    return s
{-# INLINE singleton #-}

-- | Insert a single element into the set.
insert :: MonadIO m => StdSet -> Int -> m ()
insert s e = withStdSet s (c_insert e)
{-# INLINE insert #-}

-- | Bulk-insert a list of elements into the set.
insertMany :: MonadIO m => StdSet -> [Int] -> m ()
insertMany s es = mapM_ (insert s) es
{-# INLINE insertMany #-}

-- | Check whether the set contains a given key.
--   Since the set can only contain 'Int' values,
--   This is also equivalent to the `find` function.
member :: MonadIO m => StdSet -> Int -> m Bool
member s e = fromCBool =<< withStdSet s (c_member e)
{-# INLINE member #-}

-- | Remove an element from the set, using `std::set::erase`.
delete :: MonadIO m => StdSet -> Int -> m ()
delete s e = withStdSet s (c_delete e)
{-# INLINE delete #-}

-- | Return the number of elements in the set.
size :: MonadIO m => StdSet -> m Int
size s = withStdSet s c_size
{-# INLINE size #-}

null :: MonadIO m => StdSet -> m Bool
null s = fromCBool =<< withStdSet s c_null
{-# INLINE null #-}

-- | Get all of the elements from the set (in ascending order).
--   Note: do not overuse this function, as it will not perform very well.
toList :: MonadIO m => StdSet -> m [Int]
toList s = do
    n <- size s
    withStdSet s (\ptr ->
        allocaArray @Int n (\arr -> do
            c_toList arr ptr
            peekArray n arr))
{-# INLINE toList #-}

-- | Convert a list to the set.
fromList :: MonadIO m => [Int] -> m StdSet
fromList es = do
    s <- empty
    insertMany s es
    return s
{-# INLINE fromList #-}
