module Data.IntSet.Cpp where

import Prelude
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr


---------------------------------------------------
-- === Newtype wrappers for the set pointers === --
---------------------------------------------------

newtype StdSet = StdSet { toRawSet :: ForeignPtr StdSet }

newtype RawSetPtr = RawSetPtr { toRawPtr :: Ptr RawSetPtr }



---------------------------
-- === Foreign calls === --
---------------------------

foreign import ccall unsafe "c_create_std_set"
    c_createStdSet :: IO RawSetPtr

foreign import ccall unsafe "c_insert"
    c_insert :: Int -> RawSetPtr -> IO ()

foreign import ccall unsafe "c_insert_many"
    c_insertMany :: Ptr Int -> Int -> RawSetPtr -> IO ()

foreign import ccall unsafe "c_member"
    c_member :: Int -> RawSetPtr -> IO Bool

foreign import ccall unsafe "c_delete"
    c_delete :: Int -> RawSetPtr -> IO ()

foreign import ccall unsafe "&c_delete_set"
    c_deleteSet :: FunPtr (Ptr RawSetPtr -> IO ())

foreign import ccall unsafe "c_to_list"
    c_toList :: Ptr Int -> RawSetPtr -> IO ()

foreign import ccall unsafe "c_size"
    c_size :: RawSetPtr -> IO Int

foreign import ccall unsafe "c_null"
    c_null :: RawSetPtr -> IO Bool



--------------------------------------------
-- === Haskell wrappers for C++ calls === --
--------------------------------------------

-- | A utility function for easily using the C calls that use `Ptr ()` with the StdSet.
withStdSet' :: (RawSetPtr -> IO a) -> StdSet -> IO a
withStdSet' f s = withForeignPtr (toRawSet s) (f . RawSetPtr . castPtr)

-- | A utility function to perform a foreign C call on the set and return the resulting set.
withStdSet :: (RawSetPtr -> IO a) -> StdSet -> IO StdSet
withStdSet f s = withStdSet' f s >> return s

-- | Creates an `std::set` object in C++ and attaches a finalizer.
--   The memory will be automatically deallocated by Haskell's GC.
createStdSet :: IO StdSet
createStdSet = do
    ptr        <- c_createStdSet
    foreignPtr <- newForeignPtr c_deleteSet (toRawPtr ptr)
    return $ StdSet $ castForeignPtr foreignPtr

-- | Create an empty Cpp IntSet
empty :: IO StdSet
empty = createStdSet

-- | Create a set with one element
singleton :: Int -> IO StdSet
singleton e = empty >>= insert e

-- | Insert a single element into the set.
insert :: Int -> StdSet -> IO StdSet
insert e = withStdSet (c_insert e)

-- | Bulk-insert a list of elements into the set.
insertMany :: [Int] -> StdSet -> IO StdSet
insertMany es = withStdSet (\ptr ->
    withArrayLen es (\n esPtr ->
        c_insertMany esPtr n ptr))

-- | Check whether the set contains a given key.
--   Since the set can only contain 'Int' values,
--   This is also equivalent to the `find` function.
member :: Int -> StdSet -> IO Bool
member e = withStdSet' (c_member e)

-- | Remove an element from the set, using `std::set::erase`.
delete :: Int -> StdSet -> IO StdSet
delete e = withStdSet (c_delete e)

-- | Return the number of elements in the set.
size :: StdSet -> IO Int
size = withStdSet' c_size

-- | Get all of the elements from the set (in ascending order).
--   Note: do not overuse this function, as it will not perform very well.
toList :: StdSet -> IO [Int]
toList s = do
    n <- size s
    withStdSet' (\ptr ->
        allocaArray @Int n (\arr -> do
            c_toList arr ptr
            peekArray n arr)) s

-- | Convert a list to the set.
fromList :: [Int] -> IO StdSet
fromList es = empty >>= insertMany es
