module Data.IntMap.Cpp where

import Prelude
import Control.Monad          ((<=<))
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable (peek)

import Foreign.Utils

---------------------------------------------------
-- === Newtype wrappers for the map pointers === --
---------------------------------------------------

newtype StdMap = StdMap { toRawMap :: ForeignPtr StdMap }

newtype RawMapPtr = RawMapPtr { toRawPtr :: Ptr RawMapPtr }



---------------------------
-- === Foreign calls === --
---------------------------

foreign import ccall unsafe "c_map_create"
    c_createStdMap :: IO RawMapPtr

foreign import ccall unsafe "c_map_insert"
    c_insert :: Int -> Int -> RawMapPtr -> IO ()

foreign import ccall unsafe "c_map_insert_many"
    c_insertMany :: Ptr Int -> Ptr Int -> Int -> RawMapPtr -> IO ()

foreign import ccall unsafe "c_map_member"
    c_member :: Int -> RawMapPtr -> IO Int

foreign import ccall unsafe "c_map_lookup"
    c_lookup :: Int -> Ptr Int -> RawMapPtr -> IO Int

foreign import ccall unsafe "c_map_delete"
    c_delete :: Int -> RawMapPtr -> IO ()

foreign import ccall unsafe "&c_map_delete_map"
    c_deleteMap :: FunPtr (Ptr RawMapPtr -> IO ())

foreign import ccall unsafe "c_map_to_list"
    c_toList :: Ptr Int -> Ptr Int -> RawMapPtr -> IO ()

foreign import ccall unsafe "c_map_size"
    c_size :: RawMapPtr -> IO Int

foreign import ccall unsafe "c_map_null"
    c_null :: RawMapPtr -> IO Int



--------------------------------------------
-- === Haskell wrappers for C++ calls === --
--------------------------------------------

-- | A utility function for easily using the C calls that use `Ptr ()` with the StdMap.
withStdMap' :: (RawMapPtr -> IO a) -> StdMap -> IO a
withStdMap' f s = withForeignPtr (toRawMap s) (f . RawMapPtr . castPtr)

-- | A utility function to perform a foreign C call on the map and return the resulting map.
withStdMap :: (RawMapPtr -> IO a) -> StdMap -> IO StdMap
withStdMap f s = withStdMap' f s >> return s

-- | Creates an `std::map` object in C++ and attaches a finalizer.
--   The memory will be automatically deallocated by Haskell's GC.
createStdMap :: IO StdMap
createStdMap = do
    ptr        <- c_createStdMap
    foreignPtr <- newForeignPtr c_deleteMap (toRawPtr ptr)
    return $ StdMap $ castForeignPtr foreignPtr

-- | Create an empty Cpp IntMap
empty :: IO StdMap
empty = createStdMap

-- | Create a map with one element (one key and one value)
singleton :: Int -> Int -> IO StdMap
singleton k v = empty >>= insert k v

-- | Insert a single element into the map.
insert :: Int -> Int -> StdMap -> IO StdMap
insert k v = withStdMap (c_insert k v)

-- | Bulk-insert a list of pairs into the map.
insertMany :: [(Int, Int)] -> StdMap -> IO StdMap
insertMany ps = let (ks, vs) = unzip ps in
    withStdMap (\ptr ->
        withArrayLen ks (\n ksPtr ->
        withArray    vs (\  vsPtr ->
            c_insertMany ksPtr vsPtr n ptr)))

-- | Check whether the map contains a given key.
--   Since the map can only contain 'Int' values,
--   This is also equivalent to the `find` function.
member :: Int -> StdMap -> IO Bool
member k = fromCBool <=< withStdMap' (c_member k)

-- | Try to find the element in the map, returning Maybe elem.
lookup :: Int -> StdMap -> IO (Maybe Int)
lookup k = withStdMap' (\ptr ->
    alloca @Int (\res -> do
        exists <- fromCBool =<< c_lookup k res ptr
        if exists then Just <$> peek res else return Nothing))

-- | Remove an element under a given key from the map, using `std::map::erase`.
delete :: Int -> StdMap -> IO StdMap
delete k = withStdMap (c_delete k)

-- | Return the number of elements in the map.
size :: StdMap -> IO Int
size = withStdMap' c_size

-- | Is the map empty?
null :: StdMap -> IO Bool
null = fromCBool <=< withStdMap' c_null

-- | Get all of the elements from the map (in ascending order).
--   Note: do not overuse this function, as it will not perform very well.
toList :: StdMap -> IO [(Int, Int)]
toList m = do
    n <- size m
    withStdMap' (\ptr ->
        allocaArray @Int n (\ks -> do
        allocaArray @Int n (\vs -> do
            c_toList ks vs ptr
            zip <$> peekArray n ks <*> peekArray n vs))) m

-- | Convert a list to the map.
fromList :: [(Int, Int)] -> IO StdMap
fromList ps = empty >>= insertMany ps
