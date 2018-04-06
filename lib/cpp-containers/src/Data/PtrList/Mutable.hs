-----------------------------------------------------------------------------
-- |
-- Module      :  Data.IntList.Cpp
--
-- A Haskell wrapper for C++'s std::deque, providing fast front- and back-
-- insertion and fast random access. Expect operations like:
-- `head` and `last` to be constant-time.
--
-----------------------------------------------------------------------------

{-# LANGUAGE TupleSections #-}
module Data.PtrList.Mutable where

import Prologue hiding (null, unsafeHead, unsafeLast)

import Control.Monad          ((<=<))
import Control.Monad.IO.Class
import Data.Maybe             (fromJust)
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Ptr.Utils
import Foreign.Storable       (Storable)



-- class IsPtr a where
--     asSomePtr :: Iso' a SomePtr
--     default asSomePtr :: Coercible a SomePtr => Iso' a SomePtr
--     asSomePtr = iso coerce coerce ; {-# INLINE asSomePtr #-}

-- instance IsPtr (Ptr a)


type IsPtr a = BiConvertible' SomePtr a

-- instance Convertible

---------------------------------------------------
-- === Newtype wrappers for the list pointers === --
---------------------------------------------------

-- === Definition === --

newtype UnmanagedPtrList a = UnmanagedPtrList SomePtr deriving (Storable)
makeLenses ''UnmanagedPtrList

class IsPtrList t where
    newIO  :: ∀ a. IO (t a)
    withIO :: ∀ a b. t a -> (UnmanagedPtrList a -> IO b) -> IO b


-- === Foreign calls === --

foreign import ccall unsafe "c_list_create"
    c_new :: IO (UnmanagedPtrList a)

foreign import ccall unsafe "c_list_push_front"
    c_pushFront :: SomePtr -> UnmanagedPtrList a -> IO ()

foreign import ccall unsafe "c_list_push_back"
    c_pushBack :: SomePtr -> UnmanagedPtrList a -> IO ()

foreign import ccall unsafe "c_list_pop_front"
    c_popFront :: UnmanagedPtrList a -> IO SomePtr

foreign import ccall unsafe "c_list_pop_back"
    c_popBack :: UnmanagedPtrList a -> IO SomePtr

foreign import ccall unsafe "c_list_at"
    c_at :: Int -> UnmanagedPtrList a -> IO SomePtr

foreign import ccall unsafe "c_list_front"
    c_front :: UnmanagedPtrList a -> IO SomePtr

foreign import ccall unsafe "c_list_back"
    c_back :: UnmanagedPtrList a -> IO SomePtr

foreign import ccall unsafe "c_list_delete_list"
    c_deleteList :: UnmanagedPtrList a -> IO ()

foreign import ccall unsafe "c_list_to_list"
    c_toList :: Ptr Int -> UnmanagedPtrList a -> IO ()

foreign import ccall unsafe "c_list_size"
    c_size :: UnmanagedPtrList a -> IO Int

foreign import ccall unsafe "c_list_null"
    c_null :: UnmanagedPtrList a -> IO Int


-- -- === API === --

-- | A utility function for easily using the C calls that use `Ptr ()` with the UnmanagedPtrList x.
with :: (IsPtrList t, MonadIO m) => t a -> (UnmanagedPtrList a -> IO b) -> m b
with s f = liftIO $ withIO s f ; {-# INLINE with #-}

-- -- | A utility function to perform a foreign C call on the list and return the resulting list.
-- map :: (IsPtrList t, MonadIO m) => UnmanagedPtrList x -> (UnmanagedPtrList -> IO a) -> m UnmanagedPtrList x
-- map s f = with s f >> return s ; {-# INLINE map #-}

-- | A utility for wrapping unsafe indexing calls
whenNonEmpty :: (IsPtrList t, MonadIO m) => (t a -> m b) -> t a -> m (Maybe b)
whenNonEmpty indexer t = null t >>= \case
    True  -> return Nothing
    False -> Just <$> indexer t
{-# INLINE whenNonEmpty #-}

-- | Creates an `std::deque` object in C++ and attaches a finalizer.
--   The memory will be automatically deallocated by Haskell's GC.
new :: (IsPtrList t, MonadIO m) => m (t a)
new = liftIO newIO ; {-# INLINE new #-}

free :: (IsPtrList t, MonadIO m) => t a -> m ()
free t = with t c_deleteList ; {-# INLINE free #-}

-- | Prepend a single element to the front of the list (O(1))
pushFront :: (IsPtrList t, MonadIO m, IsPtr a) => t a -> a -> m ()
pushFront t a = with t (c_pushFront $ convert' a) ; {-# INLINE pushFront #-}

-- | Prepend a single element to the front of the list (O(1))
pushBack :: (IsPtrList t, MonadIO m, IsPtr a) => t a -> a -> m ()
pushBack t a = with t (c_pushBack $ convert' a) ; {-# INLINE pushBack #-}

unsafePopFront :: (IsPtrList t, MonadIO m, IsPtr a) => t a -> m a
unsafePopFront t = convert' <$> with t c_popFront ; {-# INLINE unsafePopFront #-}

-- | Remove the first element of the list, returning it (O(1))
popFront :: (IsPtrList t, MonadIO m, IsPtr a) => t a -> m (Maybe a)
popFront = whenNonEmpty unsafePopFront ; {-# INLINE popFront #-}

unsafePopBack :: (IsPtrList t, MonadIO m, IsPtr a) => t a -> m a
unsafePopBack t = convert' <$> with t c_popBack ; {-# INLINE unsafePopBack #-}

-- | Remove the last element of the list, returning it (O(1))
popBack :: (IsPtrList t, MonadIO m, IsPtr a) => t a -> m (Maybe a)
popBack = whenNonEmpty unsafePopBack ; {-# INLINE popBack #-}

-- | Peek the first element of the list (without removing)
head :: (IsPtrList t, MonadIO m, IsPtr a) => t a -> m (Maybe a)
head = whenNonEmpty unsafeHead ; {-# INLINE head #-}

unsafeHead :: (IsPtrList t, MonadIO m, IsPtr a) => t a -> m a
unsafeHead t = convert' <$> with t c_front ; {-# INLINE unsafeHead #-}

-- | Peek the last element of the list (without removing)
last :: (IsPtrList t, MonadIO m, IsPtr a) => t a -> m (Maybe a)
last = whenNonEmpty unsafeLast ; {-# INLINE last #-}

unsafeLast :: (IsPtrList t, MonadIO m, IsPtr a) => t a -> m a
unsafeLast t = convert' <$> with t c_back ; {-# INLINE unsafeLast #-}


-- -- | Bulk-insert a list of elements into the list. (O(n))
-- insertMany :: (IsPtrList t, MonadIO m) => UnmanagedPtrList x -> [Int] -> m ()
-- insertMany l = mapM_ (pushBack l)
-- {-# INLINE insertMany #-}

-- -- | Get an element under a given index.
-- --   Returns `Just v` if the value exists, `Nothing` otherwise.
-- index :: (IsPtrList t, MonadIO m) => UnmanagedPtrList x -> Int -> m (Maybe Int)
-- index l idx = do
--     n <- length l
--     if idx >= n
--         then return Nothing
--         else Just <$> with l (c_at idx)
-- {-# INLINE index #-}

-- -- | Return the number of elements in the list.
-- length :: (IsPtrList t, MonadIO m) => UnmanagedPtrList x -> m Int
-- length l = with l c_size
-- {-# INLINE length #-}

null :: (IsPtrList t, MonadIO m) => t a -> m Bool
null t = fromCBool =<< with t c_null ; {-# INLINE null #-}

-- -- | Get all of the elements from the list as a pure haskell list.
-- --   Note: do not overuse this function, as it will not perform very well.
-- toList :: (IsPtrList t, MonadIO m) => UnmanagedPtrList x -> m [Int]
-- toList l = do
--     n <- length l
--     with l (\ptr ->
--         allocaArray @Int n (\arr -> do
--             c_toList arr ptr
--             peekArray n arr))
-- {-# INLINE toList #-}

-- -- | Convert a pure haskell list to the c++'s std::list.
-- fromList :: (IsPtrList t, MonadIO m) => [Int] -> m UnmanagedPtrList x
-- fromList es = do
--     l <- empty
--     insertMany l es
--     return l
-- {-# INLINE fromList #-}

-- -- | Deconstruct the list into the first element and the rest.
-- --   Safe call: will return Maybe.
-- uncons :: (IsPtrList t, MonadIO m) => UnmanagedPtrList x -> m (Maybe (Int, [Int]))
-- uncons l = do
--     -- TODO[piotrMocz]: it's not necessary to compute `tl` if hd is `Nothing`.
--     hd <- popFront l
--     tl <- Just <$> toList l
--     return $ (,) <$> hd <*> tl
-- {-# INLINE uncons #-}

-- init, tail :: (IsPtrList t, MonadIO m) => UnmanagedPtrList x -> m UnmanagedPtrList x
-- init l = popBack l >> return l   ; {-# INLINE init #-}
-- tail l = popFront l >> return l  ; {-# INLINE tail #-}

-- initSafe, tailSafe :: (IsPtrList t, MonadIO m) => UnmanagedPtrList x -> m (Maybe UnmanagedPtrList x)
-- initSafe = whenNonEmpty init        ; {-# INLINE initSafe #-}
-- tailSafe = whenNonEmpty tail        ; {-# INLINE tailSafe #-}

-- ---------------------
-- -- === Aliases === --
-- ---------------------

-- cons :: (IsPtrList t, MonadIO m) => Int -> UnmanagedPtrList x -> m UnmanagedPtrList x
-- cons el l = pushFront l el >> return l
-- {-# INLINE cons #-}

-- head, last :: (IsPtrList t, MonadIO m) => UnmanagedPtrList x -> m Int
-- head = unsafeHead ;  {-# INLINE head #-}
-- last = backUnsafe  ;  {-# INLINE last #-}

-- headSafe, lastSafe :: (IsPtrList t, MonadIO m) => UnmanagedPtrList x -> m (Maybe Int)
-- headSafe = front   ;  {-# INLINE headSafe #-}
-- lastSafe = back    ;  {-# INLINE lastSafe #-}


-- === Instances ===  --

instance IsPtrList UnmanagedPtrList where
    newIO        = c_new ; {-# INLINE newIO  #-}
    withIO !s !f = f s             ; {-# INLINE withIO #-}
