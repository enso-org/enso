module Data.Vector.Storable.Foreign where

import Prologue hiding (empty, fromList, toList, unsafeRead)

import qualified Data.Construction         as Data
import qualified Data.List                 as List
import qualified Foreign.Marshal.Alloc     as Mem
import qualified Foreign.Storable.Deriving as Storable
import qualified Foreign.Storable.Utils    as Storable

import Foreign.Ptr      (Ptr, nullPtr)
import Foreign.Storable (Storable)
import System.IO.Unsafe (unsafeDupablePerformIO, unsafePerformIO)


--------------------
-- === Vector === --
--------------------

-- === Definition === --

data Vector a = Vector
    { _size :: !Int
    , _ptr  :: !(Ptr a)
    } deriving (Eq)
makeLenses      ''Vector
Storable.derive ''Vector


-- === Construction === --

new :: ∀ a m. (MonadIO m, Storable a) => Int -> m (Vector a)
new elNum = fmap (Vector elNum) . liftIO . Mem.mallocBytes
          $ elNum * Storable.sizeOf' @a
{-# INLINE new #-}

empty :: ∀ a. Vector a
empty = Vector 0 nullPtr ; {-# INLINE empty #-}

free :: MonadIO m => Vector a -> m ()
free = liftIO . Mem.free . view ptr ; {-# INLINE free #-}


-- === Lookup === --

unsafeRead :: ∀ a m. (MonadIO m, Storable a) => Vector a -> Int -> m a
unsafeRead v off = liftIO $ Storable.peekElemOff (v ^. ptr) off ; {-# INLINE unsafeRead #-}

unsafeWrite :: ∀ a m. (MonadIO m, Storable a) => Vector a -> Int -> a -> m ()
unsafeWrite v off a = liftIO $ Storable.pokeElemOff (v ^. ptr) off a ; {-# INLINE unsafeWrite #-}


-- === List === --

fromList :: Storable a => [a] -> Vector a
fromList lst = unsafeDupablePerformIO $ do
    v <- new (List.length lst)
    mapM_ (uncurry $ unsafeWrite v) $ zip [0..] lst
    pure v
{-# NOINLINE fromList #-}

toList :: (MonadIO m, Storable a) => Vector a -> m [a]
toList v = mapM (unsafeRead v) [0 .. (v ^. size) - 1] ; {-# INLINE toList #-}


-- === Conversions === --

type instance Item (Vector a) = a
instance (Convertible' a b, Storable b) => Convertible [a] (Vector b) where
    convert = fromList . fmap convert' ; {-# INLINE convert #-}


-- === Debug === --

instance (Show a, Storable a) => Show (Vector a) where
    show = show . unsafePerformIO . toList ; {-# NOINLINE show #-}

instance Mempty (Vector a) where mempty = empty ; {-# INLINE mempty #-}


-- === Instances === --

instance MonadIO m => Data.Destructor1 m Vector where
    destruct1 = free ; {-# INLINE destruct1 #-}
