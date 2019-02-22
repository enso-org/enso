{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Vector.Storable.Foreign where

import Prologue hiding (empty, fromList, toList, unsafeRead)

import qualified Data.Construction         as Data
import qualified Data.List                 as List
import qualified Data.Property             as Property
import qualified Foreign.DynamicStorable   as DynamicStorable
import qualified Foreign.Marshal.Alloc     as Mem
import qualified Foreign.Marshal.Utils     as Mem
import qualified Foreign.Storable.Deriving as Storable
import qualified Foreign.Storable.Utils    as Storable

import Foreign.DynamicStorable (DynamicStorable)
import Foreign.Ptr             (Ptr, nullPtr)
import Foreign.Storable        (Storable)
import Foreign.Storable.Utils  (castPeekAndOffset, castPokeAndOffset)
import Foreign.Storable.Utils  (Dynamic, Dynamics)



--------------------
-- === Vector === --
--------------------

-- === Definition === --

data Vector a = Vector
    { _size :: !Int
    , _ptr  :: !(Ptr a)
    } deriving (Show)
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

fromList :: (MonadIO m, Storable a) => [a] -> m (Vector a)
fromList lst = liftIO $ do
    v <- new (List.length lst)
    mapM_ (uncurry $ unsafeWrite v) $ zip [0..] lst
    pure v
{-# NOINLINE fromList #-}

toList :: (MonadIO m, Storable a) => Vector a -> m [a]
toList v = mapM (unsafeRead v) [0 .. (v ^. size) - 1] ; {-# INLINE toList #-}


-- === Debug === --

-- instance (Show a, Storable a) => Show (Vector a) where
--     show = show . unsafeIO . toList ; {-# NOINLINE show #-}

instance Mempty (Vector a) where mempty = empty ; {-# INLINE mempty #-}


-- === Instances === --

type instance Property.Get Dynamics (Vector a) = Dynamic

instance Eq (Vector a) where
    (==) = (==) `on` view ptr ; {-# INLINE (==) #-}

instance MonadIO m => Data.ShallowDestructor1 m Vector where
    destructShallow1 = free ; {-# INLINE destructShallow1 #-}

instance Storable a => DynamicStorable (Vector a) where
    sizeOf = \v -> let
        elems      = v ^. size
        headerSize = Storable.sizeOf' @Int
        bodySize   = elems * Storable.sizeOf' @a
        in pure $! headerSize + bodySize
    {-# INLINE sizeOf #-}

    peek = \ptr -> do
        (elems, srcBodyPtr) <- castPeekAndOffset @Int ptr
        let byteSize = elems * Storable.sizeOf' @a
        tgtBodyPtr <- Mem.mallocBytes byteSize
        Mem.copyBytes tgtBodyPtr srcBodyPtr byteSize
        pure $ Vector elems tgtBodyPtr
    {-# INLINE peek #-}

    poke = \ptr (Vector elems srcBodyPtr) -> do
        let bodyByteSize = elems * Storable.sizeOf' @a
        tgtBodyPtr <- castPokeAndOffset @Int ptr elems
        Mem.copyBytes tgtBodyPtr srcBodyPtr bodyByteSize
    {-# INLINE poke #-}

