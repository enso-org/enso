module Luna.Core.Store where

import           Prelude                      hiding (length, (.))

import           Control.Exception.Base (assert)
import           Control.Lens.Utils
import           Control.Monad.Primitive
import           Data.Coerce
import           Data.Convert
import qualified Data.Vector.Storable         as Storable
import qualified Data.Vector.Storable         as Vector hiding (length)
import           Data.Vector.Storable         (Vector)
import qualified Data.Vector.Storable.Mutable as Storable
import qualified Data.Vector.Storable.Mutable as Vector
import           Data.Vector.Storable.Mutable (MVector)
import           Foreign.Storable             (Storable)
import           GHC.Exts                     (fromList)
import           GHC.Generics                 (Generic)

import           Luna.Core.Data               (List(..), Spec(..))


type StoreM' m   = StoreM (PrimState m)
data StoreM  s a = StoreM { _vector   :: {-# UNPACK #-} !(MVector s a)
                          , _freeIdxs ::                !List
                          } deriving (Generic)
makeLenses ''StoreM


coerceTo :: forall b a. Coercible a b => a -> b
coerceTo = coerce ; {-# INLINE coerceTo #-}

coerceVectorTo :: forall b a s. MVector s a -> MVector s b
coerceVectorTo = coerce ; {-# INLINE coerceVectorTo #-}


-- | O(1)
length :: Storable a => StoreM s a -> Int
length s = Vector.length $ s ^. vector ; {-# INLINE length #-}


-- === Construction === --

new   :: (PrimMonad m, Storable a) =>        m (StoreM' m a)
alloc :: (PrimMonad m, Storable a) => Int -> m (StoreM' m a)
new      = alloc 1024                                          ; {-# INLINE new   #-}
alloc !i = StoreM <$> Vector.unsafeNew i <*> pure (fromList [0 .. i - 1]) ; {-# INLINE alloc #-}


-- === Direct modifications === --

-- | O(1)
releaseKey :: Convertible' k Int => k -> StoreM s a -> StoreM s a
releaseKey !k !s = s & freeIdxs %~ (Cons $ convert' k) ; {-# INLINE releaseKey #-}

-- | O(1) / ?
reserveKey :: (PrimMonad m, Storable a, Convertible' Int k) => StoreM' m a -> m (StoreM' m a, k)
reserveKey s = go s (unsafeDoubleGrow s >>= flip go (error "impossible")) where
    go t e = case t ^. freeIdxs of
        Cons i is -> return (s & freeIdxs .~ is, convert' i)
        Null      -> e
    {-# INLINE go #-}
{-# INLINE reserveKey #-}

unsafeGrow :: (PrimMonad m, Storable a) => StoreM' m a -> Int -> m (StoreM' m a)
unsafeGrow !s !i = assert (i > 0) $ vector (flip Vector.unsafeGrow i) s ; {-# INLINE unsafeGrow #-} -- FIXME: alokacja pustych adresow!

unsafeDoubleGrow :: (PrimMonad m, Storable a) => StoreM' m a -> m (StoreM' m a)
unsafeDoubleGrow !s = unsafeGrow s (length s) ; {-# INLINE unsafeDoubleGrow #-}

unsafeWrite :: (PrimMonad m, Storable a, Convertible' k Int) => StoreM' m a -> k -> a -> m ()
unsafeWrite !s !k !a = Vector.unsafeWrite (s ^. vector) (convert' k) a ; {-# INLINE unsafeWrite #-}

unsafeWriteSpec :: forall t a k m. (PrimMonad m, Storable t, Convertible' k Int) => StoreM' m a -> k -> t -> m ()
unsafeWriteSpec !s !k !t = Vector.unsafeWrite (coerceVectorTo @(Spec t) $ s ^. vector) (convert' k) (Spec t) ; {-# INLINE unsafeWriteSpec #-}

