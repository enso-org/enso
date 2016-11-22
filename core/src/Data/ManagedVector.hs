module Data.ManagedVector where

import Prelude
import Control.Lens.Utils
import Data.Functor.Utils hiding ((.))
import Control.Monad.Primitive
import qualified Data.STRef as ST
import           Data.STRef (STRef)

import qualified Data.Vector.Mutable as MV
import qualified Data.Vector         as V
import           Data.Vector         (unsafeThaw)



-- === Utils === --

type STRefM m = STRef (PrimState m)

newSTRef :: PrimMonad m => a -> m (STRefM m a)
newSTRef = liftPrim . ST.newSTRef ; {-# INLINE newSTRef #-}

readSTRef :: PrimMonad m => STRefM m a -> m a
readSTRef = liftPrim . ST.readSTRef ; {-# INLINE readSTRef #-}

writeSTRef :: PrimMonad m => STRefM m a -> a -> m ()
writeSTRef = liftPrim .: ST.writeSTRef ; {-# INLINE writeSTRef #-}

modifySTRef' :: PrimMonad m => STRefM m a -> (a -> a) -> m ()
modifySTRef' = liftPrim .: ST.modifySTRef' ; {-# INLINE modifySTRef' #-}

modifySTRefM' :: PrimMonad m => STRefM m r -> (r -> m (a, r)) -> m a
modifySTRefM' ref f = do
    v  <- readSTRef ref
    (a, v') <- f v
    v' `seq` writeSTRef ref v'
    return a
{-# INLINE modifySTRefM' #-}

modifySTRefM'_ :: PrimMonad m => STRefM m r -> (r -> m r) -> m ()
modifySTRefM'_ = dropResult . modifySTRefM' ; {-# INLINE modifySTRefM'_ #-}


---

dropResult :: (Functor m, Functor f) => (f (m ((), t)) -> a) -> (f (m t) -> a)
dropResult = (. (((),) .:)) ; {-# INLINE dropResult #-}


---------------------------
-- === ManagedVector === --
---------------------------

-- === Definition === --

type Idx = Int

type MeasuredVectorM m   = MeasuredVector (PrimState m)
data MeasuredVector  s a = MeasuredVector { _free :: ![Idx]
                                          , _vec  :: !(MV.MVector s a)
                                          }

type    ManagedVectorM m   = ManagedVector (PrimState m)
newtype ManagedVector  s a = ManagedVector (STRef s (MeasuredVector s a))

makeLenses  ''MeasuredVector
makeWrapped ''ManagedVector


-- === Construction === --



empty :: PrimMonad m => m (ManagedVectorM m a)
empty = fmap wrap' . newSTRef =<< (MeasuredVector mempty <$> unsafeThaw mempty) ; {-# INLINE empty #-}

withMeasuredVectorM :: PrimMonad m => (MeasuredVectorM m a -> m (t, MeasuredVectorM m a)) -> ManagedVectorM m a -> m t
withMeasuredVectorM f (ManagedVector ref) = modifySTRefM' ref f ; {-# INLINE withMeasuredVectorM #-}

withMeasuredVectorM_ :: PrimMonad m => (MeasuredVectorM m a -> m (MeasuredVectorM m a)) -> ManagedVectorM m a -> m ()
withMeasuredVectorM_ = dropResult withMeasuredVectorM ; {-# INLINE withMeasuredVectorM_ #-}

autoGrow :: PrimMonad m => MeasuredVectorM m a -> m (Idx, MeasuredVectorM m a)
autoGrow mvec = do
    let v    = mvec ^.vec
        size = MV.length v
        grow = if size == 0 then 1 else size
    nv <- MV.unsafeGrow v grow
    let nmvec = mvec & vec  .~ nv
                     & free .~ [size + 1 .. size + grow - 1]
    return (size, nmvec)
{-# INLINE autoGrow #-}

getNewIdx :: PrimMonad m => MeasuredVectorM m a -> m (Idx, MeasuredVectorM m a)
getNewIdx mvec = case mvec ^. free of
    (i : is) -> return (i, mvec & free .~ is)
    []       -> autoGrow mvec
{-# INLINE getNewIdx #-}

add :: PrimMonad m => ManagedVectorM m a -> a -> m Idx
add mv a = flip withMeasuredVectorM mv $ \v -> do
    (idx, nv) <- getNewIdx v
    unsafeWrite nv idx a
    return (idx, nv)
{-# INLINE add #-}

unsafeWrite :: PrimMonad m => MeasuredVectorM m a -> Idx -> a -> m ()
unsafeWrite = MV.unsafeWrite . view vec ; {-# INLINE unsafeWrite #-}
