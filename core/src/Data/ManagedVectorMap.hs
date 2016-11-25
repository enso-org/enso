module Data.ManagedVectorMap where

import Prelude
import Data.Default
import Control.Lens.Utils
import Data.Functor.Utils hiding ((.))
import Control.Monad.Primitive
import qualified Data.STRef as ST
import           Data.STRef (STRef)

import qualified Data.Vector.Mutable as MV
import qualified Data.Vector         as V
import           Data.Vector         (unsafeThaw)
import           Data.Map            (Map)
import qualified Data.Map            as Map


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




-----------------------
-- === VectorRef === --
-----------------------

type Vector    = MV.MVector
type VectorM m = Vector (PrimState m)

newtype VectorRef  s a = VectorRef (STRef s (Vector s a))
type    VectorRefM m   = VectorRef (PrimState m)

makeLenses  ''VectorRef
makeWrapped ''VectorRef


-- === Utils === --

newVectorRef :: PrimMonad m => m (VectorRefM m a)
newVectorRef = fmap wrap' . newSTRef =<< unsafeThaw mempty ; {-# INLINE newVectorRef #-}

modifyVectorRefM :: PrimMonad m => (VectorM m a -> m (t, VectorM m a)) -> VectorRefM m a -> m t
modifyVectorRefM f (unwrap' -> ref) = modifySTRefM' ref f ; {-# INLINE modifyVectorRefM #-}

modifyVectorRefM_ :: PrimMonad m => (VectorM m a -> m (VectorM m a)) -> VectorRefM m a -> m ()
modifyVectorRefM_ = dropResult modifyVectorRefM ; {-# INLINE modifyVectorRefM_ #-}

withVectorRefM :: PrimMonad m => (VectorM m a -> m t) -> VectorRefM m a -> m t
withVectorRefM f (unwrap' -> ref) = f =<< readSTRef ref ; {-# INLINE withVectorRefM #-}

unsafeGrow :: PrimMonad m => Int -> VectorRefM m a -> m ()
unsafeGrow i = modifyVectorRefM_ (flip MV.unsafeGrow i)

unsafeWrite :: PrimMonad m => VectorRefM m a -> Idx -> a -> m ()
unsafeWrite v i a = withVectorRefM (\mv -> MV.unsafeWrite mv i a) v ; {-# INLINE unsafeWrite #-}

unsafeRead :: PrimMonad m => Idx -> VectorRefM m a -> m a
unsafeRead i (unwrap' -> ref) = readSTRef ref >>= flip MV.unsafeRead i ; {-# INLINE unsafeRead #-}


-- === Instances === --

instance Show (VectorRef s a) where show _ = "VectorRef"



------------------------------
-- === ManagedVectorMap === --
------------------------------

-- === Definition === --

type Idx       = Int

type ManagedVectorMapM m     = ManagedVectorMap (PrimState m)
data ManagedVectorMap  s k a = ManagedVectorMap { _size :: !Int
                                                , _free :: ![Idx]
                                                , _vec  :: !(Map k (VectorRef s a))
                                                } deriving (Show)

makeLenses  ''ManagedVectorMap


-- === Construction === --

instance Default (ManagedVectorMap s k a) where
    def = ManagedVectorMap 0 def def ; {-# INLINE def #-}

autoGrow :: PrimMonad m => ManagedVectorMapM m k a -> m (Idx, ManagedVectorMapM m k a)
autoGrow m = (size', nm) <$ mapM_ (unsafeGrow grow) (m ^. vec) where
    size' = m ^. size
    grow  = if size' == 0 then 1 else size'
    nm    = m & size %~ (+ grow)
              & free .~ [size' + 1 .. size' + grow - 1]
{-# INLINE autoGrow #-}

reserveIdx :: PrimMonad m => ManagedVectorMapM m k a -> m (Idx, ManagedVectorMapM m k a)
reserveIdx m = case m ^. free of
    (i : is) -> return (i, m & free .~ is)
    []       -> autoGrow m
{-# INLINE reserveIdx #-}

-- | unsafeAddKey doesn't create missing fields
unsafeAddKey :: (PrimMonad m, Ord k) => k -> ManagedVectorMapM m k a -> m (ManagedVectorMapM m k a)
unsafeAddKey k m = do
    v <- newVectorRef
    return $ m & vec . at k ?~ v

keys :: ManagedVectorMap s k a -> [k]
keys = Map.keys . view vec ; {-# INLINE keys #-}

assocs :: ManagedVectorMap s k a -> [(k, VectorRef s a)]
assocs = Map.assocs . view vec ; {-# INLINE assocs #-}

mapWithKey :: (k -> VectorRef s a -> VectorRef s b) -> ManagedVectorMap s k a -> ManagedVectorMap s k b
mapWithKey f = vec %~ Map.mapWithKey f ; {-# INLINE mapWithKey #-}

traverseWithKey :: Applicative m => (k -> VectorRef s a -> m (VectorRef s b)) -> ManagedVectorMap s k a -> m (ManagedVectorMap s k b)
traverseWithKey = vec . Map.traverseWithKey ; {-# INLINE traverseWithKey #-}

--
-- add :: PrimMonad m => ManagedVectorMapM m a -> a -> m Idx
-- add mv a = flip withMeasuredVectorMapM mv $ \v -> do
--     (idx, nv) <- getNewIdx v
--     unsafeWrite nv idx a
--     return (idx, nv)
-- {-# INLINE add #-}
--



-- === Instances === --

-- Ixed
type instance     IxValue (ManagedVectorMap s k a) = VectorRef s a
type instance     Index   (ManagedVectorMap s k a) = k
instance Ord k => Ixed    (ManagedVectorMap s k a) where
    ix = vec .: ix ; {-# INLINE ix #-}
