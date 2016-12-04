module Luna.IR.Internal.LayerStore where

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
import           Data.List           (sort)


-- FIXME[WD]: refactor vvv
-- === Utils === --

type STRefM m = STRef (PrimState m)

newSTRef :: PrimMonad m => a -> m (STRefM m a)
newSTRef = liftPrim . ST.newSTRef ; {-# INLINE newSTRef #-}

readSTRef :: PrimMonad m => STRefM m a -> m a
readSTRef = liftPrim . ST.readSTRef ; {-# INLINE readSTRef #-}

writeSTRef :: PrimMonad m => STRefM m a -> a -> m ()
writeSTRef = liftPrim .: ST.writeSTRef ; {-# INLINE writeSTRef #-}

modifySTRef'_ :: PrimMonad m => STRefM m r -> (r -> r) -> m ()
modifySTRef'_ = liftPrim .: ST.modifySTRef' ; {-# INLINE modifySTRef'_ #-}

modifySTRef' :: PrimMonad m => STRefM m r -> (r -> (a, r)) -> m a
modifySTRef' s = modifySTRefM' s . fmap return ; {-# INLINE modifySTRef' #-}

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



---------------------------
-- === LayerStoreRef === --
---------------------------

-- === Definition === --

type Idx = Int

type    LayerStoreRefM m     = LayerStoreRef (PrimState m)
newtype LayerStoreRef  s k a = LayerStoreRef (STRef s (LayerStore s k a))
type    LayerStoreM    m     = LayerStore    (PrimState m)
data    LayerStore     s k a = LayerStore { _size :: !Int
                                          , _free :: ![Idx]
                                          , _vec  :: !(Map k (VectorRef s a))
                                          } deriving (Show)

makeWrapped ''LayerStoreRef
makeLenses  ''LayerStore


-- === Ref modify === --

readLayerStoreRef :: PrimMonad m => LayerStoreRefM m k a -> m (LayerStoreM m k a)
readLayerStoreRef = readSTRef . unwrap' ; {-# INLINE readLayerStoreRef #-}

modifyLayerStoreRefM'_ :: PrimMonad m => (LayerStoreM m k a -> m (LayerStoreM m k a)) -> LayerStoreRefM m k a -> m ()
modifyLayerStoreRefM'_ f = flip modifySTRefM'_ f . unwrap' ; {-# INLINE modifyLayerStoreRefM'_ #-}

modifyLayerStoreRef'_ :: PrimMonad m => (LayerStoreM m k a -> LayerStoreM m k a) -> LayerStoreRefM m k a -> m ()
modifyLayerStoreRef'_ = modifyLayerStoreRefM'_ . fmap return ; {-# INLINE modifyLayerStoreRef'_ #-}


-- === Construction === --

empty :: PrimMonad m => m (LayerStoreRefM m k a)
empty = LayerStoreRef <$> newSTRef def ; {-# INLINE empty #-}

instance Default (LayerStore s k a) where
    def = LayerStore 0 def def ; {-# INLINE def #-}



autoGrow :: PrimMonad m => LayerStoreRefM m k a -> m Idx
autoGrow = flip modifySTRefM' autoGrow' . unwrap' ; {-# INLINE autoGrow #-}

-- | If realocation is not needed, performs in O(1).
reserveIdx :: PrimMonad m => LayerStoreRefM m k a -> m Idx
reserveIdx = flip modifySTRefM' reserveIdx' . unwrap' ; {-# INLINE reserveIdx #-}

-- | Performs in O(1)
freeIdx :: PrimMonad m => LayerStoreRefM m k a -> Idx -> m ()
freeIdx s i = modifySTRef'_ (unwrap' s) (free %~ (i:)) ; {-# INLINE freeIdx #-}

-- | Doesn't initialize created vector.
unsafeAddKey :: (PrimMonad m, Ord k) => k -> LayerStoreRefM m k a -> m ()
unsafeAddKey = modifyLayerStoreRefM'_ . unsafeAddKey' ; {-# INLINE unsafeAddKey #-}

keys :: PrimMonad m => LayerStoreRefM m k a -> m [k]
keys s = Map.keys . view vec <$> readSTRef (unwrap' s) ; {-# INLINE keys #-}

ixes :: PrimMonad m => LayerStoreRefM m k a -> m [Int]
ixes = ixes' <∘> readLayerStoreRef ; {-# INLINE ixes #-}

assocs :: PrimMonad m => LayerStoreRefM m k a -> m [(k, VectorRefM m a)]
assocs = Map.assocs . view vec <∘> readLayerStoreRef ; {-# INLINE assocs #-}

mapWithKey :: PrimMonad m => (k -> VectorRefM m a -> VectorRefM m a) -> LayerStoreRefM m k a -> m ()
mapWithKey = modifyLayerStoreRef'_ . mapWithKey' ; {-# INLINE mapWithKey #-}

traverseWithKey :: PrimMonad m => (k -> VectorRefM m a -> m (VectorRefM m a)) -> LayerStoreRefM m k a -> m ()
traverseWithKey = modifyLayerStoreRefM'_ . traverseWithKey' ; {-# INLINE traverseWithKey #-}

readKey :: (PrimMonad m, Ord k) => k -> LayerStoreRefM m k a -> m (Maybe (VectorRefM m a))
readKey k = (^? (vec . ix k)) <∘> readLayerStoreRef ; {-# INLINE readKey #-}


-- === Non-ref API === --

autoGrow' :: PrimMonad m => LayerStore (PrimState m) k a -> m (Idx, LayerStore (PrimState m) k a)
autoGrow' m = (size', nm) <$ mapM_ (unsafeGrow grow) (m ^. vec) where
    size' = m ^. size
    grow  = if size' == 0 then 1 else size'
    nm    = m & size %~ (+ grow)
              & free .~ [size' + 1 .. size' + grow - 1]
{-# INLINE autoGrow' #-}

reserveIdx' :: PrimMonad m => LayerStoreM m k a -> m (Idx, LayerStoreM m k a)
reserveIdx' m = case m ^. free of
    (i : is) -> return (i, m & free .~ is)
    []       -> autoGrow' m
{-# INLINE reserveIdx' #-}

unsafeAddKey' :: (PrimMonad m, Ord k) => k -> LayerStoreM m k a -> m (LayerStoreM m k a)
unsafeAddKey' k m = do
    v <- newVectorRef
    return $ m & vec . at k ?~ v
{-# INLINE unsafeAddKey' #-}


-- | Used index access performs in approx. O(n (log n))
--   in order to make allocating and freeing indexes as fast as possible.
ixes' :: LayerStore s k a -> [Int]
ixes' m = findIxes [0 .. m ^. size - 1] (sort $ m ^. free) where
    findIxes (i : is) (f : fs) = if i == f then findIxes is fs else i : findIxes is (f : fs)
    findIxes is       []       = is
    findIxes []       _        = []
    {-# INLINE findIxes #-}
{-# INLINE ixes' #-}


mapWithKey' :: (k -> VectorRef s a -> VectorRef s b) -> LayerStore s k a -> LayerStore s k b
mapWithKey' f = vec %~ Map.mapWithKey f ; {-# INLINE mapWithKey' #-}

traverseWithKey' :: Applicative m => (k -> VectorRef s a -> m (VectorRef s b)) -> LayerStore s k a -> m (LayerStore s k b)
traverseWithKey' = vec . Map.traverseWithKey ; {-# INLINE traverseWithKey' #-}


-- === Instances === --

-- Ixed
type instance     IxValue (LayerStore s k a) = VectorRef s a
type instance     Index   (LayerStore s k a) = k
instance Ord k => Ixed    (LayerStore s k a) where
    ix = vec .: ix ; {-# INLINE ix #-}
