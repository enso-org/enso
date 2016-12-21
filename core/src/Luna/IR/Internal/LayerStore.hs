module Luna.IR.Internal.LayerStore where

import Prelude hiding (tail)
import Data.Default
import Data.Monoid
import Data.Maybe     (fromJust)
import Control.Lens.Utils
import Control.Monad
import Data.Functor.Utils hiding ((.))
import Control.Monad.Primitive
import qualified Data.STRef as ST
import           Data.STRef (STRef)

import qualified Data.Vector.Mutable as MV
import           Data.Vector.Mutable (MVector)
import qualified Data.Vector         as V
import           Data.Vector         (Vector)
import           Data.Map            (Map)
import qualified Data.Map            as Map
import qualified Data.List           as List


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

type MVectorM m = MVector (PrimState m)

newtype VectorRef  s a = VectorRef (STRef s (MVector s a))
type    VectorRefM m   = VectorRef (PrimState m)

makeLenses  ''VectorRef
makeWrapped ''VectorRef


-- === Utils === --

newVectorRef :: Int -> PrimMonad m => m (VectorRefM m a)
newVectorRef = fmap wrap' . newSTRef <=< MV.new ; {-# INLINE newVectorRef #-}

modifyVectorRefM :: PrimMonad m => (MVectorM m a -> m (t, MVectorM m a)) -> VectorRefM m a -> m t
modifyVectorRefM f (unwrap' -> ref) = modifySTRefM' ref f ; {-# INLINE modifyVectorRefM #-}

modifyVectorRefM_ :: PrimMonad m => (MVectorM m a -> m (MVectorM m a)) -> VectorRefM m a -> m ()
modifyVectorRefM_ = dropResult modifyVectorRefM ; {-# INLINE modifyVectorRefM_ #-}

withVectorRefM :: PrimMonad m => (MVectorM m a -> m t) -> VectorRefM m a -> m t
withVectorRefM f (unwrap' -> ref) = f =<< readSTRef ref ; {-# INLINE withVectorRefM #-}

unsafeGrow :: PrimMonad m => Int -> VectorRefM m a -> m ()
unsafeGrow i = modifyVectorRefM_ (flip MV.unsafeGrow i)

unsafeWrite :: PrimMonad m => VectorRefM m a -> Idx -> a -> m ()
unsafeWrite v i a = withVectorRefM (\mv -> MV.unsafeWrite mv i a) v ; {-# INLINE unsafeWrite #-}

unsafeRead :: PrimMonad m => Idx -> VectorRefM m a -> m a
unsafeRead i (unwrap' -> ref) = readSTRef ref >>= flip MV.unsafeRead i ; {-# INLINE unsafeRead #-}


unsafeFreezeVR :: PrimMonad m => VectorRefM m a -> m (Vector a)
unsafeFreezeVR = V.unsafeFreeze <=< readSTRef . unwrap' ; {-# INLINE unsafeFreezeVR #-}

freezeVR :: PrimMonad m => VectorRefM m a -> m (Vector a)
freezeVR = V.freeze <=< readSTRef . unwrap' ; {-# INLINE freezeVR #-}

unsafeThawVR :: PrimMonad m => Vector a -> m (VectorRefM m a)
unsafeThawVR = fmap VectorRef . newSTRef <=< V.unsafeThaw ; {-# INLINE unsafeThawVR #-}

thawVR :: PrimMonad m => Vector a -> m (VectorRefM m a)
thawVR = fmap VectorRef . newSTRef <=< V.thaw ; {-# INLINE thawVR #-}


-- === Instances === --

instance Show (VectorRef s a) where show _ = "VectorRef"



---------------------------
-- === LayerStoreRef === --
---------------------------

-- === Definition === --

defSize :: Int
defSize = 1024

type Idx = Int

type    LayerStoreRefM m     = LayerStoreRef  (PrimState m)
newtype LayerStoreRef  s k a = LayerStoreRef  (STRef s (LayerStoreST s k a))
type    LayerStoreM    m k a = LayerStoreST   (PrimState m) k a
type    LayerStoreST   s k a = LayerStoreBase k (VectorRef s a)
type    LayerStore       k a = LayerStoreBase k (Vector a)
data    LayerStoreBase   k a = LayerStore { _size :: !Int
                                          , _free :: ![Idx]
                                          , _vec  :: !(Map k a)
                                          } deriving (Show, Functor, Foldable, Traversable)

makeWrapped ''LayerStoreRef
makeLenses  ''LayerStoreBase


-- === Ref modify === --

readLayerStoreRef :: PrimMonad m => LayerStoreRefM m k a -> m (LayerStoreM m k a)
readLayerStoreRef = readSTRef . unwrap' ; {-# INLINE readLayerStoreRef #-}

modifyLayerStoreRefM'_ :: PrimMonad m => (LayerStoreM m k a -> m (LayerStoreM m k a)) -> LayerStoreRefM m k a -> m ()
modifyLayerStoreRefM'_ f = flip modifySTRefM'_ f . unwrap' ; {-# INLINE modifyLayerStoreRefM'_ #-}

modifyLayerStoreRef'_ :: PrimMonad m => (LayerStoreM m k a -> LayerStoreM m k a) -> LayerStoreRefM m k a -> m ()
modifyLayerStoreRef'_ = modifyLayerStoreRefM'_ . fmap return ; {-# INLINE modifyLayerStoreRef'_ #-}


-- === Construction === --

-- append' :: LayerStoreM m k a -> LayerStoreM m k a -> LayerStoreM m k a
-- append' a s = undefined where
--     growSize =


instance Default (LayerStoreST s k a) where
    def = LayerStore defSize [0 .. defSize - 1] def ; {-# INLINE def #-}


empty :: PrimMonad m => m (LayerStoreRefM m k a)
empty = LayerStoreRef <$> newSTRef def ; {-# INLINE empty #-}

autoGrowFree :: PrimMonad m => LayerStoreRefM m k a -> m Idx
autoGrowFree = flip modifySTRefM' autoGrowFree' . unwrap' ; {-# INLINE autoGrowFree #-}

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


-- === Mutability === --

unsafeFreeze :: PrimMonad m => LayerStoreRefM m k a -> m (LayerStore k a)
unsafeFreeze = readLayerStoreRef >=> unsafeFreeze' ; {-# INLINE unsafeFreeze #-}

freeze :: PrimMonad m => LayerStoreRefM m k a -> m (LayerStore k a)
freeze = readLayerStoreRef >=> freeze' ; {-# INLINE freeze #-}

unsafeThaw :: PrimMonad m => LayerStore k a -> m (LayerStoreRefM m k a)
unsafeThaw = fmap wrap' . newSTRef <=< unsafeThaw' ; {-# INLINE unsafeThaw #-}

thaw :: PrimMonad m => LayerStore k a -> m (LayerStoreRefM m k a)
thaw = fmap wrap' . newSTRef <=< thaw' ; {-# INLINE thaw #-}

duplicate :: PrimMonad m => LayerStoreRefM m k a -> m (LayerStoreRefM m k a)
duplicate = freeze >=> unsafeThaw ; {-# INLINE duplicate #-}



unsafeFreeze' :: PrimMonad m => LayerStoreM m k a -> m (LayerStore k a)
unsafeFreeze' = mapM unsafeFreezeVR ; {-# INLINE unsafeFreeze' #-}

freeze' :: PrimMonad m => LayerStoreM m k a -> m (LayerStore k a)
freeze' = mapM freezeVR ; {-# INLINE freeze' #-}

unsafeThaw' :: PrimMonad m => LayerStore k a -> m (LayerStoreM m k a)
unsafeThaw' = mapM unsafeThawVR ; {-# INLINE unsafeThaw' #-}

thaw' :: PrimMonad m => LayerStore k a -> m (LayerStoreM m k a)
thaw' = mapM thawVR ; {-# INLINE thaw' #-}

duplicate' :: PrimMonad m => LayerStoreM m k a -> m (LayerStoreM m k a)
duplicate' = freeze' >=> unsafeThaw' ; {-# INLINE duplicate' #-}



-- === Non-ref API === --

autoGrowFree' :: PrimMonad m => LayerStoreST (PrimState m) k a -> m (Idx, LayerStoreST (PrimState m) k a)
autoGrowFree' m = (grow, nm) <$ mapM_ (unsafeGrow grow) (m ^. vec) where
    grow = m ^. size
    nm   = m & size %~ (+ grow)
             & free .~ [grow + 1 .. grow + grow - 1]
{-# INLINE autoGrowFree' #-}

reserveIdx' :: PrimMonad m => LayerStoreM m k a -> m (Idx, LayerStoreM m k a)
reserveIdx' m = case m ^. free of
    (i : is) -> return (i, m & free .~ is)
    []       -> autoGrowFree' m
{-# INLINE reserveIdx' #-}

unsafeAddKey' :: (PrimMonad m, Ord k) => k -> LayerStoreM m k a -> m (LayerStoreM m k a)
unsafeAddKey' k m = do
    v <- newVectorRef (m ^. size)
    return $ m & vec . at k ?~ v
{-# INLINE unsafeAddKey' #-}


-- | Used index access performs in approx. O(n (log n))
--   in order to make allocating and freeing indexes as fast as possible.
ixes' :: LayerStoreBase k a -> [Int]
ixes' m = findIxes [0 .. m ^. size - 1] (List.sort $ m ^. free) where
    findIxes (i : is) (f : fs) = if i == f then findIxes is fs else i : findIxes is (f : fs)
    findIxes is       []       = is
    findIxes []       _        = []
    {-# INLINE findIxes #-}
{-# INLINE ixes' #-}


mapWithKey' :: (k -> VectorRef s a -> VectorRef s b) -> LayerStoreST s k a -> LayerStoreST s k b
mapWithKey' f = vec %~ Map.mapWithKey f ; {-# INLINE mapWithKey' #-}

traverseWithKey' :: Applicative m => (k -> VectorRef s a -> m (VectorRef s b)) -> LayerStoreST s k a -> m (LayerStoreST s k b)
traverseWithKey' = vec . Map.traverseWithKey ; {-# INLINE traverseWithKey' #-}

-- === Structural operations === --

-- This operation may leave some keys uninitialized, if they are not present in the imported structure
unsafeMerge :: (Ord k, PrimMonad m) => LayerStore k a -> LayerStoreRefM m k a -> m [(Idx, Idx)]
unsafeMerge st@(LayerStore _ _ m) store = do
    localKeys <- keys store
    let foreignKeys  = Map.keys m
    let importedKeys = List.intersect localKeys foreignKeys
    let ixesToImport = ixes' st
    newIxes <- mapM reserveIdx $ store <$ ixesToImport
    let ixMapping = zip ixesToImport newIxes
    forM_ importedKeys $ \k -> do
        targetVec <- fromJust <$> readKey k store -- this fromJust is guaranteed not to fail by construction of importedKeys
        forM_ ixMapping $ \(oldIx, newIx) ->
            unsafeWrite targetVec newIx $ V.unsafeIndex (fromJust $ Map.lookup k m) oldIx -- again, fromJust safety guaraneed by construction of importedKeys
    return ixMapping



-- === Instances === --

-- Ixed
type instance     IxValue (LayerStoreST s k a) = VectorRef s a
type instance     Index   (LayerStoreST s k a) = k
instance Ord k => Ixed    (LayerStoreST s k a) where
    ix = vec .: ix ; {-# INLINE ix #-}
