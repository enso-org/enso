{-# LANGUAGE NoMonomorphismRestriction #-}

{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances #-}
#endif

module Data.Container.Instances.Vector.Boxed.Mutable where

import Prelude

import Control.Monad.ST
import Data.Container.Class
import Data.Container.List
import Data.Default
import Data.Monoid
import Data.Typeable
import Data.Foldable
import           Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as MV


import qualified Data.Container.Opts as M
import           Data.Container.Opts (Opt(P,N), Knowledge(..), Query(..), OptQuery(..), ModsOf, ParamsOf, CondOpt, ifOpt)
import Data.Layer
import Data.Container.Type (In)

import Data.Container.Proxy
import Control.Monad.Primitive (PrimMonad, PrimState)

------------------------------
-- === Global instances === --
------------------------------

type instance Index     (MVector s a) = Int
type instance Item      (MVector s a) = a
type instance Container (MVector s a) = MVector s a
type instance DataStore (MVector s a) = MVector s a
instance      Monad m => IsContainerM  m (MVector s a) where fromContainerM = return         ; {-# INLINE fromContainerM #-}
instance      Monad m => HasContainerM m (MVector s a) where viewContainerM = return         ; {-# INLINE viewContainerM #-}
                                                             setContainerM  = const . return ; {-# INLINE setContainerM  #-}


--instance ToList   (MVector s a) where toList   = V.toList   ; {-# INLINE toList   #-}
--instance FromList (MVector s a) where fromList = V.fromList ; {-# INLINE fromList #-}

----------------------------------
-- === Operations instances === --
----------------------------------

-- === Finite ===

-- [+] Measurable
-- [+] MinBounded
-- [+] MaxBounded

type instance ParamsOf MeasurableOp (MVector s a) = '[]
type instance ModsOf   MeasurableOp (MVector s a) = '[]

type instance ParamsOf MinBoundedOp (MVector s a) = '[]
type instance ModsOf   MinBoundedOp (MVector s a) = '[]

type instance ParamsOf MaxBoundedOp (MVector s a) = '[]
type instance ModsOf   MaxBoundedOp (MVector s a) = '[]

instance Monad m              => MeasurableQM_ '[] ps m     (MVector s a) where sizeM_     _   = return . Res () . MV.length       ; {-# INLINE sizeM_     #-}
instance (Monad m, idx ~ Int) => MinBoundedQM_ '[] ps m idx (MVector s a) where minBoundM_ _ _ = return $ Res () 0                 ; {-# INLINE minBoundM_ #-}
instance (Monad m, idx ~ Int) => MaxBoundedQM_ '[] ps m idx (MVector s a) where maxBoundM_ _ v = return $ Res () $ MV.length v - 1 ; {-# INLINE maxBoundM_ #-}


-- === Construction ===

-- [ ] Singleton
-- [+] Allocable
-- [ ] Expandable
-- [+] Growable

--type instance ParamsOf SingletonOp  (MVector s a) = '[]
--type instance ModsOf   SingletonOp  (MVector s a) = '[M.Ixed]

type instance ParamsOf AllocableOp  (MVector s a) = '[]
type instance ModsOf   AllocableOp  (MVector s a) = '[M.Ixed]

--type instance ParamsOf ExpandableOp (MVector s a) = '[]
--type instance ModsOf   ExpandableOp (MVector s a) = '[M.Ixed]

type instance ParamsOf GrowableOp   (MVector s a) = '[]
type instance ModsOf   GrowableOp   (MVector s a) = '[M.Ixed]

--instance (Monad m, a ~ a') => SingletonQM_ '[N       ] ps m a' (MVector s a) where singletonM_ _ = return . Res ()     . V.singleton ; {-# INLINE singletonM_ #-}
--instance (Monad m, a ~ a') => SingletonQM_ '[P M.Ixed] ps m a' (MVector s a) where singletonM_ _ = return . Res (0,()) . V.singleton ; {-# INLINE singletonM_ #-}

instance (PrimMonad m, s ~ PrimState m) => AllocableQM_ '[N       ] ps m    (MVector s a) where allocM_ _ i = Res ()            <$> MV.unsafeNew i ; {-# INLINE allocM_ #-}
instance (PrimMonad m, s ~ PrimState m) => AllocableQM_ '[P M.Ixed] ps m    (MVector s a) where allocM_ _ i = Res ([0..i-1],()) <$> MV.unsafeNew i ; {-# INLINE allocM_ #-}

--instance Monad m => ExpandableQM_ '[N       ] ps m   (MVector s a) where expandM_ _ v = return $ Res ()                $ runST $ V.unsafeThaw v >>= flip MV.unsafeGrow 1 >>= V.unsafeFreeze {-# INLINE expandM_ #-}
--instance Monad m => ExpandableQM_ '[P M.Ixed] ps m   (MVector s a) where expandM_ _ v = return $ Res ([V.length v],()) $ runST $ V.unsafeThaw v >>= flip MV.unsafeGrow 1 >>= V.unsafeFreeze {-# INLINE expandM_ #-}


--instance (PrimMonad m, s ~ PrimState m) => GrowableQM_ '[P M.Ixed] ps m   (MVector s Int) where growM_ _ i v = Res (take i [size v ..],()) <$> (MV.unsafeGrow v i >>= flip (foldlM (flip ($))) ((\pi pv -> MV.write pv pi 7 *> pure pv) <$> take i [size v ..]))                      ; {-# INLINE growM_ #-}
instance (PrimMonad m, s ~ PrimState m) => GrowableQM_ '[N       ] ps m   (MVector s a) where growM_ _ i v = Res () <$> MV.unsafeGrow v i                      ; {-# INLINE growM_ #-}
instance (PrimMonad m, s ~ PrimState m) => GrowableQM_ '[P M.Ixed] ps m   (MVector s a) where growM_ _ i v = Res (take i [size v ..],()) <$> MV.unsafeGrow v i ; {-# INLINE growM_ #-}


-- === Modification ===
-- [+] Appendable
-- [+] Prependable
-- [+] Addable
-- [+] Removable
-- [+] Insertable
-- [+] Freeable


--type instance ParamsOf AppendableOp  (MVector s a) = '[]
--type instance ModsOf   AppendableOp  (MVector s a) = '[M.Ixed]

--type instance ParamsOf PrependableOp (MVector s a) = '[]
--type instance ModsOf   PrependableOp (MVector s a) = '[M.Ixed]

type instance ParamsOf AddableOp     (MVector s a) = '[]
type instance ModsOf   AddableOp     (MVector s a) = '[M.Ixed]

--type instance ParamsOf RemovableOp   (MVector s a) = '[M.Try]
--type instance ModsOf   RemovableOp   (MVector s a) = '[M.Ixed]

type instance ParamsOf InsertableOp  (MVector s a) = '[]
type instance ModsOf   InsertableOp  (MVector s a) = '[M.Ixed]

type instance ParamsOf FreeableOp    (MVector s a) = '[]
type instance ModsOf   FreeableOp    (MVector s a) = '[]

--instance (Monad m, a ~ a') => AppendableQM_  '[N       ] ps m a' (MVector s a) where
--    appendM_ _ el v = (return . Res ())          $ V.snoc v el ; {-# INLINE appendM_ #-}
--instance (Monad m, a ~ a') => AppendableQM_  '[P M.Ixed] ps m a' (MVector s a) where
--    appendM_ _ el v = (return . Res (size v,())) $ V.snoc v el ; {-# INLINE appendM_ #-}


--instance (Monad m, a ~ a') => PrependableQM_ '[N       ] ps m a' (MVector s a) where
--    prependM_ _ el v = (return . Res ())     $ V.cons el v ; {-# INLINE prependM_ #-}
--instance (Monad m, a ~ a') => PrependableQM_ '[P M.Ixed] ps m a' (MVector s a) where
--    prependM_ _ el v = (return . Res (0,())) $ V.cons el v ; {-# INLINE prependM_ #-}



--instance (Monad m, a ~ a') => AddableQM_ '[N       ] ps m a' (MVector s a) where addM_ _ el v = (return . Res ())          $ V.snoc v el ; {-# INLINE addM_ #-}
--instance (Monad m, a ~ a') => AddableQM_ '[P M.Ixed] ps m a' (MVector s a) where addM_ _ el v = (return . Res (size v,())) $ V.snocx v el ; {-# INLINE addM_ #-}


--instance (Monad m, Eq a, a ~ a') => RemovableQM_ '[N       ] '[P M.Try] m a' (MVector s a) where
--    removeM_ _ el v = case idx of
--        Just  i -> (return . Res ()) $ V.slice 0 (i-1) v <> V.slice i (size v - i) v
--        Nothing -> fail "Element not found"
--        where idx = V.findIndex (== el) v
--    {-# INLINE removeM_ #-}

--instance (Monad m, Eq a, a ~ a') => RemovableQM_ '[P M.Ixed] '[P M.Try] m a' (MVector s a) where
--    removeM_ _ el v = case idx of
--        Just  i -> (return . Res (i,())) $ V.slice 0 (i-1) v <> V.slice i (size v - i) v
--        Nothing -> fail "Element not found"
--        where idx = V.findIndex (== el) v
--    {-# INLINE removeM_ #-}


instance (PrimMonad m, s ~ PrimState m, a ~ a', idx ~ Int) => InsertableQM_ '[N       ] ps m idx a' (MVector s a) where insertM_ _ idx el v = Res ()       <$> (MV.write v idx el *> pure v) ; {-# INLINE insertM_ #-}
instance (PrimMonad m, s ~ PrimState m, a ~ a', idx ~ Int) => InsertableQM_ '[P M.Ixed] ps m idx a' (MVector s a) where insertM_ _ idx el v = Res (idx,()) <$> (MV.write v idx el *> pure v) ; {-# INLINE insertM_ #-}


instance (Monad m, idx ~ Int) => FreeableQM_ '[] ps m idx (MVector s a) where
    freeM_ _ idx v = undefined -- (return . Res ()) $ (V.//) v [(idx,error $ "uninitialized element at index " <> show idx)]
    {-# INLINE freeM_ #-}



-- === Indexing ===

-- [+] Indexable
-- [-] TracksFreeIxes
-- [-] TracksUsedIxes
-- [+] TracksIxes
-- [ ] TracksElems

type instance ParamsOf IndexableOp       (MVector s a) = '[M.Unchecked, M.Try]
type instance ModsOf   IndexableOp       (MVector s a) = '[M.Ixed]

type instance ParamsOf TracksIxesOp      (MVector s a) = '[]
type instance ModsOf   TracksIxesOp      (MVector s a) = '[]


instance (PrimMonad m, s ~ PrimState m, a ~ a', idx ~ Int) => IndexableQM_  '[N       ] '[unchecked, try] m idx a' (MVector s a) where indexM_  _ idx v = Res ()       <$> MV.unsafeRead v idx ; {-# INLINE indexM_ #-}
--instance (Monad m, a ~ a', idx ~ Int, CondOpt unchecked, CondOpt try) => IndexableQM_  '[P M.Ixed] '[unchecked, try] m idx a' (MVector s a) where
--    indexM_  _ idx v = Res (idx,()) <$> checkedBoundsIfM2 (Proxy :: Proxy unchecked) (Proxy :: Proxy try) idx v (V.unsafeIndex v idx)
--    {-# INLINE indexM_ #-}


instance (Monad m, idx ~ Int) => TracksIxesQM_ '[] ps m idx (MVector s a) where ixesM_ _ v = (return . Res ()) $ [0 .. size v - 1] ; {-# INLINE ixesM_ #-}


--instance (Monad m, a ~ a') => TracksElemsQM_ '[] ps m a' (MVector s a) where
--    elemsM_ _ v = (return . Res ()) $ V.toList v ; {-# INLINE elemsM_ #-}


------ missing instances ----

--instance Default (MVector s a) where def = mempty ; {-# INLINE def #-}


----- Utils (TODO: refactor)

--failT2 p = ifOpt p fail error

--checkBounds2 i v l r = if i > max || i < 0 then l ("index " <> show i <> " out of bounds x[0," <> show max <> "]") else r where max = size v - 1
--checkBoundsM2 p idx v = checkBounds2 idx v (const . failT2 p) return
--checkedBoundsIfM2 unchecked try idx v = checkedIfM2 unchecked (checkBoundsM2 try idx v)

--checkedIfM2 p = ifOpt p return
