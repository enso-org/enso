{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}


{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances #-}
#endif

module Data.Container.Instances.Vector.Lazy where

import Prelude

import Control.Monad.ST
import Data.Container.Class
import Data.Container.List
import Data.Default
import Data.Monoid
import Data.Typeable

import Data.IntMap (IntMap)
import Data.Map    (Map)
import Data.Maybe  (fromJust)

import qualified Data.IntMap                 as IntMap
import qualified Data.Map                    as Map
import qualified Data.Vector                 as V
import qualified Data.Vector.Mutable         as MV
import qualified Data.Vector.Unboxed         as UV
import qualified Data.Vector.Unboxed.Mutable as UMV

--import           Data.Container.Poly {- x -}
--import GHC.Prim

import qualified Data.Container.Opts as M
import           Data.Container.Opts (Opt(P,N), Knowledge(..), Query(..), OptQuery(..), ModsOf, ParamsOf)
import Data.Layer
import Data.Container.Type (In)

import Data.Container.Proxy

------------------------------
-- === Global instances === --
------------------------------

type instance Index     (V.Vector a) = Int
type instance Item      (V.Vector a) = a
type instance Container (V.Vector a) = V.Vector a
type instance DataStore (V.Vector a) = V.Vector a
instance      Monad m => IsContainerM  m (V.Vector a) where fromContainerM = return
instance      Monad m => HasContainerM m (V.Vector a) where viewContainerM = return
                                                            setContainerM  = const . return


instance ToList   (V.Vector a) where toList   = V.toList
instance FromList (V.Vector a) where fromList = V.fromList

----------------------------------
-- === Operations instances === --
----------------------------------

-- === Finite ===

-- [+] Measurable
-- [+] MinBounded
-- [+] MaxBounded

type instance ParamsOf MeasurableOp (V.Vector a) = '[]
type instance ModsOf   MeasurableOp (V.Vector a) = '[]

type instance ParamsOf MinBoundedOp (V.Vector a) = '[]
type instance ModsOf   MinBoundedOp (V.Vector a) = '[]

type instance ParamsOf MaxBoundedOp (V.Vector a) = '[]
type instance ModsOf   MaxBoundedOp (V.Vector a) = '[]

instance Monad m              => MeasurableQM_ '[] ps m     (V.Vector a) where sizeM_     _   = return . Res () . V.length
instance (Monad m, idx ~ Int) => MinBoundedQM_ '[] ps m idx (V.Vector a) where minBoundM_ _ _ = return $ Res () 0
instance (Monad m, idx ~ Int) => MaxBoundedQM_ '[] ps m idx (V.Vector a) where maxBoundM_ _ v = return $ Res () $ V.length v - 1


-- === Construction ===

-- [+] Singleton
-- [+] Allocable
-- [+] Expandable
-- [+] Growable

type instance ParamsOf SingletonOp  (V.Vector a) = '[]
type instance ModsOf   SingletonOp  (V.Vector a) = '[M.Ixed]

type instance ParamsOf AllocableOp  (V.Vector a) = '[]
type instance ModsOf   AllocableOp  (V.Vector a) = '[M.Ixed]

type instance ParamsOf ExpandableOp (V.Vector a) = '[]
type instance ModsOf   ExpandableOp (V.Vector a) = '[M.Ixed]

type instance ParamsOf GrowableOp   (V.Vector a) = '[]
type instance ModsOf   GrowableOp   (V.Vector a) = '[M.Ixed]

instance (Monad m, a ~ a') => SingletonQM_ '[N       ] ps m a' (V.Vector a) where singletonM_ _ = return . Res ()     . V.singleton
instance (Monad m, a ~ a') => SingletonQM_ '[P M.Ixed] ps m a' (V.Vector a) where singletonM_ _ = return . Res (0,()) . V.singleton

instance Monad m =>           AllocableQM_ '[N       ] ps m    (V.Vector a) where allocM_ _ i = return $ Res ()            $ runST $ V.unsafeFreeze =<< MV.unsafeNew i
instance Monad m =>           AllocableQM_ '[P M.Ixed] ps m    (V.Vector a) where allocM_ _ i = return $ Res ([0..i-1],()) $ runST $ V.unsafeFreeze =<< MV.unsafeNew i

instance Monad m =>           ExpandableQM_ '[N       ] ps m   (V.Vector a) where expandM_ _ v = return $ Res ()                $ runST $ V.unsafeThaw v >>= flip MV.unsafeGrow 1 >>= V.unsafeFreeze
instance Monad m =>           ExpandableQM_ '[P M.Ixed] ps m   (V.Vector a) where expandM_ _ v = return $ Res ([V.length v],()) $ runST $ V.unsafeThaw v >>= flip MV.unsafeGrow 1 >>= V.unsafeFreeze

instance Monad m =>           GrowableQM_   '[N       ] ps m   (V.Vector a) where growM_ _ i v = return $ Res ()                                      $ runST $ V.unsafeThaw v >>= flip MV.unsafeGrow i >>= V.unsafeFreeze
instance Monad m =>           GrowableQM_   '[P M.Ixed] ps m   (V.Vector a) where growM_ _ i v = return $ Res ([V.length v .. V.length v + i - 1],()) $ runST $ V.unsafeThaw v >>= flip MV.unsafeGrow i >>= V.unsafeFreeze


-- === Modification ===
-- [+] Appendable
-- [+] Prependable
-- [+] Addable
-- [+] Removable
-- [+] Insertable
-- [+] Freeable


type instance ParamsOf AppendableOp  (V.Vector a) = '[]
type instance ModsOf   AppendableOp  (V.Vector a) = '[M.Ixed]

type instance ParamsOf PrependableOp (V.Vector a) = '[]
type instance ModsOf   PrependableOp (V.Vector a) = '[M.Ixed]

type instance ParamsOf AddableOp     (V.Vector a) = '[]
type instance ModsOf   AddableOp     (V.Vector a) = '[M.Ixed]

type instance ParamsOf RemovableOp   (V.Vector a) = '[M.Try]
type instance ModsOf   RemovableOp   (V.Vector a) = '[M.Ixed]

type instance ParamsOf InsertableOp  (V.Vector a) = '[]
type instance ModsOf   InsertableOp  (V.Vector a) = '[M.Ixed]

type instance ParamsOf FreeableOp    (V.Vector a) = '[]
type instance ModsOf   FreeableOp    (V.Vector a) = '[]

instance (Monad m, a ~ a')       => AppendableQM_  '[N       ] ps m a' (V.Vector a) where appendM_  _ el v = (return . Res ())            $ V.snoc v el
instance (Monad m, a ~ a')       => AppendableQM_  '[P M.Ixed] ps m a' (V.Vector a) where appendM_  _ el v = (return . Res (size v,())) $ V.snoc v el

instance (Monad m, a ~ a')       => PrependableQM_ '[N       ] ps m a' (V.Vector a) where prependM_ _ el v = (return . Res ())     $ V.cons el v
instance (Monad m, a ~ a')       => PrependableQM_ '[P M.Ixed] ps m a' (V.Vector a) where prependM_ _ el v = (return . Res (0,())) $ V.cons el v

instance (Monad m, a ~ a')       => AddableQM_     '[N       ] ps m a' (V.Vector a) where addM_     _ el v = (return . Res ())            $ V.snoc v el
instance (Monad m, a ~ a')       => AddableQM_     '[P M.Ixed] ps m a' (V.Vector a) where addM_     _ el v = (return . Res (size v,())) $ V.snoc v el

instance (Monad m, Eq a, a ~ a') => RemovableQM_   '[N       ] '[P M.Try] m a' (V.Vector a) where removeM_ _ el v = case idx of
                                                                                                                           Just  i -> (return . Res ()) $ V.slice 0 (i-1) v <> V.slice i (size v - i) v
                                                                                                                           Nothing -> fail "Element not found"
                                                                                                                       where idx = V.findIndex (== el) v
instance (Monad m, Eq a, a ~ a') => RemovableQM_ '[P M.Ixed] '[P M.Try] m a' (V.Vector a) where removeM_ _ el v = case idx of
                                                                                                                         Just  i -> (return . Res (i,())) $ V.slice 0 (i-1) v <> V.slice i (size v - i) v
                                                                                                                         Nothing -> fail "Element not found"
                                                                                                                     where idx = V.findIndex (== el) v


instance (Monad m, a ~ a', idx ~ Int) => InsertableQM_ '[N       ] ps m idx a' (V.Vector a) where insertM_ _ idx el v = (return . Res ())       $ (V.//) v [(idx,el)]
instance (Monad m, a ~ a', idx ~ Int) => InsertableQM_ '[P M.Ixed] ps m idx a' (V.Vector a) where insertM_ _ idx el v = (return . Res (idx,())) $ (V.//) v [(idx,el)]


instance (Monad m, idx ~ Int) => FreeableQM_ '[] ps m idx (V.Vector a) where freeM_ _ idx v = (return . Res ())       $ (V.//) v [(idx,error $ "uninitialized element at index " <> show idx)]



-- === Indexing ===

-- [+] Indexable
-- [-] TracksFreeIxes
-- [-] TracksUsedIxes
-- [+] TracksIxes
-- [+] TracksElems

type instance ParamsOf IndexableOp       (V.Vector a) = '[M.Unchecked, M.Try]
type instance ModsOf   IndexableOp       (V.Vector a) = '[M.Ixed]

type instance ParamsOf TracksIxesOp      (V.Vector a) = '[]
type instance ModsOf   TracksIxesOp      (V.Vector a) = '[]

instance (Monad m, a ~ a', idx ~ Int, Cond2 unchecked, Cond2 try) => IndexableQM_  '[N       ] '[unchecked, try] m idx a' (V.Vector a) where indexM_  _ idx v = Res ()       <$> checkedBoundsIfM2 (Proxy :: Proxy unchecked) (Proxy :: Proxy try) idx v (V.unsafeIndex v idx)
instance (Monad m, a ~ a', idx ~ Int, Cond2 unchecked, Cond2 try) => IndexableQM_  '[P M.Ixed] '[unchecked, try] m idx a' (V.Vector a) where indexM_  _ idx v = Res (idx,()) <$> checkedBoundsIfM2 (Proxy :: Proxy unchecked) (Proxy :: Proxy try) idx v (V.unsafeIndex v idx)

instance (Monad m, idx ~ Int)                                     => TracksIxesQM_  '[]        ps                m idx    (V.Vector a) where ixesM_   _     v = (return . Res ()) $ [0 .. size v -1]
instance (Monad m, a ~ a')                                        => TracksElemsQM_ '[]        ps                m     a' (V.Vector a) where elemsM_  _     v = (return . Res ()) $ V.toList v



---- missing instances ----

instance Default (V.Vector a) where def = mempty


--- Utils (TODO: refactor)

failT2 p = ifT2 p fail error

checkBounds2 i v l r = if i > max || i < 0 then l ("index " <> show i <> " out of bounds x[0," <> show max <> "]") else r where max = size v - 1
checkBoundsM2 p idx v = checkBounds2 idx v (const . failT2 p) return
checkedBoundsIfM2 unchecked try idx v = checkedIfM2 unchecked (checkBoundsM2 try idx v)

checkedIfM2 p = ifT2 p return


class    Cond2 (opt :: Opt *) where ifT2 :: Proxy opt -> a -> a -> a
instance Cond2 (P a)          where ifT2 _ = const
instance Cond2 N              where ifT2 _ = flip const


