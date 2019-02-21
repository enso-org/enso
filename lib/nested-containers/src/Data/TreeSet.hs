{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}
{-# LANGUAGE OverloadedLists      #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.TreeSet where

import Prologue hiding (null)

import qualified GHC.Exts as GHC
import qualified Prelude  as P

import           Control.Lens               (mapping)
import           Control.Lens.Utils.Wrapped (unwrapped)
import           Data.TreeMap               (IsSparseValue, IsValue, JustValue,
                                             TreeMap, branches, checkVal, mkVal)
import qualified Data.TreeMap               as TreeMap


---------------------
-- === TreeSet === --
---------------------

-- === Definition === --

type SparseTreeSet = TreeSet Maybe
type SolidTreeSet  = TreeSet JustValue

newtype TreeSet t k = TreeSet (TreeMap t k ()) deriving (Mempty, Semigroup, Default, P.Monoid)
makeLenses ''TreeSet

instance (Show k, IsValue t) => Show (TreeSet t k) where show      = show . toList
-- instance (Show k, IsValue t) => Read (TreeSet t k) where readsPrec = fromList .: readsPrec


-- === Attributes === --

null :: IsValue t => TreeSet t k -> Bool
null = TreeMap.null . unwrap

keys   :: TreeSet t k -> [k]
elems  :: TreeSet t k -> [TreeSet t k]
assocs :: TreeSet t k -> [(k, TreeSet t k)]
keys   = TreeMap.keys . unwrap
elems  t = fmap  (wrap . view TreeMap.subtree) $ TreeMap.elems  (unwrap t)
assocs t = fmap2 (wrap . view TreeMap.subtree) $ TreeMap.assocs (unwrap t)

paths :: IsValue t => TreeSet t k -> [NonEmpty k]
paths = TreeMap.paths_ . unwrap


-- === Modification === --

insert :: (IsSparseValue t, Ord k) => NonEmpty k -> TreeSet t k -> TreeSet t k
insert ks = wrapped %~ TreeMap.insert ks ()

insertAllLevels :: (IsValue t, Ord k) => NonEmpty k -> TreeSet t k -> TreeSet t k
insertAllLevels ks = wrapped %~ TreeMap.insertDef (mkVal ()) ks ()

delete        :: (IsSparseValue t, Ord k) => NonEmpty k -> TreeSet t k -> TreeSet t k
deleteSubtree :: (IsValue       t, Ord k) => NonEmpty k -> TreeSet t k -> TreeSet t k
delete        ks = wrapped %~ TreeMap.delete        ks
deleteSubtree ks = wrapped %~ TreeMap.deleteSubtree ks


-- === Lookup & indexing === --

check :: (IsValue t, Ord k) => NonEmpty k -> TreeSet t k -> Bool
check ks = isJust . TreeMap.lookup ks . unwrap

check' :: (IsValue t, Ord k) => k -> TreeSet t k -> Bool
check' = check . (:| [])


-- Indexing
type instance Index    (TreeSet t k) = k
type instance IxValue  (TreeSet t k) = TreeSet t k
instance Ord k => Ixed (TreeSet t k)    where ix t = wrapped . ix t . unwrapped
instance Ord k => At   (SolidTreeSet k) where at t = wrapped . branches . at t . mapping (solidSubtreeIso . unwrapped)

solidSubtreeIso :: IsValue t => Iso' (TreeMap.TreeBranch t k ()) (TreeMap t k ())
solidSubtreeIso = iso (view TreeMap.subtree) (TreeMap.TreeBranch (mkVal ()))


-- === Traversals === --

foldBranches         :: (IsValue t)          =>                                   (a -> k -> Bool ->   a) -> a -> TreeSet t k ->   [a]
foldBranchesM        :: (IsValue t, Monad m) =>                                   (a -> k -> Bool -> m a) -> a -> TreeSet t k -> m [a]
foldBranchesM_       :: (IsValue t, Monad m) =>                                   (a -> k -> Bool -> m a) -> a -> TreeSet t k -> m ()
foldReduceBranches   :: (IsValue t)          => (a -> k -> Bool -> [c] ->   c) -> (a -> k -> Bool ->   a) -> a -> TreeSet t k ->   [c]
foldReduceBranchesM  :: (IsValue t, Monad m) => (a -> k -> Bool -> [c] -> m c) -> (a -> k -> Bool -> m a) -> a -> TreeSet t k -> m [c]
reduceBranches       :: (IsValue t)          =>      (k -> Bool -> [c] ->   c)                                 -> TreeSet t k ->   [c]
reduceBranchesM      :: (IsValue t, Monad m) =>      (k -> Bool -> [c] -> m c)                                 -> TreeSet t k -> m [c]
foldBranches          f b = TreeMap.foldBranches        (_ftrans f) b . unwrap
foldBranchesM         f b = TreeMap.foldBranchesM       (_ftrans f) b . unwrap
foldBranchesM_        f b = TreeMap.foldBranchesM_      (_ftrans f) b . unwrap
foldReduceBranches  h f b = TreeMap.foldReduceBranches  (_ftrans h) (_ftrans f) b . unwrap
foldReduceBranchesM h f b = TreeMap.foldReduceBranchesM (_ftrans h) (_ftrans f) b . unwrap
reduceBranches      h     = TreeMap.reduceBranches      (_ftrans2 h)  . unwrap
reduceBranchesM     h     = TreeMap.reduceBranchesM     (_ftrans2 h)  . unwrap

_ftrans :: IsValue t1 => (t2 -> t3 -> Bool -> t4) -> t2 -> t3 -> t1 a -> t4
_ftrans  f a k t = f a k (isJust $ checkVal t)

_ftrans2 :: forall (t1 :: * -> *) t2 t3 a. IsValue t1
    => (t2 -> Bool -> t3) -> t2 -> t1 a -> t3
_ftrans2 f   k t = f   k (isJust $ checkVal t)

instance IsValue t => Foldable (TreeSet t) where foldr f b = foldr f b . copyKeysToVals


-- === Conversions === --

copyKeysToVals :: IsValue t => TreeSet t k -> TreeMap t k k
copyKeysToVals = TreeMap.mapWithKey const . unwrap

-- Lists
type instance         Item     (TreeSet t k) = NonEmpty k
instance (IsValue t, s ~ Item (TreeSet t k)) => Convertible (TreeSet t k) [s] where convert = paths
instance (Ord k, s ~ Item (SparseTreeSet k)) => Convertible [s] (SparseTreeSet k) where
    convert []        = mempty
    convert (ks : ls) = insert ks $ convert ls
instance (Ord k, s ~ Item (SolidTreeSet k)) => Convertible [s] (SolidTreeSet k) where
    convert []        = mempty
    convert (ks : ls) = insertAllLevels ks $ convert ls
instance IsList (TreeSet t k) => GHC.IsList (TreeSet t k) where
    type Item   (TreeSet t k) = Item (TreeSet t k)
    toList   = toList
    fromList = fromList
