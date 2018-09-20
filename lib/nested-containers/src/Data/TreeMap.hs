{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}
{-# LANGUAGE OverloadedLists      #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.TreeMap where

import Prologue hiding (null)

import           Data.List.NonEmpty (NonEmpty ((:|)))
import           Data.Map           (Map)
import qualified Data.Map           as Map
import qualified GHC.Exts           as GHC
import qualified Prelude            as P

import Control.Lens              (_Just)
import Control.Lens.Utils.Nested (nestedDefAt, nestedIx)



------------------------------
-- === Value containers === --
------------------------------

-- === Definition === --

class Traversable t => IsValue t where
    ixVal    :: ∀ a. Traversal' (t a) a
    mkVal    :: ∀ a. a   -> t a
    checkVal :: ∀ a. t a -> Maybe a

class IsValue t => IsSparseValue t where
    emptyVal :: ∀ a. t a


-- === Utils === --

valExists :: IsValue t => t a -> Bool
valExists = isJust . checkVal
{-# INLINE valExists #-}

fromValWith :: IsValue t => b -> (a -> b) -> t a -> b
fromValWith = \b f -> fromJust b . fmap f . checkVal
{-# INLINE fromValWith #-}

fromVal :: IsValue t => a -> t a -> a
fromVal = \a -> fromMaybe a . checkVal
{-# INLINE fromVal #-}


-- === JustValue === --

newtype JustValue a = JustValue a
    deriving (Show, Functor, Foldable, Traversable, Default, Mempty, Semigroup)
makeLenses ''JustValue

instance IsValue JustValue where
    checkVal = Just . unwrap
    mkVal    = JustValue
    ixVal    = wrapped
    {-# INLINE checkVal #-}
    {-# INLINE mkVal    #-}
    {-# INLINE ixVal    #-}


-- === Maybe === --

instance IsSparseValue Maybe where
    emptyVal = Nothing
    {-# INLINE emptyVal #-}

instance IsValue Maybe where
    checkVal = id
    mkVal    = Just
    ixVal    = _Just
    {-# INLINE checkVal #-}
    {-# INLINE mkVal    #-}
    {-# INLINE ixVal    #-}



---------------------
-- === TreeMap === --
---------------------

-- === Definition === --

type SparseTreeMap = TreeMap Maybe
type SolidTreeMap  = TreeMap JustValue

newtype TreeMap t k a = TreeMap
    { _branches :: Map k (TreeBranch t k a) }
    deriving ( Default, Read, Foldable, Functor, Mempty, P.Monoid, Show
             , Semigroup, Traversable )

data TreeBranch t k a = TreeBranch
    { _value   :: !(t a)
    , _subtree :: !(TreeMap t k a)
    } deriving (Show, Read, Functor, Foldable, Traversable)

makeLenses ''TreeMap
makeLenses ''TreeBranch


-- === Helpers === --

justIfNotNullBranch :: IsValue t => TreeBranch t k a -> Maybe (TreeBranch t k a)
justIfNotNullBranch = \t -> justIf (not $ nullBranch t) t
{-# INLINE justIfNotNullBranch #-}


-- === Construction === --

singleton :: (IsValue t, Ord k) => k -> a -> TreeMap t k a
singleton = \k a -> mempty & branches . at k .~ Just (singletonBranch a)
{-# INLINE singleton #-}

singletonBranch :: IsValue t => a -> TreeBranch t k a
singletonBranch = \a -> TreeBranch (mkVal a) mempty
{-# INLINE singletonBranch #-}

instance Mempty (t a)
      => Mempty (TreeBranch t k a) where
    mempty = TreeBranch mempty mempty
    {-# INLINE mempty #-}

instance Default (t a)
     => Default (TreeBranch t k a) where
    def = TreeBranch def mempty
    {-# INLINE def #-}


-- === Attributes === --

null :: TreeMap t k a -> Bool
null = Map.null . unwrap
{-# INLINE null #-}

nullBranch :: IsValue t => TreeBranch t k a -> Bool
nullBranch t = (not . valExists $ t ^. value) && (null $ t ^. subtree)
{-# INLINE nullBranch #-}

keys   :: TreeMap t k a -> [k]
elems  :: TreeMap t k a -> [TreeBranch t k a]
assocs :: TreeMap t k a -> [(k, TreeBranch t k a)]
keys   = Map.keys   . unwrap
elems  = Map.elems  . unwrap
assocs = Map.assocs . unwrap
{-# INLINE keys   #-}
{-# INLINE elems  #-}
{-# INLINE assocs #-}

paths_ :: IsValue t => TreeMap t k a -> [NonEmpty k]
paths  :: IsValue t => TreeMap t k a -> [(NonEmpty k, a)]
paths_ = fst .: paths
paths t = concat $ (\(k,v) -> branchPaths (pure k) v) <$> assocs t where
    treePaths path tree
        = concat $ (\(k,v) -> branchPaths (path <> [k]) v) <$> assocs tree
    branchPaths path (TreeBranch v s)
        = fromValWith id ((:) . (path,)) v $ treePaths path s
{-# INLINE paths  #-}
{-# INLINE paths_ #-}

-- === Modification === --

insertDef :: (IsValue t, Ord k)
    => t a -> NonEmpty k -> a -> TreeMap t k a -> TreeMap t k a
insertDef = \d ks a t -> let
    emptyBranch = TreeBranch d mempty
    in t & branches . nestedDefAt emptyBranch ks
         %~ (Just . set value (mkVal a) . fromMaybe emptyBranch)
{-# INLINE insertDef #-}

insert :: (IsSparseValue t, Ord k)
    => NonEmpty k -> a -> TreeMap t k a -> TreeMap t k a
insert = insertDef emptyVal
{-# INLINE insert #-}

deleteAtSubbranch :: (Ord k, IsSparseValue t)
    => [k] -> TreeBranch t k a -> TreeBranch t k a
deleteAtSubbranch = \path t -> case path of
    []     -> t & value .~ emptyVal
    (k:ks) -> t & fromJust id
        (set (at k) . justIfNotNullBranch . deleteAtSubbranch ks <$> t ^. at k)
{-# INLINE deleteAtSubbranch #-}

deleteSubbranch :: (Ord k, IsValue t)
    => [k] -> TreeBranch t k a -> Maybe (TreeBranch t k a)
deleteSubbranch = \path t -> case path of
    []     -> Nothing
    (k:ks) -> justIfNotNullBranch
            $ t & at k %~ (join . fmap (deleteSubbranch ks))
{-# INLINE deleteSubbranch #-}

delete :: (Ord k, IsSparseValue t)
    => NonEmpty k -> TreeMap t k a -> TreeMap t k a
delete (k :| ks) = branches . at k
                %~ join . fmap (justIfNotNullBranch . deleteAtSubbranch ks)
{-# INLINE delete #-}

deleteSubtree :: (Ord k, IsValue t)
    => NonEmpty k -> TreeMap t k a -> TreeMap t k a
deleteSubtree (k :| ks) = branches . at k %~ join . fmap (deleteSubbranch ks)
{-# INLINE deleteSubtree #-}

modify :: (IsValue t, Ord k)
    => NonEmpty k -> (a -> a) -> TreeMap t k a -> TreeMap t k a
modify ks f = focus ks %~ f
{-# INLINE modify #-}

instance (Semigroup (t a), Ord k)
      => Semigroup (TreeBranch t k a) where
    TreeBranch v s <> TreeBranch v' s' = TreeBranch (v <> v') (s <> s')
    {-# INLINE (<>) #-}

instance (Monoid (t a), Ord k)
      => P.Monoid (TreeBranch t k a) where
    mempty  = mempty
    mappend = (<>)
    {-# INLINE mempty  #-}
    {-# INLINE mappend #-}

type instance Index    (TreeBranch t k a) = k
type instance Index    (TreeMap    t k a) = k
type instance IxValue  (TreeBranch t k a) = TreeBranch t k a
type instance IxValue  (TreeMap    t k a) = TreeMap    t k a

instance Ord k
      => At (TreeBranch t k a) where
    at = \s -> subtree . wrapped . at s
    {-# INLINE at #-}

instance Ord k
      => Ixed (TreeBranch t k a) where
    ix = \s -> subtree . wrapped . ix s
    {-# INLINE ix #-}

instance Ord k
      => Ixed (TreeMap t k a) where
    ix s = wrapped . ix s . subtree
    {-# INLINE ix #-}


-- === Traversals === --

mapWithKey :: IsValue t
    => (k -> a -> b) -> TreeMap t k a -> TreeMap t k b
mapWithKey = runIdentity .: traverseWithKey . (fmap . fmap $ pure)
{-# INLINE mapWithKey #-}

mapWithPath :: IsValue t
    => (NonEmpty k -> a -> b) -> TreeMap t k a -> TreeMap t k b
mapWithPath = runIdentity .: traverseWithPath . (fmap . fmap $ pure)
{-# INLINE mapWithPath #-}

traverseWithKey  :: (IsValue t, Monad m) =>          (k -> a -> m b) -> TreeMap t k a -> m (TreeMap t k b)
traverseWithPath :: (IsValue t, Monad m) => (NonEmpty k -> a -> m b) -> TreeMap t k a -> m (TreeMap t k b)
traverseWithKey = \f -> let
    traverseBranch k (TreeBranch v s)
        = TreeBranch <$> (mapM (f k) v)
                     <*> (s & wrapped (Map.traverseWithKey traverseBranch))
    in wrapped $ Map.traverseWithKey traverseBranch
{-# INLINE traverseWithKey #-}

traverseWithPath f = let
    traverseBranch path (TreeBranch v s)
        = TreeBranch <$> (mapM (f path) v)
                     <*> (s & wrapped (Map.traverseWithKey
                                          (\p -> traverseBranch $ path <> [p])))
    in wrapped $ Map.traverseWithKey (\p -> traverseBranch $ p :| [])
{-# INLINE traverseWithPath #-}

foldBranches           :: (IsValue t)          =>                                  (b -> k -> t a ->   b) -> b -> TreeMap t k a ->   [b]
foldBranchesM          :: (IsValue t, Monad m) =>                                  (b -> k -> t a -> m b) -> b -> TreeMap t k a -> m [b]
foldBranchesM_         :: (IsValue t, Monad m) =>                                  (b -> k -> t a -> m b) -> b -> TreeMap t k a -> m ()
foldReduceBranches     :: (IsValue t)          => (b -> k -> t a -> [c] ->   c) -> (b -> k -> t a ->   b) -> b -> TreeMap t k a ->   [c]
foldReduceBranchesM    :: (IsValue t, Monad m) => (b -> k -> t a -> [c] -> m c) -> (b -> k -> t a -> m b) -> b -> TreeMap t k a -> m [c]
reduceBranches         :: (IsValue t)          =>      (k -> t a -> [c] ->   c)                                -> TreeMap t k a ->   [c]
reduceBranchesM        :: (IsValue t, Monad m) =>      (k -> t a -> [c] -> m c)                                -> TreeMap t k a -> m [c]
foldReduceSubBranchesM :: (IsValue t, Monad m) => (b -> k -> t a -> [c] -> m c) -> (b -> k -> t a -> m b) -> b -> k -> TreeBranch t k a -> m c
foldBranches           = \  f b -> runIdentity . foldBranchesM (pure .:. f) b
foldBranchesM          = \  f b -> fmap concat . foldReduceBranchesM (\b _ ma subs -> pure $ concat subs & if valExists ma then (b:) else id) f b
foldBranchesM_         =           void .:. foldBranchesM
foldReduceBranches     = \h f b -> runIdentity . foldReduceBranchesM (pure .:: h) (pure .:. f) b
foldReduceBranchesM    = \h f b -> mapM (uncurry $ foldReduceSubBranchesM h f b) . assocs
reduceBranches         = \h     -> foldReduceBranches  (const h) (const3 ())          ()
reduceBranchesM        = \h     -> foldReduceBranchesM (const h) (const3 $ pure ()) ()
foldReduceSubBranchesM = \h f b k (TreeBranch v s) -> do
    b' <- f b k v
    h b' k v =<< foldReduceBranchesM h f b' s



-- === Lookup & indexing === --

lookup :: (Ord k, IsValue t) => NonEmpty k -> TreeMap t k a -> Maybe a
lookup (k :| ks) = join . fmap (lookupBranch ks) . view (branches . at k) where
    lookupBranch = \case []     -> checkVal . view value
                         (k:ks) -> join . fmap (lookupBranch ks) . view (at k)
{-# INLINE lookup #-}

focus :: (IsValue t, Ord k) => NonEmpty k -> Traversal' (TreeMap t k a) a
focus ks = branches . nestedIx ks . value . ixVal
{-# INLINE focus #-}


-- === Conversions === --

type instance Item (TreeMap t k a) = (NonEmpty k, a)

instance (IsValue t, s ~ Item (TreeMap t k a))
      => Convertible (TreeMap t k a) [s] where
    convert = paths
    {-# INLINE convert #-}

instance (Ord k, s ~ Item (TreeMap t k a))
      => Convertible [s] (SparseTreeMap k a) where
    convert = \case
        []             -> mempty
        ((ks, a) : ls) -> insert ks a $ convert ls
    {-# INLINE convert #-}

instance (Ord k, Default a, s ~ Item (SolidTreeMap k a))
      => Convertible [s] (SolidTreeMap k a) where
    convert = \case
        []             -> mempty
        ((ks, a) : ls) -> insertDef def ks a $ convert ls
    {-# INLINE convert #-}

instance IsList (TreeMap t k a)
      => GHC.IsList (TreeMap t k a) where
    type Item   (TreeMap t k a) = Item (TreeMap t k a)
    toList   = toList
    fromList = fromList
    {-# INLINE toList   #-}
    {-# INLINE fromList #-}
