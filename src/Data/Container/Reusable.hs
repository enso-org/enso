{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE DeriveAnyClass       #-}

module Data.Container.Reusable where

import Prelude hiding ((.))

import Control.Lens         hiding (Indexable, Ixed, index)
import Data.Container.Class
import Data.Container.List
import Data.Container.Opts  (Opt(P,N), Query(..), ModsOf, ParamsOf, Unchecked, Inplace, Ixed)
import Data.Container.Proxy
import Data.Default
import Data.Functor.Utils
import Data.Layer_OLD
import Data.List            ((\\))
import Data.Monoid
import GHC.Generics    (Generic)
import Control.DeepSeq (NFData, rnf)


----------------------
-- === Reusable === --
----------------------

data Reusable idx a = Reusable [idx] !a deriving (Generic, Functor, Foldable, Traversable, Monoid)
type Reusable'    a = Reusable (Index a) a

type instance Index     (Reusable idx a) = Index (Container a)
type instance Item      (Reusable idx a) = Item  (Container a)
type instance Container (Reusable idx a) = Reusable idx a
type instance DataStore (Reusable idx a) = Container a

instance Monad m => IsContainerM  m (Reusable idx a) where fromContainerM = return
instance Monad m => HasContainerM m (Reusable idx a) where viewContainerM = return
                                                           setContainerM  = const . return

type instance       Unlayered  (Reusable idx a) = a
instance            Layered    (Reusable idx a) where layered = lens (\(Reusable _ a) -> a) (\(Reusable ixs _) a -> Reusable ixs a)
instance Monad m => LayeredM m (Reusable idx a)

instance (IsContainer a, FromList (Container a))
      => FromList  (Reusable idx a) where fromList = Reusable mempty . fromContainer . fromList

instance Default a => Default (Reusable idx a) where def = Reusable def def

_indexes :: Lens' (Reusable idx a) [idx]
_indexes = lens (\(Reusable ixs _) -> ixs) (\(Reusable _ a) ixs -> Reusable ixs a)

instance (ToList (Container a), HasContainer a) => ToList (Reusable idx a) where toList = toList . view container . unlayer

instance ( Show (Item a), TracksElems (Item a) (Reusable idx a)
         ) => Show (Reusable idx a) where
    show a = "Reusable [" <> intercalate ", " (fmap show (elems_ a :: [Item a])) <> "]"


-- === Utils === --

type TrackUsedElems  t el = (TracksUsedIxes (Index t) t, Indexable (Index t) el t, Tup2RTup el ~ (el, ()))
type TrackUsedElems' t    = TrackUsedElems t (Item t)

usedElems :: forall t el. TrackUsedElems t el => t -> [el]
usedElems c = flip index c <$> (usedIxes c :: [Index t])


------------------------
-- === Instances === ---
------------------------

-- Normal Form
--instance (NFData [Item t], TrackUsedElems' t, t ~ Reusable idx a) => NFData (Reusable idx a) where
--    rnf t = rnf (usedElems t :: [Item t])

instance NFData a => NFData (Reusable idx a) where rnf (Reusable ixs a) = rnf a


-- === Finite ===

-- [+] Measurable
-- [+] MinBounded
-- [+] MaxBounded

type instance ParamsOf MeasurableOp (Reusable idx a) = ParamsOf MeasurableOp (Container a)
type instance ModsOf   MeasurableOp (Reusable idx a) = ModsOf   MeasurableOp (Container a)

type instance ParamsOf MinBoundedOp (Reusable idx a) = ParamsOf MinBoundedOp (Container a)
type instance ModsOf   MinBoundedOp (Reusable idx a) = ModsOf   MinBoundedOp (Container a)

type instance ParamsOf MaxBoundedOp (Reusable idx a) = ParamsOf MaxBoundedOp (Container a)
type instance ModsOf   MaxBoundedOp (Reusable idx a) = ModsOf   MaxBoundedOp (Container a)

instance (MeasurableQM (GetOpts ms) (GetOpts ps) m     a)             => MeasurableQM_ ms ps m     (Reusable idx  a) where sizeM_     _ = sizeQM     (Query :: Query (GetOpts ms) (GetOpts ps)) . unlayer
instance (MinBoundedQM (GetOpts ms) (GetOpts ps) m idx a, idx ~ idx') => MinBoundedQM_ ms ps m idx (Reusable idx' a) where minBoundM_ _ = minBoundQM (Query :: Query (GetOpts ms) (GetOpts ps)) . unlayer
instance (MaxBoundedQM (GetOpts ms) (GetOpts ps) m idx a, idx ~ idx') => MaxBoundedQM_ ms ps m idx (Reusable idx' a) where maxBoundM_ _ = maxBoundQM (Query :: Query (GetOpts ms) (GetOpts ps)) . unlayer



-- === Construction ===

-- [+] Singleton
-- [+] Allocable
-- [+] Expandable
-- [+] Growable

type instance ParamsOf SingletonOp  (Reusable idx a) = ParamsOf SingletonOp  (Container a)
type instance ModsOf   SingletonOp  (Reusable idx a) = ModsOf   SingletonOp  (Container a)

type instance ParamsOf AllocableOp  (Reusable idx a) = ParamsOf AllocableOp  (Container a)
type instance ModsOf   AllocableOp  (Reusable idx a) = ModsOf   AllocableOp  (Container a)

type instance ParamsOf ExpandableOp (Reusable idx a) = ParamsOf ExpandableOp (Container a)
type instance ModsOf   ExpandableOp (Reusable idx a) = ModsOf   ExpandableOp (Container a)

type instance ParamsOf GrowableOp   (Reusable idx a) = ParamsOf GrowableOp   (Container a)
type instance ModsOf   GrowableOp   (Reusable idx a) = ModsOf   GrowableOp   (Container a)

instance ( SingletonQM (Ixed ': GetOpts ms) (GetOpts ps) m el a, idx ~ Index (Container a)) => SingletonQM_ ms ps m el (Reusable idx a) where
    singletonM_ _ el = do Res (ix,ds) r <- singletonQM (Query :: Query (Ixed ': GetOpts ms) (GetOpts ps)) el
                          return $ Res ds $ Reusable [ix] r

instance ( AllocableQM (Ixed ': GetOpts ms) (GetOpts ps) m a, idx ~ Index (Container a)) => AllocableQM_ ms ps m (Reusable idx a) where
    allocM_ _ i = do Res (ixs,ds) r <- allocQM (Query :: Query (Ixed ': GetOpts ms) (GetOpts ps)) i
                     return $ Res ds $ Reusable ixs r

instance ( ExpandableQM (Ixed ': GetOpts ms) (GetOpts ps) m a, idx ~ Index (Container a)) => ExpandableQM_ ms ps m (Reusable idx a) where
    expandM_ _ (Reusable ixs a) = do Res (ixs',ds) r <- expandQM (Query :: Query (Ixed ': GetOpts ms) (GetOpts ps)) a
                                     return $ Res ds $ Reusable (ixs <> ixs') r

instance ( GrowableQM (Ixed ': GetOpts ms) (GetOpts ps) m a, idx ~ Index (Container a)) => GrowableQM_ ms ps m (Reusable idx a) where
    growM_ _ i (Reusable ixs a) = do Res (ixs',ds) r <- growQM (Query :: Query (Ixed ': GetOpts ms) (GetOpts ps)) i a
                                     return $ Res ds $ Reusable (ixs <> ixs') r



-- === Modification ===
-- [+] Appendable
-- [+] Prependable
-- [+] Addable
-- [ ] Removable
-- [+] Insertable
-- [+] Freeable
-- [+] Reservable

type instance ParamsOf AppendableOp   (Reusable idx a) = ParamsOf AppendableOp   (Container a)
type instance ModsOf   AppendableOp   (Reusable idx a) = ModsOf   AppendableOp   (Container a)

type instance ParamsOf PrependableOp  (Reusable idx a) = ParamsOf PrependableOp  (Container a)
type instance ModsOf   PrependableOp  (Reusable idx a) = ModsOf   PrependableOp  (Container a)

type instance ParamsOf AddableOp      (Reusable idx a) = ParamsOf AddableOp      (Container a)
type instance ModsOf   AddableOp      (Reusable idx a) = ModsOf   AddableOp      (Container a)

type instance ParamsOf InsertableOp   (Reusable idx a) = (Unchecked ': Inplace ': ParamsOf InsertableOp (Container a))
type instance ModsOf   InsertableOp   (Reusable idx a) = ModsOf   InsertableOp   (Container a)

type instance ParamsOf FreeableOp     (Reusable idx a) = ParamsOf FreeableOp     (Container a)
type instance ModsOf   FreeableOp     (Reusable idx a) = ModsOf   FreeableOp     (Container a)

type instance ParamsOf ReservableOp   (Reusable idx a) = '[]
type instance ModsOf   ReservableOp   (Reusable idx a) = '[Ixed]


instance (AppendableQM  (GetOpts ms) (GetOpts ps) m el a)           => AppendableQM_  ms ps m el   (Reusable idx  a) where appendM_  _      = nested layered . appendQM  (Query :: Query (GetOpts ms) (GetOpts ps))
instance (PrependableQM (GetOpts ms) (GetOpts ps) m el a)           => PrependableQM_ ms ps m el   (Reusable idx  a) where prependM_ _      = nested layered . prependQM (Query :: Query (GetOpts ms) (GetOpts ps))
instance (InsertableQM  (GetOpts ms) (GetOpts ps) m idx el a
         , ExpandableM m (Reusable idx a)
         , Result_ InsertableOp (IdxElInfo idx el (Container a)) (GetOpts ms) ~ Result_ AddableOp (ElInfo el (Reusable idx a)) (GetOpts ms)
         ) => AddableQM_     ms ps m el   (Reusable idx  a) where addM_     q el t = case view _indexes t of
                                                                                         (x:xs) -> fmap2 (Reusable xs) $ insertQM (Query :: Query (GetOpts ms) (GetOpts ps)) x el $ unlayer t
                                                                                         []     -> addM_ q el =<< expandM t

instance (InsertableQM (GetOpts ms) (GetOpts ps) m idx el a, idx ~ idx') => InsertableQM_ ms (P Unchecked ': P Inplace ': ps) m idx el (Reusable idx' a) where insertM_ _     = nested layered .: insertQM (Query :: Query (GetOpts ms) (GetOpts ps))
instance (FreeableQM   (GetOpts ms) (GetOpts ps) m idx    a, idx ~ idx') => FreeableQM_   ms ps                               m idx    (Reusable idx' a) where freeM_   _ idx = fmap2 (_indexes %~ (idx:)) . nested layered (freeQM (Query :: Query (GetOpts ms) (GetOpts ps)) idx)

instance (Monad m, idx ~ Index (Container a), ExpandableM m (Reusable idx a)) => ReservableQM_ '[P Ixed] ps m (Reusable idx  a) where
    reserveM_ q c@(Reusable []     a) = reserveM_ q =<< expandM c
    reserveM_ _   (Reusable (i:is) a) = return $ Res (i,()) (Reusable is a)



---- === Indexing ===

-- [+] Indexable
-- [+] TracksFreeIxes
-- [+] TracksUsedIxes
-- [+] TracksIxes
-- [+] TracksElems

type instance ParamsOf IndexableOp      (Reusable idx a) = ParamsOf IndexableOp      (Container a)
type instance ModsOf   IndexableOp      (Reusable idx a) = ModsOf   IndexableOp      (Container a)

type instance ParamsOf TracksIxesOp     (Reusable idx a) = ParamsOf TracksIxesOp (Container a)
type instance ModsOf   TracksIxesOp     (Reusable idx a) = ModsOf   TracksIxesOp (Container a)

type instance ParamsOf TracksFreeIxesOp (Reusable idx a) = '[]
type instance ModsOf   TracksFreeIxesOp (Reusable idx a) = '[]

type instance ParamsOf TracksUsedIxesOp (Reusable idx a) = '[]
type instance ModsOf   TracksUsedIxesOp (Reusable idx a) = '[]

type instance ParamsOf TracksElemsOp    (Reusable idx a) = '[]
type instance ModsOf   TracksElemsOp    (Reusable idx a) = '[]


instance (IndexableQM      (GetOpts ms) (GetOpts ps) m idx el a, idx ~ idx') => IndexableQM_       ms ps m idx el (Reusable idx' a) where indexM_     _ idx   = indexQM    (Query :: Query (GetOpts ms) (GetOpts ps)) idx . unlayer
instance (TracksIxesQM     (GetOpts ms) (GetOpts ps) m idx    a, idx ~ idx') => TracksIxesQM_      ms ps m idx    (Reusable idx' a) where ixesM_      _       = ixesQM     (Query :: Query (GetOpts ms) (GetOpts ps))     . unlayer
instance (Monad m, idx ~ idx')                                               => TracksFreeIxesQM_ '[] ps m idx    (Reusable idx' a) where freeIxesM_  _       = return . Res () . view _indexes

instance (TracksIxes idx (Reusable idx a)
         , TracksFreeIxes idx (Reusable idx a), idx ~ idx', Monad m, Eq idx) => TracksUsedIxesQM_ '[] ps m idx    (Reusable idx' a) where usedIxesM_  _     t = return $ Res () $ ixes t \\ freeIxes t

instance ( TracksUsedIxes  idx    (Reusable idx a)
         , Indexable       idx el (Reusable idx a)
         , Monad           m
         ) => TracksElemsQM_     '[] ps m     el (Reusable idx a) where elemsM_     _   t = return $ Res () $ fmap (flip index_ t) (usedIxes_ t :: [idx]) where
