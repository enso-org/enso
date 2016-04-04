{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE DeriveAnyClass       #-}


module Data.Container.Resizable where

import Prelude hiding ((.))

import           Control.Lens
import           Control.Monad.Identity
import           Data.Container.Class
import           Data.Container.List
import           Data.Container.Opts    (Query(..), ModsOf, ParamsOf)
import qualified Data.Container.Opts    as M
import           Data.Container.Proxy
import           Data.Default
import           Data.Layer_OLD
import           Data.Functor.Utils
import           GHC.Generics    (Generic)
import           Control.DeepSeq (NFData)

----------------------
-- === Resizable === --
----------------------

data Resizable style a = Resizable !style !a deriving (Generic, Show, Functor, Foldable, Traversable, Monoid)

type instance Index     (Resizable s a) = Index (Container a)
type instance Item      (Resizable s a) = Item  (Container a)
type instance Container (Resizable s a) = Resizable s a
type instance DataStore (Resizable s a) = Container a

instance Monad m => IsContainerM  m (Resizable s a) where fromContainerM = return
instance Monad m => HasContainerM m (Resizable s a) where viewContainerM = return
                                                          setContainerM  = const . return


type instance       Unlayered  (Resizable s a) = a
instance            Layered    (Resizable s a) where layered = lens (\(Resizable _ a) -> a) (\(Resizable s _) a -> Resizable s a)
instance Monad m => LayeredM m (Resizable s a)

instance (IsContainer a, FromList (Container a), Default s)
      => FromList  (Resizable s a) where fromList = Resizable def . fromContainer . fromList

instance (Default s, Default a) => Default (Resizable s a) where def = Resizable def def

instance (ToList (Container a), HasContainer a) => ToList (Resizable s a) where toList = toList . view container . unlayer

style :: Lens' (Resizable s a) s
style = lens (\(Resizable s _) -> s) (\(Resizable _ a) s -> Resizable s a)



-- === Styles ===

data    Minimal     = Minimal     deriving (Generic, NFData, Show)
data    Exponential = Exponential deriving (Generic, NFData, Show)
newtype Linear      = Linear Int  deriving (Generic, NFData, Show)

instance Default Minimal     where def = Minimal
instance Default Exponential where def = Exponential
instance Default Linear      where def = Linear 1

-- === Resizing utilities ====

class                                            ResizeStep s           t where resizeStep :: Resizable s t -> Int
instance                                         ResizeStep Minimal     t where resizeStep _                        = 1
instance                                         ResizeStep Linear      t where resizeStep (view style -> Linear i) = i
instance Measurable (Resizable Exponential t) => ResizeStep Exponential t where resizeStep = checkZeroSize . size
instance Measurable (Resizable Double      t) => ResizeStep Double      t where resizeStep = (2 *) . checkZeroSize . size

checkZeroSize :: (Num a, Eq a) => a -> a
checkZeroSize s = if s == 0 then 1 else s


------------------------
-- === Instances === ---
------------------------

-- Normal Form

instance (NFData style, NFData a) => NFData (Resizable style a)

-- === Finite ===

-- [+] Measurable
-- [+] MinBounded
-- [+] MaxBounded

type instance ParamsOf MeasurableOp (Resizable s a) = ParamsOf MeasurableOp (Container a)
type instance ModsOf   MeasurableOp (Resizable s a) = ModsOf   MeasurableOp (Container a)

type instance ParamsOf MinBoundedOp (Resizable s a) = ParamsOf MinBoundedOp (Container a)
type instance ModsOf   MinBoundedOp (Resizable s a) = ModsOf   MinBoundedOp (Container a)

type instance ParamsOf MaxBoundedOp (Resizable s a) = ParamsOf MaxBoundedOp (Container a)
type instance ModsOf   MaxBoundedOp (Resizable s a) = ModsOf   MaxBoundedOp (Container a)

instance (MeasurableQM (GetOpts ms) (GetOpts ps) m     a) => MeasurableQM_ ms ps m     (Resizable s a) where sizeM_     _ = sizeQM     (Query :: Query (GetOpts ms) (GetOpts ps)) . unlayer
instance (MinBoundedQM (GetOpts ms) (GetOpts ps) m idx a) => MinBoundedQM_ ms ps m idx (Resizable s a) where minBoundM_ _ = minBoundQM (Query :: Query (GetOpts ms) (GetOpts ps)) . unlayer
instance (MaxBoundedQM (GetOpts ms) (GetOpts ps) m idx a) => MaxBoundedQM_ ms ps m idx (Resizable s a) where maxBoundM_ _ = maxBoundQM (Query :: Query (GetOpts ms) (GetOpts ps)) . unlayer



-- === Construction ===

-- [+] Singleton
-- [+] Allocable
-- [+] Expandable
-- [+] Growable

type instance ParamsOf SingletonOp  (Resizable s a) = ParamsOf SingletonOp  (Container a)
type instance ModsOf   SingletonOp  (Resizable s a) = ModsOf   SingletonOp  (Container a)

type instance ParamsOf AllocableOp  (Resizable s a) = ParamsOf AllocableOp  (Container a)
type instance ModsOf   AllocableOp  (Resizable s a) = ModsOf   AllocableOp  (Container a)

type instance ParamsOf ExpandableOp (Resizable s a) = ParamsOf GrowableOp   (Container a)
type instance ModsOf   ExpandableOp (Resizable s a) = ModsOf   GrowableOp   (Container a)

type instance ParamsOf GrowableOp   (Resizable s a) = ParamsOf GrowableOp   (Container a)
type instance ModsOf   GrowableOp   (Resizable s a) = ModsOf   GrowableOp   (Container a)

instance (SingletonQM  (GetOpts ms) (GetOpts ps) m el a, Default s) => SingletonQM_  ms ps m el (Resizable s a) where  singletonM_ _ = fmap2 (Resizable def) . singletonQM (Query :: Query (GetOpts ms) (GetOpts ps))
instance (AllocableQM  (GetOpts ms) (GetOpts ps) m    a, Default s) => AllocableQM_  ms ps m    (Resizable s a) where  allocM_     _ = fmap2 (Resizable def) . allocQM     (Query :: Query (GetOpts ms) (GetOpts ps))
instance ( GrowableQM  (GetOpts ms) (GetOpts ps) m    a, ResizeStep s a
         , Result_ GrowableOp (PrimInfo (Container a)) (GetOpts ms) ~ Result_ ExpandableOp (PrimInfo (Resizable s a)) (GetOpts ms)
         ) => ExpandableQM_ ms ps m    (Resizable s a) where  expandM_    _ t = nested layered (growQM (Query :: Query (GetOpts ms) (GetOpts ps)) $ resizeStep t) t



-- === Modification ===
-- [+] Appendable
-- [+] Prependable
-- [+] Addable
-- [+] Removable
-- [+] Insertable
-- [+] Freeable

type instance ParamsOf AppendableOp   (Resizable s a) = ParamsOf AppendableOp  (Container a)
type instance ModsOf   AppendableOp   (Resizable s a) = ModsOf   AppendableOp  (Container a)

type instance ParamsOf PrependableOp  (Resizable s a) = ParamsOf PrependableOp (Container a)
type instance ModsOf   PrependableOp  (Resizable s a) = ModsOf   PrependableOp (Container a)

type instance ParamsOf AddableOp      (Resizable s a) = ParamsOf AddableOp     (Container a)
type instance ModsOf   AddableOp      (Resizable s a) = ModsOf   AddableOp     (Container a)

type instance ParamsOf RemovableOp    (Resizable s a) = ParamsOf RemovableOp   (Container a)
type instance ModsOf   RemovableOp    (Resizable s a) = ModsOf   RemovableOp   (Container a)

type instance ParamsOf InsertableOp   (Resizable s a) = ParamsOf InsertableOp  (Container a)
type instance ModsOf   InsertableOp   (Resizable s a) = ModsOf   InsertableOp  (Container a)

type instance ParamsOf FreeableOp     (Resizable s a) = ParamsOf FreeableOp    (Container a)
type instance ModsOf   FreeableOp     (Resizable s a) = ModsOf   FreeableOp    (Container a)

instance (AppendableQM  (GetOpts ms) (GetOpts ps) m     el a) => AppendableQM_  ms ps m     el (Resizable s a) where appendM_  _ = nested layered .  appendQM  (Query :: Query (GetOpts ms) (GetOpts ps))
instance (PrependableQM (GetOpts ms) (GetOpts ps) m     el a) => PrependableQM_ ms ps m     el (Resizable s a) where prependM_ _ = nested layered .  prependQM (Query :: Query (GetOpts ms) (GetOpts ps))
instance (AddableQM     (GetOpts ms) (GetOpts ps) m     el a) => AddableQM_     ms ps m     el (Resizable s a) where addM_     _ = nested layered .  addQM     (Query :: Query (GetOpts ms) (GetOpts ps))
instance (RemovableQM   (GetOpts ms) (GetOpts ps) m     el a) => RemovableQM_   ms ps m     el (Resizable s a) where removeM_  _ = nested layered .  removeQM  (Query :: Query (GetOpts ms) (GetOpts ps))
instance (InsertableQM  (GetOpts ms) (GetOpts ps) m idx el a) => InsertableQM_  ms ps m idx el (Resizable s a) where insertM_  _ = nested layered .: insertQM  (Query :: Query (GetOpts ms) (GetOpts ps))
instance (FreeableQM    (GetOpts ms) (GetOpts ps) m idx    a) => FreeableQM_    ms ps m idx    (Resizable s a) where freeM_    _ = nested layered .  freeQM    (Query :: Query (GetOpts ms) (GetOpts ps))




---- === Indexing ===

-- [+] Indexable
-- [+] TracksFreeIxes
-- [+] TracksUsedIxes
-- [+] TracksIxes
-- [+] TracksElems

type instance ParamsOf IndexableOp      (Resizable s a) = ParamsOf IndexableOp      (Container a)
type instance ModsOf   IndexableOp      (Resizable s a) = ModsOf   IndexableOp      (Container a)

type instance ParamsOf TracksIxesOp     (Resizable s a) = ParamsOf TracksIxesOp     (Container a)
type instance ModsOf   TracksIxesOp     (Resizable s a) = ModsOf   TracksIxesOp     (Container a)

type instance ParamsOf TracksFreeIxesOp (Resizable s a) = ParamsOf TracksFreeIxesOp (Container a)
type instance ModsOf   TracksFreeIxesOp (Resizable s a) = ModsOf   TracksFreeIxesOp (Container a)

type instance ParamsOf TracksUsedIxesOp (Resizable s a) = ParamsOf TracksUsedIxesOp (Container a)
type instance ModsOf   TracksUsedIxesOp (Resizable s a) = ModsOf   TracksUsedIxesOp (Container a)

type instance ParamsOf TracksElemsOp    (Resizable s a) = ParamsOf TracksElemsOp    (Container a)
type instance ModsOf   TracksElemsOp    (Resizable s a) = ModsOf   TracksElemsOp    (Container a)

instance (IndexableQM      (GetOpts ms) (GetOpts ps) m idx el a) => IndexableQM_       ms ps m idx el (Resizable s a) where indexM_     _ idx   = indexQM    (Query :: Query (GetOpts ms) (GetOpts ps)) idx . unlayer
instance (TracksIxesQM     (GetOpts ms) (GetOpts ps) m idx    a) => TracksIxesQM_      ms ps m idx    (Resizable s a) where ixesM_      _       = ixesQM     (Query :: Query (GetOpts ms) (GetOpts ps))     . unlayer
instance (TracksFreeIxesQM (GetOpts ms) (GetOpts ps) m idx    a) => TracksFreeIxesQM_  ms ps m idx    (Resizable s a) where freeIxesM_  _       = freeIxesQM (Query :: Query (GetOpts ms) (GetOpts ps))     . unlayer
instance (TracksUsedIxesQM (GetOpts ms) (GetOpts ps) m idx    a) => TracksUsedIxesQM_  ms ps m idx    (Resizable s a) where usedIxesM_  _       = usedIxesQM (Query :: Query (GetOpts ms) (GetOpts ps))     . unlayer
instance (TracksElemsQM    (GetOpts ms) (GetOpts ps) m     el a) => TracksElemsQM_     ms ps m     el (Resizable s a) where elemsM_     _       = elemsQM    (Query :: Query (GetOpts ms) (GetOpts ps))     . unlayer











---- === TF Instances ===

--type instance Container (HResizable l a) = HResizable l a

--instance IsContainer  (HResizable l a) where fromContainer = id
--instance HasContainer (HResizable l a) where container     = id


--type instance Item        (HResizable l a) = Item       a
--type instance ElementByIx  idx (HResizable l a) = ElementByIx idx a
--type instance Index      el  (HResizable l a) = Index     el  a

--type instance DataStore (HResizable l a) = DataStore a


---- === Finite ===

---- [+] Measurable
---- [+] MinBounded
---- [+] MaxBounded


--type instance ModsOf MeasurableQSM (HResizable l a) = ModsOf MeasurableQSM a
--type instance ModsOf MinIndexedQSM (HResizable l a) = ModsOf MinIndexedQSM a
--type instance ModsOf MaxIndexedQSM (HResizable l a) = ModsOf MaxIndexedQSM a

--instance MeasurableQM q m a => MeasurableQSM (HResizable l a) m q s where sizeQSM     _ _ = queried (Proxy :: Proxy q) sizeM'     . unlayer
--instance MinIndexedQM idx q m a => MinIndexedQSM idx (HResizable l a) m q s where minIndexQSM _ _ = queried (Proxy :: Proxy q) minIndexM' . unlayer
--instance MaxIndexedQM idx q m a => MaxIndexedQSM idx (HResizable l a) m q s where maxIndexQSM _ _ = queried (Proxy :: Proxy q) maxIndexM' . unlayer


---- === Construction ===

---- [+] Singleton
---- [+] Allocable
---- [+] Expandable
---- [+] Growable

--        --type instance ModsOf SingletonQSM (HResizable l a) = ModsOf SingletonQSM a
--        --instance (SingletonQM el q m a, Default l) => SingletonQSM el (HResizable l a) m q s where singletonQSM _ _    = (fmap . fmap) wrap . queried (Proxy :: Proxy q) singletonM'

--        --type instance ModsOf AllocableQSM (HResizable l a) = ModsOf AllocableQSM a
--        --instance (AllocableQM q m a, Default l) => AllocableQSM (HResizable l a) m q s where allocQSM _ _    = (fmap . fmap) wrap . queried (Proxy :: Proxy q) allocM'

--type instance ModsOf ExpandableQSM (HResizable l a) = ModsOf GrowableQSM a
--instance (GrowableQM q m a, TransCheck q GrowableInfo ExpandableInfo a, ResizeStep l a) => ExpandableQSM (HResizable l a) m q s where expandQSM _ _ c = nested layered (queried (Proxy :: Proxy q) growM' (resizeStep c)) c

--type instance ModsOf GrowableQSM (HResizable l a) = ModsOf GrowableQSM a
--instance GrowableQM q m a => GrowableQSM (HResizable l a) m q s where growQSM _ _    = nested layered . queried (Proxy :: Proxy q) growM'


---- === Modification ===

---- [+] Appendable
---- [ ] Prependable
---- [ ] Addable
---- [ ] Removable

--type instance ModsOf AppendableQSM (HResizable l a) = ModsOf AppendableQSM a
--type instance ModsOf InsertableQSM (HResizable l a) = ModsOf InsertableQSM a

--instance   AppendableQM     el q m a => AppendableQSM     el (HResizable l a) m q s where appendQSM _ _ = nested layered .  queried (Proxy :: Proxy q) appendM'
--instance   InsertableQM idx el q m a => InsertableQSM idx el (HResizable l a) m q s where insertQSM _ _ = nested layered .: queried (Proxy :: Proxy q) insertM'



------ === Indexing ===

---- [+] Indexable
---- [ ] TracksElems
---- [ ] TracksIxes
---- [ ] TracksFreeIxes
---- [ ] TracksUsedIxes


--type instance ModsOf IndexableQSM   (HResizable l a) = ModsOf IndexableQSM   a
--type instance ModsOf TracksIxesQSM  (HResizable l a) = ModsOf TracksIxesQSM  a
--type instance ModsOf TracksElemsQSM (HResizable l a) = ModsOf TracksElemsQSM a

--instance   IndexableQM   idx el q m a => IndexableQSM   idx el (HResizable l a) m q s where indexQSM _ _ idx = queried (Proxy :: Proxy q) indexM' idx . unlayer
--instance   TracksIxesQM  idx    q m a => TracksIxesQSM  idx    (HResizable l a) m q s where ixesQSM  _ _     = queried (Proxy :: Proxy q) ixesM'      . unlayer
--instance   TracksElemsQM     el q m a => TracksElemsQSM     el (HResizable l a) m q s where elemsQSM _ _     = queried (Proxy :: Proxy q) elemsM'     . unlayer




--------------------------------------


--data Minimal     = Minimal   deriving (Show)
--data Exponential = Exponential deriving (Show)

--instance Default Minimal     where def = Minimal
--instance Default Exponential where def = Exponential

------class Resize2 style cont where
------    resizeAmount :: Proxy style -> cont -> Int

----class                                                                                               Resize style       cont idx where resize     :: idx -> HResizable style cont -> HResizable style cont
----instance (                       I.MaxIndexed cont idx, I.Growable cont cont, Enum idx, Ord idx) => Resize Minimal     cont idx where resize idx c = if isOverBounds idx c then flip I.growQSM c $ ((-) `on` fromEnum) idx $ I.maxIndex c else c
----instance (I.Measurable cont Int, I.MaxIndexed cont idx, I.Growable cont cont, Enum idx, Ord idx) => Resize Exponential cont idx where resize idx c = if isOverBounds idx c then flip I.growQSM c $ dupCheckSize (fromEnum idx) (I.sizeQSM c) - I.sizeQSM c else c

----class                             ResizeStep style       cont where resizeStep :: HResizable style cont -> Int
----instance                          ResizeStep Minimal     cont where resizeStep c = 1
----instance I.Measurable cont Int => ResizeStep Exponential cont where resizeStep c = checkZeroSize $ I.sizeQSM c





----dupCheckSize i = dupSize i . checkZeroSize

----dupSize i sizeQSM = if i >= sizeQSM then dupSize i (2 * sizeQSM)
----                              else sizeQSM


----isOverBounds :: (Ord idx, I.MaxIndexed cont idx, HasContainer t cont) => idx -> t -> Bool
----isOverBounds idx cont = idx > I.maxIndex cont







---- ---- TODO ----
---- after doing it we could be able to optimize the premise of ResizeStep Exponential and make inference nicer
----
---- -- Optimize following use cases:
----
---- xxx :: (MeasurableQM2 '[] Identity t, (ResultX
----                         (Info NA NA MeasurableQSM2 (DataStore t))
----                         (Selected
----                            (LstIn (ModsOf MeasurableQSM2 t) '[])
----                            (FilterMutable (ModsOf MeasurableQSM2 t)))
----                       ~ ()),
----
---- DataFillable
----                                 '[]
----                                 (TaggedCont
----                                    (Selected
----                                       (LstIn (ModsOf MeasurableQSM2 t) '[])
----                                       (FilterMutable (ModsOf MeasurableQSM2 t)))
----                                    ()), (Taggable
----                         (Selected
----                            (LstIn (ModsOf MeasurableQSM2 t) '[])
----                            (FilterMutable (ModsOf MeasurableQSM2 t)))
----                         ())) => HResizable style t -> Int
----
---- --- in particular:
----
---- (Selected
----                            (LstIn (ModsOf MeasurableQSM2 t) '[])
----                            (FilterMutable (ModsOf MeasurableQSM2 t)))
----
---- -- should always return []
