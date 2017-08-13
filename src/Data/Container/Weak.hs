{-# LANGUAGE CPP                       #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE FunctionalDependencies    #-}

module Data.Container.Weak where

import Prelude hiding ((.))

import Control.Lens
import Control.Lens.Utils
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Container.Class
import Data.Container.List
import Data.Container.Proxy
import Data.Default
import Data.Functor.Utils
import Data.Layer_OLD

import Data.Container.Immersed (withDivedM)
import Data.Container.Opts     (Query(..), ModsOf, ParamsOf)
import System.IO.Unsafe        (unsafePerformIO)

import qualified Data.Container.Opts as M
import qualified System.Mem.Weak     as Mem
import GHC.Generics (Generic)
------------------
-- === Weak === --
------------------

data Weak f a = Weak !(Maybe f) !a deriving (Generic, Functor, Foldable, Traversable, Default, Monoid)
type Weak'  a = Weak (IdxFinalizer (Index a)) a

-- Instances

type instance Index     (Weak f a) = Index (Container a)
type instance Item      (Weak f a) = WeakData (Item (Container a))
type instance Container (Weak f a) = Weak f a
type instance DataStore (Weak f a) = Container a
type instance Finalizer (Weak f a) = Maybe f

instance      Monad m => IsContainerM  m (Weak f a) where fromContainerM = return
instance      Monad m => HasContainerM m (Weak f a) where viewContainerM = return
                                                          setContainerM  = const . return

type instance Unlayered (Weak f a) = a
instance      Layered   (Weak f a) where layered = lens (\(Weak _ a) -> a) (\(Weak f _) a -> Weak f a)



-- === Finalizers ===

newtype IdxFinalizer idx = IdxFinalizer (idx -> IO ())

type family Finalizer  a
class       HasFinalizer a where finalizer :: Lens' a (Finalizer a)

class       HasFinalizerM a m f | a -> f where viewFinalizerM :: a -> m (Maybe f)
                                               setFinalizerM  :: Maybe f -> a -> m a

-- Basic finalizer instances

instance {-# OVERLAPPABLE #-} ( Finalizer (Unlayered a) ~ Finalizer a
                              , HasFinalizer (Unlayered a)
                              , Layered a)
                           => HasFinalizer a          where finalizer = layered . finalizer
instance {-# OVERLAPPABLE #-} HasFinalizer (Weak f a) where finalizer = lens (\(Weak f _) -> f) (\(Weak _ a) f -> Weak f a)




instance {-# OVERLAPPABLE #-} Monad m => HasFinalizerM (Weak f a) m f where viewFinalizerM   (Weak f _) = return f
                                                                            setFinalizerM  f (Weak _ a) = return $ Weak f a

instance {-# OVERLAPPABLE #-} ( Monad m
                              , HasFinalizerM (Container (Unlayered a)) m f
                              , HasContainerM m (Unlayered a)
                              , LayeredM m a
                              )
      => HasFinalizerM a m f where viewFinalizerM    = viewLayeredM >=> viewContainerM >=> viewFinalizerM
                                   setFinalizerM v a = flip withDivedM a $ setFinalizerM v


-- Instances

instance Rewrapped (IdxFinalizer idx) (IdxFinalizer idx')
instance Wrapped   (IdxFinalizer idx) where
    type Unwrapped (IdxFinalizer idx) = idx -> IO ()
    _Wrapped' = iso (\(IdxFinalizer f) -> f) IdxFinalizer



instance Monoid (IdxFinalizer idx) where
    mempty = wrap' . const $ return ()
    mappend (unwrap' -> f) (unwrap' -> f') = wrap' $ \idx -> f idx >> f' idx

-- items

type family WeakData a where WeakData (Mem.Weak a) = a
type WeakItemAxiom t = Item (Container t) ~ Mem.Weak (WeakData (Item (Container t)))
instance (WeakItemAxiom a, IsContainer a, FromList (Container a)) => FromList (Weak f a) where
    fromList = Weak def . fromContainer . fromList . fmap (unsafePerformIO . flip Mem.mkWeakPtr Nothing)
    {-# NOINLINE fromList #-}


---- utils

mkWeakPtr = liftIO .: Mem.mkWeakPtr

----instance Show (Weak f a) where
----    showsPrec d (Weak f a) = showParen (d > app_prec) $
----            showString "Weak " . showsPrec (succ app_prec) (fmap (unsafePerformIO . Mem.deRefWeak) c)
----         where app_prec = 10
----    {-# NOINLINE showsPrec #-}


------------------------
-- === Instances === ---
------------------------

-- === Finite ===

-- [+] Measurable
-- [+] MinBounded
-- [+] MaxBounded

type instance ParamsOf MeasurableOp (Weak f a) = ParamsOf MeasurableOp (Container a)
type instance ModsOf   MeasurableOp (Weak f a) = ModsOf   MeasurableOp (Container a)

type instance ParamsOf MinBoundedOp (Weak f a) = ParamsOf MinBoundedOp (Container a)
type instance ModsOf   MinBoundedOp (Weak f a) = ModsOf   MinBoundedOp (Container a)

type instance ParamsOf MaxBoundedOp (Weak f a) = ParamsOf MaxBoundedOp (Container a)
type instance ModsOf   MaxBoundedOp (Weak f a) = ModsOf   MaxBoundedOp (Container a)

instance (Monad m, MeasurableQM (GetOpts ms) (GetOpts ps) m     a) => MeasurableQM_ ms ps m     (Weak f a) where sizeM_     _ = sizeQM     (Query :: Query (GetOpts ms) (GetOpts ps)) . unlayer
instance (Monad m, MinBoundedQM (GetOpts ms) (GetOpts ps) m idx a) => MinBoundedQM_ ms ps m idx (Weak f a) where minBoundM_ _ = minBoundQM (Query :: Query (GetOpts ms) (GetOpts ps)) . unlayer
instance (Monad m, MaxBoundedQM (GetOpts ms) (GetOpts ps) m idx a) => MaxBoundedQM_ ms ps m idx (Weak f a) where maxBoundM_ _ = maxBoundQM (Query :: Query (GetOpts ms) (GetOpts ps)) . unlayer


-- === Construction ===

-- [+] Singleton
-- [+] Allocable
-- [+] Expandable
-- [+] Growable

type instance ParamsOf SingletonOp  (Weak f a) = ParamsOf SingletonOp  (Container a)
type instance ModsOf   SingletonOp  (Weak f a) = ModsOf   SingletonOp  (Container a)

type instance ParamsOf AllocableOp  (Weak f a) = ParamsOf AllocableOp  (Container a)
type instance ModsOf   AllocableOp  (Weak f a) = ModsOf   AllocableOp  (Container a)

type instance ParamsOf ExpandableOp (Weak f a) = ParamsOf GrowableOp   (Container a)
type instance ModsOf   ExpandableOp (Weak f a) = ModsOf   GrowableOp   (Container a)

type instance ParamsOf GrowableOp   (Weak f a) = ParamsOf GrowableOp   (Container a)
type instance ModsOf   GrowableOp   (Weak f a) = ModsOf   GrowableOp   (Container a)

instance (Monad m,  MonadIO m
         , SingletonQM  (GetOpts ms) (GetOpts ps) m (Mem.Weak el) a
         , (Result_ SingletonOp (ElInfo (Mem.Weak el) (Container a)) (GetOpts ms) ~ Result_ SingletonOp (ElInfo el (Weak f a)) (GetOpts ms))
         ) => SingletonQM_  ms ps m el (Weak f a) where
    singletonM_ _ el = (fmap2 (Weak def) . singletonQM (Query :: Query (GetOpts ms) (GetOpts ps))) =<< (liftIO . flip mkWeakPtr Nothing) el

instance (Monad m, AllocableQM  (GetOpts ms) (GetOpts ps) m    a) => AllocableQM_  ms ps m    (Weak f a) where  allocM_     _ = fmap2 (Weak def) . allocQM     (Query :: Query (GetOpts ms) (GetOpts ps))
instance (Monad m, ExpandableQM (GetOpts ms) (GetOpts ps) m    a) => ExpandableQM_ ms ps m    (Weak f a) where  expandM_    _ = nested layered   $ expandQM    (Query :: Query (GetOpts ms) (GetOpts ps))
instance (Monad m, GrowableQM   (GetOpts ms) (GetOpts ps) m    a) => GrowableQM_   ms ps m    (Weak f a) where  growM_      _ = nested layered   . growQM      (Query :: Query (GetOpts ms) (GetOpts ps))



-- === Modification ===
-- [+] Appendable
-- [+] Prependable
-- [+] Addable
-- [ ] Removable
-- [ ] Insertable
-- [+] Freeable

type instance ParamsOf AppendableOp   (Weak f a) = ParamsOf AppendableOp  (Container a)
type instance ModsOf   AppendableOp   (Weak f a) = ModsOf   AppendableOp  (Container a)

type instance ParamsOf PrependableOp  (Weak f a) = ParamsOf PrependableOp (Container a)
type instance ModsOf   PrependableOp  (Weak f a) = ModsOf   PrependableOp (Container a)

type instance ParamsOf AddableOp      (Weak f a) = ParamsOf AddableOp     (Container a)
type instance ModsOf   AddableOp      (Weak f a) = ModsOf   AddableOp     (Container a)

type instance ParamsOf FreeableOp     (Weak f a) = ParamsOf FreeableOp    (Container a)
type instance ModsOf   FreeableOp     (Weak f a) = ModsOf   FreeableOp    (Container a)


instance (Monad m, AppendableQM  (M.Ixed ': GetOpts ms) (GetOpts ps) m (Mem.Weak el) a
         , Result_ AppendableOp (ElInfo (Mem.Weak el) (Container a)) (GetOpts ms) ~ Result_ AppendableOp (ElInfo el (Weak (IdxFinalizer idx) a)) (GetOpts ms)
         , idx ~ Index (Container a)
         , MonadIO  m
         , MonadFix m
         ) => AppendableQM_  ms ps m el (Weak (IdxFinalizer idx) a) where
    appendM_  _ el t@(Weak mf a) = mdo
        Res (ix,ds) r <- appendQM (Query :: Query (M.Ixed ': GetOpts ms) (GetOpts ps)) ref a
        ref           <- mkWeakPtr el $ fmap (($ ix) . unwrap) mf
        return $ Res ds (Weak mf r)

instance (Monad m, PrependableQM  (M.Ixed ': GetOpts ms) (GetOpts ps) m (Mem.Weak el) a
         , Result_ PrependableOp (ElInfo (Mem.Weak el) (Container a)) (GetOpts ms) ~ Result_ PrependableOp (ElInfo el (Weak (IdxFinalizer idx) a)) (GetOpts ms)
         , idx ~ Index (Container a)
         , MonadIO  m
         , MonadFix m
         ) => PrependableQM_  ms ps m el (Weak (IdxFinalizer idx) a) where
    prependM_  _ el t@(Weak mf a) = mdo
        Res (ix,ds) r <- prependQM (Query :: Query (M.Ixed ': GetOpts ms) (GetOpts ps)) ref a
        ref           <- mkWeakPtr el $ fmap (($ ix) . unwrap) mf
        return $ Res ds (Weak mf r)

instance (Monad m, AddableQM  (M.Ixed ': GetOpts ms) (GetOpts ps) m (Mem.Weak el) a
         , Result_ AddableOp (ElInfo (Mem.Weak el) (Container a)) (GetOpts ms) ~ Result_ AddableOp (ElInfo el (Weak (IdxFinalizer idx) a)) (GetOpts ms)
         , idx ~ Index (Container a)
         , MonadIO  m
         , MonadFix m
         ) => AddableQM_  ms ps m el (Weak (IdxFinalizer idx) a) where
    addM_  _ el t@(Weak mf a) = mdo
        Res (ix,ds) r <- addQM (Query :: Query (M.Ixed ': GetOpts ms) (GetOpts ps)) ref a
        ref           <- mkWeakPtr el $ fmap (($ ix) . unwrap) mf
        return $ Res ds (Weak mf r)

instance (Monad m, FreeableQM (GetOpts ms) (GetOpts ps) m idx a, idx ~ idx') => FreeableQM_  ms ps m idx (Weak (IdxFinalizer idx) a) where freeM_ _ = nested layered . freeQM (Query :: Query (GetOpts ms) (GetOpts ps))

        --flip (nested layered) t $ appendQM (Query :: Query (GetOpts ms) (GetOpts ps)) =<< liftIO (flip Mem.mkWeakPtr f el)
--instance (Monad m, PrependableQM (GetOpts ms) (GetOpts ps) m el a)           => PrependableQM_ ms ps m el   (Weak idx  a) where prependM_ _      = nested layered . prependQM (Query :: Query (GetOpts ms) (GetOpts ps))
--instance (InsertableM m idx el a, ExpandableM m (Weak f a))   => AddableQM_    '[] ps m el   (Weak idx  a) where addM_     q el t = case view indexes t of
--                                                                                                                               (x:xs) -> fmap2 (Weak xs) $ insertM' x el $ unlayer t
--                                                                                                                               []     -> addM_ q el =<< expandM t
--instance (FreeableQM (GetOpts ms) (GetOpts ps) m idx a, idx ~ idx') => FreeableQM_    ms ps m idx  (Weak idx' a) where freeM_ _ idx     = fmap2 (indexes %~ (idx:)) . nested layered (freeQM (Query :: Query (GetOpts ms) (GetOpts ps)) idx)

--instance ( GrowableQM (M.Ixed ': GetOpts ms) (GetOpts ps) m a, idx ~ Index (Container a)) => GrowableQM_ ms ps m (Reusable idx a) where
--    growM_ _ i (Reusable ixs a) = do Res (ixs',ds) r <- growQM (Query :: Query (M.Ixed ': GetOpts ms) (GetOpts ps)) i a
--                                     return $ Res ds $ Reusable (ixs <> ixs') r


------------------------
-- === Instances === ---
------------------------

-- === Finite ===

-- [+] Measurable
-- [+] MinBounded
-- [+] MaxBounded

--type instance ParamsOf MeasurableOp (Weak f a) = ParamsOf MeasurableOp (Container a)
--type instance ModsOf   MeasurableOp (Weak f a) = ModsOf   MeasurableOp (Container a)

--type instance ParamsOf MinBoundedOp (Weak f a) = ParamsOf MinBoundedOp (Container a)
--type instance ModsOf   MinBoundedOp (Weak f a) = ModsOf   MinBoundedOp (Container a)

--type instance ParamsOf MaxBoundedOp (Weak f a) = ParamsOf MaxBoundedOp (Container a)
--type instance ModsOf   MaxBoundedOp (Weak f a) = ModsOf   MaxBoundedOp (Container a)

--instance (MeasurableQM (GetOpts ms) (GetOpts ps) m     a)             => MeasurableQM_ ms ps m     (Weak idx  a) where sizeM_     _ = sizeQM     (Query :: Query (GetOpts ms) (GetOpts ps)) . unlayer
--instance (MinBoundedQM (GetOpts ms) (GetOpts ps) m idx a, idx ~ idx') => MinBoundedQM_ ms ps m idx (Weak idx' a) where minBoundM_ _ = minBoundQM (Query :: Query (GetOpts ms) (GetOpts ps)) . unlayer
--instance (MaxBoundedQM (GetOpts ms) (GetOpts ps) m idx a, idx ~ idx') => MaxBoundedQM_ ms ps m idx (Weak idx' a) where maxBoundM_ _ = maxBoundQM (Query :: Query (GetOpts ms) (GetOpts ps)) . unlayer




------ === Finite ===

------ [+] Measurable
------ [+] MinBounded
------ [+] MaxBounded


--type instance ModsOf MeasurableQSM (Weak c a) = ModsOf MeasurableQSM (WeakData c a)
----type instance ModsOf MinIndexedQSM (Weak c a) = ModsOf MinIndexedQSM a
----type instance ModsOf MaxIndexedQSM (Weak c a) = ModsOf MaxIndexedQSM a

--instance MeasurableQM q m (WeakData c a) => MeasurableQSM (Weak c a) m q s where sizeQSM     _ _ = queried (Proxy :: Proxy q) sizeM' . unlayer
----instance MinIndexedQM q m a => MinIndexedQSM (Weak c a) m q s where minIndexQSM _ _ = queried (Proxy :: Proxy q) minIndexM' . unwrap
----instance MaxIndexedQM q m a => MaxIndexedQSM (Weak c a) m q s where maxIndexQSM _ _ = queried (Proxy :: Proxy q) maxIndexM' . unwrap

--weaked = lens (\(Weak mf a) -> a) (\(Weak mf _) a -> Weak mf a)

--nestedWeaked = nested weaked

------ === Construction ===

------ [+] Singleton
------ [ ] Allocable
------ [+] Expandable
------ [+] Growable

--type instance ModsOf SingletonQSM  (Weak c a) = ModsOf SingletonQSM  (WeakData c a)
--type instance ModsOf AllocableQSM  (Weak c a) = ModsOf AllocableQSM  (WeakData c a)
--type instance ModsOf ExpandableQSM (Weak c a) = ModsOf ExpandableQSM (WeakData c a)
--type instance ModsOf GrowableQSM   (Weak c a) = ModsOf GrowableQSM   (WeakData c a)

--instance (MonadIO m, SingletonQM (Mem.Weak el) q m t, t ~ WeakData c a, cls ~ SingletonQSM,
--    EqInfoQueries (SingletonInfo el (DataStore t)) (SingletonInfo (Mem.Weak el) (DataStore t)) (Mods.FilterMutable q) ) => SingletonQSM el (Weak c a) m q s where
--    singletonQSM _ _ el = do
--        ptr <- liftIO $ Mem.mkWeakPtr el Nothing
--        (fmap . fmap) (Weak Nothing) $ queried (Proxy :: Proxy q) singletonM' ptr

--instance (AllocableQM  q m (WeakData c a)) => AllocableQSM  (Weak c a) m q s where allocQSM  _ _  = (fmap . fmap) (Weak Nothing) . queried (Proxy :: Proxy q) allocM'
--instance (ExpandableQM q m (WeakData c a)) => ExpandableQSM (Weak c a) m q s where expandQSM _ _  = nestedWeaked  $ queried (Proxy :: Proxy q) expandM'
--instance (GrowableQM   q m (WeakData c a)) => GrowableQSM   (Weak c a) m q s where growQSM   _ _  = nestedWeaked  . queried (Proxy :: Proxy q) growM'



------ === Modification ===

------ [+] Appendable
------ [ ] Prependable
------ [ ] Addable
------ [ ] Removable
------ [+] Insertable

--type instance ModsOf AppendableQSM (Weak c a) = ModsOf AppendableQSM (WeakData c a)
--type instance ModsOf FreeableQSM   (Weak c a) = ModsOf FreeableQSM   (WeakData c a)
--type instance ModsOf InsertableQSM (Weak c a) = ModsOf InsertableQSM (WeakData c a)


--instance (MonadIO m, MonadFix m, AppendableQM wel (Mods.Ixed ': q) m t
--         , t    ~ WeakData c a
--         , wel  ~ Mem.Weak el
--         , ds   ~ DataStore t
--         , info ~ AppendableInfo wel ds
--         , idx  ~ ModData Mods.Ixed info
--         , idx  ~ HomoIndex c
--         , EqInfoQueries (AppendableInfo el ds) info (Mods.FilterMutable q)
--         ) => AppendableQSM el (Weak c a) m q s where
--    appendQSM _ _ el (Weak f c) = mdo
--        ptr <- liftIO $ Mem.mkWeakPtr el (($ ixs) <$> f)
--        (ixs, r) <- splitResData <$> nestedWeaked ( (ixed . queried (Proxy :: Proxy q)) appendM' ptr) (Weak f c)
--        return r

--instance (MonadIO m, MonadFix m, InsertableQM idx wel (Mods.Ixed ': q) m t
--         , t    ~ WeakData c a
--         , wel  ~ Mem.Weak el
--         , ds   ~ DataStore t
--         , info ~ InsertableInfo idx wel ds
--         , idx  ~ ModData Mods.Ixed info
--         , idx  ~ HomoIndex c
--         , EqInfoQueries (InsertableInfo idx el ds) info (Mods.FilterMutable q)
--         ) => InsertableQSM idx el (Weak c a) m q s where
--    insertQSM _ _ idx el (Weak f c) = mdo
--        ptr <- liftIO $ Mem.mkWeakPtr el (($ ixs) <$> f)
--        (ixs, r) <- splitResData <$> nestedWeaked ( (ixed . queried (Proxy :: Proxy q)) insertM' idx ptr) (Weak f c)
--        return r

--instance FreeableQM idx q m (WeakData c a) => FreeableQSM idx (Weak c a) m q s where freeQSM   _ _  = nestedWeaked . queried (Proxy :: Proxy q) freeM'


-------- === Indexing ===

------ [+] Indexable
------ [ ] TracksElems
------ [ ] TracksIxes
------ [+] TracksFreeIxes
------ [ ] TracksUsedIxes


----type instance ModsOf IndexableQSM      (Weak c a) = ModsOf IndexableQSM a
----type instance ModsOf TracksFreeIxesQSM (Weak c a) = '[]
--type instance ModsOf TracksIxesQSM  (Weak c a) = ModsOf TracksIxesQSM  (WeakData c a)
--type instance ModsOf TracksElemsQSM (Weak c a) = ModsOf TracksElemsQSM (WeakData c a)

--instance ( t    ~ WeakData c a
--         , ds   ~ DataStore t
--         , wel  ~ Mem.Weak el
--         , info ~ TracksElemsInfo wel ds
--         , TracksElemsQM (Mem.Weak el) q m (WeakData c a)
--         , EqInfoQueries (TracksElemsInfo el ds) info (Mods.FilterMutable q)
--         ) => TracksElemsQSM   el (Weak c a) m q s where elemsQSM _ _   = (fmap . fmap) (catMaybes . fmap (unsafePerformIO . Mem.deRefWeak)) . queried (Proxy :: Proxy q) elemsM' . unlayer


--instance   TracksIxesQM  idx q m (WeakData c a) => TracksIxesQSM  idx (Weak c a) m q s where ixesQSM  _ _     = queried (Proxy :: Proxy q) ixesM'      . unlayer


----type instance ModsOf TracksElemsQSM (HWeak l a) = ModsOf TracksElemsQSM a

----instance   IndexableQM   idx el q m a => IndexableQSM   idx el (HResizable l a) m q s where indexQSM _ _ idx = queried (Proxy :: Proxy q) indexM' idx . unlayer
----instance   TracksIxesQM  idx    q m a => TracksIxesQSM  idx    (HResizable l a) m q s where ixesQSM  _ _     = queried (Proxy :: Proxy q) ixesM'      . unlayer
