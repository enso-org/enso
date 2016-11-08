{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Data.Graph.Backend.NEC where

import Prologue                 hiding (Getter, Setter, Setter', set')
import Prologue.Unsafe

import qualified Data.Container as Cont
import           Data.Container (Store, HasStore, HasStores, store, unchecked, inplace, ixed)
import           Data.AutoVector as AutoVector

import Old.Data.Prop                hiding (Getter, Setter)
import Data.Container.Auto      (Auto)
import Data.Container.Resizable (Exponential)
import Data.Index
import Data.Vector              (Vector)
import Data.Vector.Mutable      (MVector)

import Data.Graph
import Control.Monad.Primitive  (PrimMonad, PrimState)
import Data.Graph.Model.Pointer.Set ()

import           Data.RTuple (TMap(..), empty, Assoc(..), Assocs, (:=:), MapVals, Cycle(..), List((:-:), Null))
import qualified Data.RTuple as List
import           Data.Container.Hetero (Hetero2(..), Any)
import           Control.Lens.Property
import           Unsafe.Coerce (unsafeCoerce)

-----------------
-- === NEC === --
-----------------

-- === Definition === --

type NEC' cont v e c = NEC (cont v) (cont e) (cont c)
data NEC nodes edges clusters = NEC { __nodeStore_    :: !nodes
                                    , __edgeStore_    :: !edges
                                    , __clusterStore_ :: !clusters
                                    } deriving (Generic, Show)
makeLenses  ''NEC


-- === Utils === --

nec :: HasStores '[Node, Edge, Cluster] a => Lens' a (NEC (Store Node a) (Store Edge a) (Store Cluster a))
nec = lens (\a -> NEC (a ^. nodeStore) (a ^. edgeStore) (a ^. clusterStore)) (\a (NEC n e c) -> a & nodeStore .~ n & edgeStore .~ e & clusterStore .~ c)


-- === Instances === --

-- Primitive

instance Defaults '[n,e,c] => Default (NEC n e c) where def                                = NEC def def def                   ; {-# INLINE def     #-}
instance Monoids  '[n,e,c] => Monoid  (NEC n e c) where mempty                             = NEC mempty mempty mempty          ; {-# INLINE mempty  #-}
                                                        mappend (NEC n e c) (NEC n' e' c') = NEC (n <> n') (e <> e') (c <> c') ; {-# INLINE mappend #-}

-- Store

type instance Store Node    (NEC n e c) = n
type instance Store Edge    (NEC n e c) = e
type instance Store Cluster (NEC n e c) = c

instance HasStore Node    (NEC n e c) where store _ = _nodeStore_
instance HasStore Edge    (NEC n e c) where store _ = _edgeStore_
instance HasStore Cluster (NEC n e c) where store _ = _clusterStore_

-- Normal Form

instance NFDatas '[v,e,c] => NFData (NEC v e c)



-------------------
-- === Graph === --
-------------------

newtype Graph    n e c =  Graph (NEC' ( AutoVector  ) n e c) deriving (Generic, Show)
newtype MGraph s n e c = MGraph (NEC' (MAutoVector s) n e c) deriving (Generic)
makeWrapped ''Graph
makeWrapped ''MGraph


-- === Utils === --

-- TODO[WD]: Move to containers as container utility function
unsafeThaw :: PrimMonad m => Graph n e c -> m (MGraph (PrimState m) n e c)
unsafeThaw (unwrap' -> NEC n e c) = wrap' <$> (NEC <$> AutoVector.unsafeThaw n <*> AutoVector.unsafeThaw e <*> AutoVector.unsafeThaw c) ; {-# INLINE unsafeThaw #-}

-- TODO[WD]: Move to containers as container utility function
unsafeFreeze :: PrimMonad m => MGraph (PrimState m) n e c -> m (Graph n e c)
unsafeFreeze (unwrap' -> NEC n e c) = wrap' <$> (NEC <$> AutoVector.unsafeFreeze n <*> AutoVector.unsafeFreeze e <*> AutoVector.unsafeFreeze c) ; {-# INLINE unsafeFreeze #-}

-- TODO[WD]: Move to containers as container utility function
thaw :: PrimMonad m => Graph n e c -> m (MGraph (PrimState m) n e c)
thaw (unwrap' -> NEC n e c) = wrap' <$> (NEC <$> AutoVector.thaw n <*> AutoVector.thaw e <*> AutoVector.thaw c) ; {-# INLINE thaw #-}

-- TODO[WD]: Move to containers as container utility function
freeze :: PrimMonad m => MGraph (PrimState m) n e c -> m (Graph n e c)
freeze (unwrap' -> NEC n e c) = wrap' <$> (NEC <$> AutoVector.freeze n <*> AutoVector.freeze e <*> AutoVector.freeze c) ; {-# INLINE freeze #-}


-- === Instances === --

-- FIXME[WD]: remove the following default instance and derive it. It currently breaks passes because passes somehow require the initial vector not to be empty.
instance Default (Graph n e c) where def = Graph $ NEC (Cont.alloc 100) (Cont.alloc 100) (Cont.alloc 100)

-- Normal Form

instance (NFData (Unwrapped (Graph n e c))) => NFData (Graph n e c)

-- Store

type instance Store t ( Graph   n e c) = AutoVector    ( Graph   n e c # t)
type instance Store t (MGraph s n e c) = MAutoVector s (MGraph s n e c # t)
instance (HasStore t cont, Store t cont ~ Store t (Graph n e c), cont ~ Unwrapped (Graph n e c))
      => HasStore t (Graph n e c) where store p = wrapped' ∘ store p ; {-# INLINE store #-}
instance (HasStore t cont, Store t cont ~ Store t (MGraph s n e c), cont ~ Unwrapped (MGraph s n e c))
      => HasStore t (MGraph s n e c) where store p = wrapped' ∘ store p ; {-# INLINE store #-}

-- Construction

type instance Prop Node    (Graph n e c) = n
type instance Prop Edge    (Graph n e c) = e
type instance Prop Cluster (Graph n e c) = c

type instance Prop Node    (MGraph s n e c) = n
type instance Prop Edge    (MGraph s n e c) = e
type instance Prop Cluster (MGraph s n e c) = c

-- Dynamic implementation

--instance (g ~ Graph n e c, HasStore t g, a ~ (g # t)) => Dynamic  t (Graph n e c) a
--instance (g ~ Graph n e c, HasStore t g)              => Dynamic' t (Graph n e c) where
--    add'    el  = store (p :: P t) $ swap ∘ fmap Ref ∘ ixed Cont.add el ; {-# INLINE add'    #-}
--    remove' ref = store (p :: P t) %~ Cont.free (ref ^. idx)            ; {-# INLINE remove' #-}


--class DynamicM t a m g where
--    addM    :: a -> g -> m (Ref t a, g)
--    removeM :: Ref t a -> g -> m g

instance (g ~ Graph n e c, HasStore t g, Monad m, a ~ (g # t)) => DynamicM  t (Graph n e c) m a
instance (g ~ Graph n e c, HasStore t g, Monad m)              => DynamicM' t (Graph n e c) m where
    addM'    el  = nested (store (p :: P t)) $ (swap ∘ fmap Ptr) <∘> ixed Cont.addM el ; {-# INLINE addM'    #-}
    removeM' ref = store (p :: P t) $ Cont.freeM (ref ^. idx)                          ; {-# INLINE removeM' #-}

instance (g ~ MGraph s n e c, s ~ PrimState m, PrimMonad m, HasStore t g, a ~ (g # t)) => DynamicM  t (MGraph s n e c) m a
instance (g ~ MGraph s n e c, s ~ PrimState m, PrimMonad m, HasStore t g)              => DynamicM' t (MGraph s n e c) m where
    addM'    el  = nested (store (p :: P t)) $ (swap ∘ fmap Ptr) <∘> ixed Cont.addM el ; {-# INLINE addM'    #-}
    removeM' ref = store (p :: P t) $ Cont.freeM (ref ^. idx)                          ; {-# INLINE removeM' #-}

-- References handling

--instance (g ~ Graph n e c, r ~ (g # t), HasStore t g) => Referred t (Graph n e c) r where
--    focus r = lens getter setter where
--        getter g     = Cont.index_ (r ^. idx) $ g ^. store (p :: P t)                          ; {-# INLINE getter #-}
--        setter g val = g & (store (p :: P t)) %~ unchecked inplace Cont.insert_ (r ^. idx) val ; {-# INLINE setter #-}
--    {-# INLINE focus #-}


instance (g ~ Graph n e c, r ~ (g # t), HasStore t g, Monad m) => LocatedM    t (Graph n e c) m
instance (g ~ Graph n e c, r ~ (g # t), HasStore t g, Monad m) => ReferencedM t (Graph n e c) m r where
    writeRefM r v = store (p :: P t) $ unchecked inplace Cont.insertM__ (r ^. idx) v ; {-# INLINE writeRefM #-}
    readRefM  r   = Cont.indexM__ (r ^. idx) ∘ view (store (p :: P t))               ; {-# INLINE readRefM  #-}


instance (g ~ MGraph s n e c, r ~ (g # t), HasStore t g, s ~ PrimState m, PrimMonad m) => LocatedM    t (MGraph s n e c) m
instance (g ~ MGraph s n e c, r ~ (g # t), HasStore t g, s ~ PrimState m, PrimMonad m) => ReferencedM t (MGraph s n e c) m r where
    writeRefM r v = store (p :: P t) $ unchecked inplace Cont.insertM__ (r ^. idx) v ; {-# INLINE writeRefM #-}
    readRefM  r   = Cont.indexM__ (r ^. idx) ∘ view (store (p :: P t))               ; {-# INLINE readRefM  #-}




--------------------
-- === Graphs === --
--------------------

newtype Graph2    rels         = Graph2  (TMap (MapVals AutoVector      rels))
newtype MGraph2 s rels         = MGraph2 (TMap (MapVals (MAutoVector s) rels))
newtype HGraph  (els :: [★])   = HGraph  (TMap (els :=: 'Cycle (Hetero2 AutoVector     )))
newtype HMGraph (els :: [★]) s = HMGraph (TMap (els :=: 'Cycle (Hetero2 (MAutoVector s))))


freeze2 :: PrimMonad m => HMGraph '[Node, Edge, Cluster] (PrimState m) -> m (HGraph '[Node, Edge, Cluster])
freeze2 (HMGraph (TMap (n :-: e :-: c :-: Null))) = do
    n' <- Hetero2 <$> AutoVector.freeze (unwrap' n)
    e' <- Hetero2 <$> AutoVector.freeze (unwrap' e)
    c' <- Hetero2 <$> AutoVector.freeze (unwrap' c)
    return $ HGraph (TMap (n' :-: e' :-: c' :-: Null))

thaw2 :: PrimMonad m => HGraph '[Node, Edge, Cluster] -> m (HMGraph '[Node, Edge, Cluster] (PrimState m))
thaw2 (HGraph (TMap (n :-: e :-: c :-: Null))) = do
    n' <- Hetero2 <$> AutoVector.thaw (unwrap' n)
    e' <- Hetero2 <$> AutoVector.thaw (unwrap' e)
    c' <- Hetero2 <$> AutoVector.thaw (unwrap' c)
    return $ HMGraph (TMap (n' :-: e' :-: c' :-: Null))

unsafeFreeze2 :: PrimMonad m => HMGraph '[Node, Edge, Cluster] (PrimState m) -> m (HGraph '[Node, Edge, Cluster])
unsafeFreeze2 (HMGraph (TMap (n :-: e :-: c :-: Null))) = do
    n' <- Hetero2 <$> AutoVector.unsafeFreeze (unwrap' n)
    e' <- Hetero2 <$> AutoVector.unsafeFreeze (unwrap' e)
    c' <- Hetero2 <$> AutoVector.unsafeFreeze (unwrap' c)
    return $ HGraph (TMap (n' :-: e' :-: c' :-: Null))

unsafeThaw2 :: PrimMonad m => HGraph '[Node, Edge, Cluster] -> m (HMGraph '[Node, Edge, Cluster] (PrimState m))
unsafeThaw2 (HGraph (TMap (n :-: e :-: c :-: Null))) = do
    n' <- Hetero2 <$> AutoVector.unsafeThaw (unwrap' n)
    e' <- Hetero2 <$> AutoVector.unsafeThaw (unwrap' e)
    c' <- Hetero2 <$> AutoVector.unsafeThaw (unwrap' c)
    return $ HMGraph (TMap (n' :-: e' :-: c' :-: Null))


-- === Instances === --

-- Wrapped
-- makeWrapped ''HMGraph -- ghc8 incompatible
instance Wrapped (HMGraph els s) where
    type Unwrapped (HMGraph els s) = TMap (els :=: 'Cycle (Hetero2 (MAutoVector s)))
    _Wrapped' = iso (\(HMGraph g) -> g) HMGraph ; {-# INLINE _Wrapped' #-}

-- Show
deriving instance Show (Unwrapped (HMGraph els s)) => Show (HMGraph els s)

-- Properties
type instance Get t                           (HMGraph els s) = Hetero2 (MAutoVector s)
type instance Set t (Hetero2 (MAutoVector s)) (HMGraph els s) = HMGraph els s

instance (g ~ HMGraph els s, g' ~ Unwrapped g, Get t g ~ Get t g', HasProperty2' t g')
      => Getter t (HMGraph els s) where
    get = get @t . unwrap' ; {-# INLINE get #-}

instance (g ~ HMGraph els s, g' ~ Unwrapped g, Get t g ~ Get t g', HasProperty2' t g, HasProperty2' t g')
      => Setter' t (HMGraph els s) where
    set' a = wrapped' %~ set' @t a ; {-# INLINE set' #-}

emptyHMGraph :: PrimMonad m => m (HMGraph '[Node, Edge, Cluster] (PrimState m))
emptyHMGraph = do
    nn <- Hetero2 <$> AutoVector.unsafeThaw def
    ne <- Hetero2 <$> AutoVector.unsafeThaw def
    nc <- Hetero2 <$> AutoVector.unsafeThaw def
    return . HMGraph
           . TMap
           . List.prepend nc
           . List.prepend ne
           . List.prepend nn
           $ List.empty

emptyHGraph :: HGraph '[Node, Edge, Cluster]
emptyHGraph = HGraph
            . TMap
            . List.prepend def
            . List.prepend def
            . List.prepend def
            $ List.empty

-- class DynamicM t g m a where
--     addM    :: a -> g -> m (Ref t a, g)
--     removeM :: Ref t a -> g -> m g
--
--     default addM    :: (DynamicM' t g m , a ~ (g # t), Functor m) => a -> g -> m (Ref t a, g)
--     default removeM :: (DynamicM' t g m , a ~ (g # t), Functor m) => Ref t a -> g -> m g
--
--     addM a g = addM' a g <&> _1 %~ retarget ; {-# INLINE addM #-}
--     removeM = removeM' ∘ retarget           ; {-# INLINE removeM #-}

-- GHC BUG - the second constraint makes infinit e compilation times in Main
instance {-# OVERLAPPABLE #-}
         (PrimMonad m, g ~ HMGraph rels s, (g ^. t) ~ Hetero2 (MAutoVector s), s ~ PrimState m, HasProperty2' t g)
        --  (PrimMonad m, HasProperty2' t g, g ~ HMGraph rels s, (g ^. t) ~ Hetero2 (MAutoVector s), s ~ PrimState m)
      => DynamicM t (HMGraph rels s) m a where
    addM el = nested (prop' @t . wrapped') $ (swap ∘ fmap Ptr) <∘> ixed Cont.addM (unsafeCoerce el)
    removeM = error "o"


-- GHC BUG - the second constraint makes infinit e compilation times in Main
instance {-# OVERLAPPABLE #-}
         (PrimMonad m, g ~ HMGraph rels s, (g ^. t) ~ Hetero2 (MAutoVector s), s ~ PrimState m, HasProperty2' t g)
      => DynamicM2 t (HMGraph rels s) m a where
    addM2 el = nested (prop' @t . wrapped') $ (swap ∘ fmap fromIntegral) <∘> ixed Cont.addM (unsafeCoerce el)
    removeM2 = error "o"


instance {-# OVERLAPPABLE #-}
         (PrimMonad m, g ~ HMGraph rels s, (g ^. t) ~ Hetero2 (MAutoVector s), s ~ PrimState m, HasProperty2' t g)
      => DynamicM3 t (HMGraph rels s) m where
    addM3 el = nested (prop' @t . wrapped') $ (swap ∘ fmap fromIntegral) <∘> ixed Cont.addM (unsafeCoerce el)
    removeM3 = error "o"




-- instance (g ~ MGraph s n e c, r ~ (g # t), HasStore t g, s ~ PrimState m, PrimMonad m) => LocatedM    t (MGraph s n e c) m
-- instance (g ~ MGraph s n e c, r ~ (g # t), HasStore t g, s ~ PrimState m, PrimMonad m) => ReferencedM t (HMGraph rels s) m r where
instance (s ~ PrimState m, PrimMonad m, g ~ HMGraph rels s, Get t g ~ Hetero2 (MAutoVector s), Getter t g)
      => ReferencedM t (HMGraph rels s) m r where
    writeRefM r v = error "x" -- store (p :: P t) $ unchecked inplace Cont.insertM__ (r ^. idx) v ; {-# INLINE writeRefM #-}
    -- readRefM  r g = Cont.indexM__ (r ^. idx) $ (get @t (g :: g) :: Get t g)              ; {-# INLINE readRefM  #-}
    readRefM  r   = unsafeCoerce <∘> Cont.indexM__ (r ^. idx) ∘ unwrap' ∘ get @t               ; {-# INLINE readRefM  #-}


-- TODO: [WD] it should be refactored together with containers library
instance (s ~ PrimState m, PrimMonad m, g ~ HMGraph rels s, Get t g ~ Hetero2 (MAutoVector s), Getter t g
         , Get t tassocs ~ Hetero2 (MAutoVector s)
         , Set t (Get t tassocs) tassocs ~ tassocs
         , Getter  t tassocs
         , Setter' t tassocs
         , assocs ~ Assocs rels ('Cycle (Hetero2 (MAutoVector s)))
         , tassocs ~ TMap assocs
         ) => ReferableM t (HMGraph rels s) m where
    setRefM  r v g = (prop' @t $ unchecked inplace Cont.insertM__ (r ^. idx) (unsafeCoerce v)) g ; {-# INLINE setRefM   #-}
    viewRefM r     = unsafeCoerce <∘> Cont.indexM__ (r ^. idx) ∘ unwrap' ∘ get @t                 ; {-# INLINE viewRefM  #-}
    viewPtrs       = Ptr2 . Ptr <∘∘> Cont.usedIxesM ∘ get @t                                      ; {-# INLINE viewPtrs  #-}


--
-- instance (g ~ Graph n e c, r ~ (g # t), HasStore t g, Monad m) => ReferencedM t (Graph n e c) m r where
--     writeRefM r v = store (p :: P t) $ unchecked inplace Cont.insertM__ (r ^. idx) v ; {-# INLINE writeRefM #-}
--     readRefM  r   = Cont.indexM__ (r ^. idx) ∘ view (store (p :: P t))               ; {-# INLINE readRefM  #-}


-- class ReferencedM r t m a where
--     writeRefM :: Ref r a -> a -> t -> m t
--     readRefM  :: Ref r a -> t -> m a
--
-- class ReferableM r t m where
--     setRefM  :: Ref2 r a -> a -> t -> m t
--     viewRefM :: Ref2 r a      -> t -> m a
--     viewPtrs :: t -> m [Ptr2 r]
