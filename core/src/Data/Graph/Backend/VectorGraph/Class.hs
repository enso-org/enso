{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}

module Data.Graph.Backend.VectorGraph.Class where

import Prologue                 hiding (Getter, Setter)

import qualified Data.Container as Cont
import           Data.Container (Tup2RTup, IsContainerM, HasContainerM, ComponentStore, HasComponentStore, componentStore, fromContainerM, setContainerM, viewContainerM, ixed, unchecked, inplace)


import Data.Prop
import Data.Container.Auto      (Auto)
import Data.Container.Resizable (Exponential)
import Data.Index
import Data.Vector              (Vector)

import Data.Graph
import Data.Graph.Backend.VectorGraph.SubGraph ()


----------------------------
-- === Data container === --
----------------------------

newtype AutoVector a = AutoVector (Auto Exponential (Vector a)) deriving (Generic, Show, Default)


-- === Instances === --

-- Normal Form
instance (NFData a, Tup2RTup a ~ (a, ())) => NFData (AutoVector a)

-- Wrappers
makeWrapped ''AutoVector
type instance Unlayered (AutoVector a) = Unwrapped (AutoVector a)
instance      Layered   (AutoVector a)

-- List conversions
type instance Item (AutoVector a) = Item (Unwrapped (AutoVector a))
deriving instance ToList   (AutoVector a)
deriving instance FromList (AutoVector a)

-- Containers
type instance Container (AutoVector a) = Container (Unwrapped (AutoVector a))
instance Monad m => HasContainerM m (AutoVector a) where
    viewContainerM = viewContainerM . unwrap ; {-# INLINE viewContainerM #-}
    setContainerM  = wrapped . setContainerM ; {-# INLINE setContainerM  #-}

instance Monad m => IsContainerM  m (AutoVector a) where
    fromContainerM = fmap AutoVector . fromContainerM ; {-# INLINE fromContainerM #-}



-------------------------
-- === VectorGraph === --
-------------------------


data VectorGraph node edge cluster = VectorGraph { _nodeGraph :: !(AutoVector node)
                                                 , _edgeGraph :: !(AutoVector edge)
                                                 , _subGraphs :: !(AutoVector cluster)
                                                 } deriving (Generic, Show)
makeLenses  ''VectorGraph


-- === Instances === --

-- Normal Form
-- The `Tup2RTup e ~ (e, ())` constrains are needed by current container implementation and can be removed in GHC > 8.0 by introducing injective TFs
instance (NFData n, NFData e, NFData c, Tup2RTup n ~ (n, ()), Tup2RTup e ~ (e, ()), Tup2RTup c ~ (c, ())) => NFData (VectorGraph n e c)

-- Data Store

type instance ComponentStore t (VectorGraph n e c) = AutoVector (VectorGraph n e c # t)
instance HasComponentStore Node    (VectorGraph n e c) where componentStore _ = nodeGraph ; {-# INLINE componentStore #-}
instance HasComponentStore Edge    (VectorGraph n e c) where componentStore _ = edgeGraph ; {-# INLINE componentStore #-}
instance HasComponentStore Cluster (VectorGraph n e c) where componentStore _ = subGraphs ; {-# INLINE componentStore #-}

-- Construction

type instance Prop Node    (VectorGraph n e c) = n
type instance Prop Edge    (VectorGraph n e c) = e
type instance Prop Cluster (VectorGraph n e c) = c
instance Default (VectorGraph n e c) where def = VectorGraph (Cont.alloc 100) (Cont.alloc 100) (Cont.alloc 100) ; {-# INLINE def #-}

-- Dynamic implementation

instance (g ~ VectorGraph n e c, HasComponentStore t g, a ~ (g # t)) => Dynamic  t (VectorGraph n e c) a
instance (g ~ VectorGraph n e c, HasComponentStore t g)              => Dynamic' t (VectorGraph n e c) where
    add'    el  = componentStore (p :: P t) $ swap ∘ fmap Ref ∘ ixed Cont.add el ; {-# INLINE add'    #-}
    remove' ref = componentStore (p :: P t) %~ Cont.free (ref ^. idx)            ; {-# INLINE remove' #-}

-- References handling

instance (g ~ (VectorGraph n e c), r ~ (g # t), HasComponentStore t g) => Referred t (VectorGraph n e c) r where
    focus r = lens getter setter where
        getter g     = Cont.index_ (r ^. idx) $ g ^. componentStore (p :: P t)                          ; {-# INLINE getter #-}
        setter g val = g & (componentStore (p :: P t)) %~ unchecked inplace Cont.insert_ (r ^. idx) val ; {-# INLINE setter #-}
    {-# INLINE focus #-}
