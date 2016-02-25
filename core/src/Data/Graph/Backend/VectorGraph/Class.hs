{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}

module Data.Graph.Backend.VectorGraph.Class where

import Prologue                 hiding (Getter, Setter)

import Data.Prop
import Data.Container           hiding (impossible)
import Data.Container.Auto      (Auto)
import Data.Container.Resizable (Exponential)
import Data.Index
import Data.Vector              (Vector)


import Data.Graph
import Data.Graph.Backend.VectorGraph.SubGraph


----------------------------
-- === Data container === --
----------------------------

newtype AutoVector a = AutoVector (Auto Exponential (Vector a)) deriving (Show, Default)


-- === Instances === --

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
                                                 } deriving (Show)


makeLenses  ''VectorGraph


-- === Instances === --

-- Construction

type instance Prop Node    (VectorGraph n e c) = n
type instance Prop Edge    (VectorGraph n e c) = e
type instance Prop Cluster (VectorGraph n e c) = c
instance Default (VectorGraph n e c) where def = VectorGraph (alloc 100) (alloc 100) (alloc 100)

-- References handling

instance r ~ n => Referred Node r (VectorGraph n e c) where
    focus r = lens getter setter where
        getter t     = index_ (r ^. idx) $ t ^. nodeGraph                        ; {-# INLINE getter #-}
        setter t val = t & nodeGraph %~ unchecked inplace insert_ (r ^. idx) val ; {-# INLINE setter #-}
    {-# INLINE focus #-}

instance r ~ e => Referred Edge r (VectorGraph n e c) where
    focus r = lens getter setter where
        getter t     = index_ (r ^. idx) $ t ^. edgeGraph                        ; {-# INLINE getter #-}
        setter t val = t & edgeGraph %~ unchecked inplace insert_ (r ^. idx) val ; {-# INLINE setter #-}
    {-# INLINE focus #-}

instance r ~ c => Referred Cluster r (VectorGraph n e c) where
    focus r = lens getter setter where
        getter t     = index_ (r ^. idx) $ t ^. subGraphs                        ; {-# INLINE getter #-}
        setter t val = t & subGraphs %~ unchecked inplace insert_ (r ^. idx) val ; {-# INLINE setter #-}
    {-# INLINE focus #-}
