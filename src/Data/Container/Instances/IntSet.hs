module Data.Container.Instances.IntSet () where

import Prelude

import Data.Container.Class
import Data.Container.List
import Data.Container.Opts
import Data.Container.Proxy
import Data.Container.Utils

import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

------------------------------
-- === Global instances === --
------------------------------

type instance Item      IntSet = Int
type instance Container IntSet = IntSet
type instance DataStore IntSet = IntSet

instance Monad m => IsContainerM  m IntSet where
    fromContainerM = return         ; {-# INLINE fromContainerM #-}
instance Monad m => HasContainerM m IntSet where
    viewContainerM = return         ; {-# INLINE viewContainerM #-}
    setContainerM  = const . return ; {-# INLINE setContainerM  #-}

instance ToList   IntSet where toList   = IntSet.toList   ; {-# INLINE toList   #-}
instance FromList IntSet where fromList = IntSet.fromList ; {-# INLINE fromList #-}

----------------------------------
-- === Operations instances === --
----------------------------------

-- === Finite ===

-- [+] Measurable
-- [-] MinBounded
-- [-] MaxBounded

type instance ParamsOf MeasurableOp IntSet = '[]
type instance ModsOf   MeasurableOp IntSet = '[]

instance Monad m => MeasurableQM_ '[] '[] m IntSet where
    sizeM_ _ = return . Res () . IntSet.size ; {-# INLINE sizeM_ #-}

-- === Construction ===

-- [+] Singleton
-- [-] Allocable
-- [-] Expandable
-- [-] Growable

type instance ParamsOf SingletonOp IntSet = '[]
type instance ModsOf   SingletonOp IntSet = '[]

instance (Monad m, a ~ Int) => SingletonQM_ '[] '[] m a IntSet where
    singletonM_ _ = return . Res () . IntSet.singleton ; {-# INLINE singletonM_ #-}

-- === Modification ===

-- [-] Appendable
-- [-] Prependable
-- [+] Addable
-- [+] Removable
-- [-] Insertable
-- [-] Freeable

type instance ParamsOf AddableOp   IntSet = '[]
type instance ModsOf   AddableOp   IntSet = '[]

type instance ParamsOf RemovableOp IntSet = '[Try]
type instance ModsOf   RemovableOp IntSet = '[]

instance (Monad m, a ~ Int) => AddableQM_   '[] '[] m a IntSet where
    addM_ _    = return . Res () .: IntSet.insert ; {-# INLINE addM_    #-}

instance (Monad m, a ~ Int) => RemovableQM_ '[] '[P Try] m a IntSet where
    removeM_ _ key s = if IntSet.member key s
        then return . Res () $ IntSet.delete key s
        else fail "Element not found"
    {-# INLINE removeM_ #-}

-- === Indexing ===

-- [-] Indexable
-- [-] TracksFreeIxes
-- [-] TracksUsedIxes
-- [-] TracksIxes
-- [+] TracksElems

type instance ParamsOf TracksElemsOp IntSet = '[]
type instance ModsOf   TracksElemsOp IntSet = '[]

instance (Monad m, a ~ Int) => TracksElemsQM_ '[] '[] m a IntSet where
    elemsM_ _ = return . Res () . toList ; {-# INLINE elemsM_ #-}
