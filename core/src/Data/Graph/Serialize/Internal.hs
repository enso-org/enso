{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Serialize.Internal where

import Prologue

import qualified Data.Graph.Data.Component.Class as Component
import qualified Data.Graph.Data.Component.List  as Component
import qualified Data.Graph.Storable.External    as External
import qualified Data.Graph.Traversal.Partition  as Partition
import qualified Data.TypeMap.Strict             as TypeMap
import qualified Foreign.Info.ByteSize           as ByteSize

import Data.Graph.Data.Component.Class (Component)


---------------------------
-- === SubGraph size === --
---------------------------

-- === Size === --

data Size = Size
    { _static  :: !Int
    , _dynamic :: !Int
    }
makeLenses ''Size

instance Default Size where def = Size 0 0  ; {-# INLINE def #-}

addSizes :: Size -> Size -> Size
addSizes = \(Size !s1 !d1) (Size !s2 !d2) ->
        let s = s1 + s2
            d = d1 + d2
        in Size s d
{-# INLINE addSizes #-}

addSizesM :: Applicative m => m Size -> m Size -> m Size
addSizesM = liftA2 addSizes
{-# INLINE addSizesM #-}


-- === Helpers === --

type ComponentSize comp m =
    ( External.ComponentSize comp    m
    , ByteSize.Known (Component comp) m
    )

compSizeFull :: ∀ comp m. ComponentSize comp m
             => Component.Some comp -> m Size
compSizeFull = \comp -> Size <$> ByteSize.get @(Component comp)
                             <*> External.componentSize comp
{-# INLINE compSizeFull #-}

addCompSize :: ∀ comp m. ComponentSize comp m
            => Size -> Component.Some comp -> m Size
addCompSize = \acc comp -> addSizesM (pure acc) $! compSizeFull comp
{-# INLINE addCompSize #-}

componentListSize :: ∀ comp m. ComponentSize comp m
                  => Component.List comp -> m Size
componentListSize = \compList -> do
    let compList' = toList compList
    foldM addCompSize def compList'
{-# INLINE componentListSize #-}


-- === Cluster size discovery === --

-- | Class used to calculate the global size of all the components in the typemap
class ClusterSizeDiscovery' (cs :: [Type]) comps m where
    componentsSize :: Partition.Clusters comps -> m Size -> m Size

instance Applicative m => ClusterSizeDiscovery' '[] ts m where
    componentsSize = \_ -> id   ; {-# INLINE componentsSize #-}

instance
    ( TypeMap.ElemGetter (Component.List comp) (Component.Lists comps)
    , ByteSize.Known (Component comp) m
    , ComponentSize comp m
    , ClusterSizeDiscovery' cs comps m
    , MonadIO m
    ) => ClusterSizeDiscovery' (comp ': cs) comps m where
    componentsSize clusters accM = do
        let compList = TypeMap.getElem @(Component.List comp) clusters
            acc'     = addSizesM accM $! componentListSize compList
        componentsSize @cs @comps clusters acc'
    {-# INLINE componentsSize #-}


-- === API === --

type ClusterSizeDiscovery comps m = ClusterSizeDiscovery' comps comps m

clusterByteSize :: ∀ comps m. (ClusterSizeDiscovery comps m, MonadIO m)
     => Partition.Clusters comps -> m Size
clusterByteSize clusters = componentsSize @comps @comps clusters $! pure def
{-# INLINE clusterByteSize #-}
