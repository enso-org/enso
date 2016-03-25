{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Builder.Cluster where

import Prelude.Luna

import Control.Monad.Event
import Data.Graph

import Data.Graph.Builder.Ref
import Data.Graph.Builder.Class (MonadBuilder)

instance ( Dispatcher SUBGRAPH_INCLUDE (SubgraphElemEvent (Ref r e) (Ref Cluster c)) m
         , RefContainer c (Ref r e) m
         , MonadBuilder t m
         , Referred Cluster t c
         ) => Clusterable r e c m where
    include el cluster = do
        withM cluster $ includeRef el
        dispatch_ SUBGRAPH_INCLUDE $ SubgraphElemEvent el cluster
    {-# INLINE include #-}

    exclude el cluster = withM cluster $ excludeRef el ; {-# INLINE exclude #-}

    members cluster = read cluster >>= toRefList ; {-# INLINE members #-}

subgraph :: (Creator m c, Constructor m (Ref Cluster c)) => m (Ref Cluster c)
subgraph = constructLayer =<< create
