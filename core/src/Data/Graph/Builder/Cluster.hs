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
        dispatch SUBGRAPH_INCLUDE $ SubgraphElemEvent el cluster
        return ()

    exclude el cluster = withM cluster $ excludeRef el

    members cluster = read cluster >>= toRefList

subgraph :: (Creator m c, Constructor m (Ref Cluster c)) => m (Ref Cluster c)
subgraph = constructLayer =<< create
