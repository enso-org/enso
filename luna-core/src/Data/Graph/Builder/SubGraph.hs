module Data.Graph.Builder.SubGraph where

import Prologue
import Data.Graph
import Data.Index
import Data.Construction
import Data.Graph.Backend.VectorGraph

import qualified Data.Graph.Backend.VectorGraph.SubGraph as SubGraph
import qualified Data.Graph.Builder.Class                as Graph
import qualified Data.Graph.Builder.Ref                  as Ref

subgraph' :: (CoverConstructor m c, Covered c, Uncovered c ~ SubGraph n) => m c
subgraph' = constructCover $ SubGraph mempty

subgraph :: (CoverConstructor m c, Constructor m (Ref Cluster c), Covered c, Uncovered c ~ SubGraph n) => m (Ref Cluster c)
subgraph = constructLayer =<< subgraph'

includes :: (Graph.MonadBuilder t m, Referred Cluster c t, Covered c, Uncovered c ~ SubGraph a', BiCastable a a') => Ref Cluster c -> Ref Node a -> m Bool
includes cluster el = SubGraph.member (el ^. idx) . uncover <$> Ref.read cluster

members :: (Graph.MonadBuilder t m, Referred Cluster c t, Covered c, Uncovered c ~ SubGraph a', BiCastable a a') => Ref Cluster c -> m [Ref Node a]
members cluster = fmap Ref <$> SubGraph.nodes . uncover <$> Ref.read cluster

include :: (Graph.MonadBuilder t m, Referred Cluster c t, Covered c, Uncovered c ~ SubGraph a', BiCastable a a') => Ref Node a -> Ref Cluster c -> m ()
include el cluster = Ref.with cluster $ covered %~ SubGraph.add (el ^. idx)

exclude :: (Graph.MonadBuilder t m, Referred Cluster c t, Covered  c, Uncovered c ~ SubGraph a', BiCastable a a') => Ref Node a -> Ref Cluster c -> m ()
exclude el cluster = Ref.with cluster $ covered %~ SubGraph.remove (el ^. idx)
