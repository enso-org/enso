{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Builder.SubGraph where

import Prelude.Luna

import Control.Monad.Event
import Data.Graph
import Data.Index
import Data.Construction
import Data.Graph.Backend.VectorGraph
import Data.Graph.Model.Events
import Data.Graph.Builder.Ref
import Data.Graph.Builder.Class

import qualified Data.Graph.Backend.VectorGraph.SubGraph as SubGraph

subgraph' :: ( CoverConstructor m c
             , Covered c
             , Uncovered c ~ SubGraph n
             ) => m c
subgraph' = constructCover $ SubGraph mempty

subgraph :: ( CoverConstructor m c
            , Constructor m (Ref Cluster c)
            , Covered c
            , Uncovered c ~ SubGraph n
            ) => m (Ref Cluster c)
subgraph = constructLayer =<< subgraph'

includes :: ( MonadBuilder t m
            , Referred Cluster c t
            , Covered c
            , Uncovered c ~ SubGraph a'
            , BiCastable a a'
            ) => Ref Cluster c -> Ref Node a -> m Bool
includes cluster el = SubGraph.member (el ^. idx) . uncover <$> read cluster

members :: ( MonadBuilder t m
           , Referred Cluster c t
           , Covered c
           , Uncovered c ~ SubGraph n
           ) => Ref Cluster c -> m [Ref Node n]
members cluster = fmap Ref <$> SubGraph.nodes . uncover <$> read cluster

class Clusterable n c m where
    include :: Ref Node n -> Ref Cluster c -> m ()
    exclude :: Ref Node n -> Ref Cluster c -> m ()

{-instance Clusterable I c m where-}
    {-include = impossible-}
    {-exclude = impossible-}

{-instance Clusterable n I m where-}
    {-include = impossible-}
    {-exclude = impossible-}

{-instance Clusterable n c IM where-}
    {-include = impossible-}
    {-exclude = impossible-}

instance ( MonadBuilder t m
         , Referred Cluster c t
         , Covered c
         , Uncovered c ~ SubGraph n'
         , BiCastable n n'
         , Dispatcher SUBGRAPH_INCLUDE (SubgraphNodeEvent n c) m
         ) => Clusterable n c m where
    include el cluster = do
        withRef cluster $ covered %~ SubGraph.add (el ^. idx)
        dispatch SUBGRAPH_INCLUDE $ SubgraphNodeEvent el cluster
        return ()

    exclude el cluster = do
        withRef cluster $ covered %~ SubGraph.remove (el ^. idx)
        {-dispatch SUBGRAPH_EXCLUDE $ SubgraphNodeEvent el cluster-}
        return ()
