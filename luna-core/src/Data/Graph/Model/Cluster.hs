module Data.Graph.Model.Cluster where

import Prologue

import Data.Prop


-- === Abstraction === --

data Cluster = Cluster deriving (Show, Eq, Ord)


class Clustered t where
    clusters :: Lens' t [t # Cluster]


---- === Utils === --

--add :: Int -> Cluster -> Cluster
--add el = wrapped %~ IntSet.insert el

--remove :: Int -> Cluster -> Cluster
--remove el = wrapped %~ IntSet.delete el

--member :: Int -> Cluster -> Bool
--member el = IntSet.member el ∘ unwrap'


---- === Instances === --

---- Wrappers
--makeWrapped ''Cluster


---- Properties

----instance Getter (Ref Cluster) (VectorGraph n e) where getter ref     = index_ (ref ^. idx) ∘ view clusterGraph                    ; {-# INLINE getter #-}
----instance Setter (Ref Cluster) (VectorGraph n e) where setter ref val = clusterGraph %~ unchecked inplace insert_ (ref ^. idx) val ; {-# INLINE setter #-}
