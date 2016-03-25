{-# LANGUAGE UndecidableInstances #-}

-- TODO[WD]: refaktor and rename this module / datas to reflect that in fact it is just a set of elems used as SubGraph definition
module Data.Graph.Backend.SubGraph where

import Prologue

import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet


-- === Definitions === --

newtype SubGraph n = SubGraph IntSet deriving (Generic, NFData, Show)
makeWrapped ''SubGraph

-- === Utils === --

add :: Int -> SubGraph n -> SubGraph n
add el = wrapped %~ IntSet.insert el

remove :: Int -> SubGraph n -> SubGraph n
remove el = wrapped %~ IntSet.delete el

member :: Int -> SubGraph n -> Bool
member el = IntSet.member el ∘ unwrap

elems :: SubGraph n -> [Int]
elems = IntSet.toList . unwrap


-- === Instances === --

-- Cast

instance Castable n n' => Castable (SubGraph n) (SubGraph n') where cast (SubGraph s) = SubGraph s

---- Wrappers
--makeWrapped ''Cluster


---- Properties

----instance Getter (Ref Cluster) (VectorGraph n e) where getter ref     = index_ (ref ^. idx) ∘ view clusterGraph                    ; {-# INLINE getter #-}
----instance Setter (Ref Cluster) (VectorGraph n e) where setter ref val = clusterGraph %~ unchecked inplace insert_ (ref ^. idx) val ; {-# INLINE setter #-}

