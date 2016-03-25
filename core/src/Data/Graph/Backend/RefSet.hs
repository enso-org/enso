{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Backend.RefSet where

import Prologue

import Data.Construction
import Data.Graph.Model.Cluster
import Data.Graph.Model.Ref

import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet


-- === Definitions === --

newtype RefSet t e = RefSet IntSet deriving (Generic, NFData, Show)
makeWrapped ''RefSet

-- === Utils === --

add :: Int -> RefSet t e -> RefSet t e
add el = wrapped %~ IntSet.insert el

remove :: Int -> RefSet t e -> RefSet t e
remove el = wrapped %~ IntSet.delete el

member :: Int -> RefSet t e -> Bool
member el = IntSet.member el ∘ unwrap

elems :: RefSet t e -> [Int]
elems = IntSet.toList . unwrap

instance Monad m => Creator m (RefSet t e) where
    create = return . RefSet $ mempty

-- === Instances === --

-- Cast

instance Castable e e' => Castable (RefSet t e) (RefSet t e') where cast (RefSet s) = RefSet s

-- RefContainer

instance Monad m => RefContainer (RefSet t e) (Ref t e) m where
    includeRef ref = return . add (unwrap ref)
    excludeRef ref = return . remove (unwrap ref)
    toRefList      = return . fmap Ref . elems
