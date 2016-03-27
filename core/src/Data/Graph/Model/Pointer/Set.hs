{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Model.Pointer.Set where

import Prologue

import Data.Construction
import Data.Graph.Model.Cluster
import Data.Graph.Model.Pointer

import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet


-- === Definitions === --

newtype Set    t (a :: Knowledge *) = Set IntSet deriving (Generic, NFData, Show)
type    LocSet t   = Set t 'Unknown
type    RefSet t a = Set t ('Known a)
makeWrapped ''Set

-- === Utils === --

add :: Ptr t a -> Set t a -> Set t a
add ptr = wrapped %~ IntSet.insert (unwrap ptr)

remove :: Ptr t a -> Set t a -> Set t a
remove ptr = wrapped %~ IntSet.delete (unwrap ptr)

member :: Ptr t a -> Set t a -> Bool
member ptr = IntSet.member (unwrap ptr) ∘ unwrap

elems :: Set t a -> [Ptr t a]
elems = wrap <∘> IntSet.toList ∘ unwrap

-- === Instances === --

-- Cast

instance Castable (Set t e) (Set t e') where cast = rewrap ; {-# INLINE cast #-}

-- RefContainer

instance (Monad m, t ~ t', e ~ e')
      => RefContainer (Set t e) (Ptr t' e') m where
    includeRef = return ∘∘ add
    excludeRef = return ∘∘ remove
    toRefList  = return ∘  elems

-- Creator

instance Monad m => Creator m (Set t e) where
    create = return . Set $ mempty

