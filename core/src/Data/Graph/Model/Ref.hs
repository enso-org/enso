{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Model.Ref (module Data.Graph.Model.Ref, module X) where

import Prelude.Luna

import Data.Construction
import Data.Direction    as X
import Data.Index
import Data.Layer
import Data.Prop

-----------------
-- === Ref === --
-----------------

-- === Definitions === --

-- FIXME[WD]: Maybe we should parametrize the Ref to indicate the ref type, like Ref Node / Ref Edge / Ref Cluster / ...
--            We can then introduce Ref and TypedRef (used with homo- and hetero- graphs)
newtype Ref r a = Ref Int deriving (Show, Eq, Ord, Functor, Traversable, Foldable)
newtype Ptr r   = Ptr Int deriving (Show, Eq, Ord)

class Referred r a t where focus :: Ref r a -> Lens' t a


-- === Utils === --

retarget :: Ref r a -> Ref r a'
retarget = rewrap


-- === Instances === --

-- Wrappers
makeWrapped ''Ref
makeWrapped ''Ptr

-- Ref primitive instances
type instance Uncovered     (Ref r a) = Uncovered a
type instance Unlayered     (Ref r a) = a
type instance Deconstructed (Ref r a) = a

-- Index
type instance Index  (Ref r a) = Int
instance      HasIdx (Ref r a) where idx = wrapped' ; {-# INLINE idx #-}
type instance Index  (Ptr r)   = Int
instance      HasIdx (Ptr r)   where idx = wrapped' ; {-# INLINE idx #-}

-- Conversions
instance Castable a a' => Castable (Ref r a) (Ref r' a') where cast = rewrap ; {-# INLINE cast #-}
instance Castable (Ref r a) (Ptr r)   where cast (Ref x) = Ptr x ; {-# INLINE cast #-}
instance Castable (Ptr r)   (Ref r a) where cast (Ptr x) = Ref x ; {-# INLINE cast #-}

-- Construction
instance Constructor m (Ref r a) => LayerConstructor m (Ref r a) where
    constructLayer = construct ; {-# INLINE constructLayer #-}



