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

data Unknown = Unknown -- FIXME[WD]: For greater flexibility we should implement this as data-kind - Known a / Unknown a

-- === Definitions === --

newtype Ref r a = Ref Int deriving (Generic, NFData, Show, Eq, Ord)
type    Ptr r   = Ref r Unknown

class Referred r t a where focus :: Ref r a -> Lens' t a
type  Referred' r t = Referred r t (t # r)

-- === Utils === --

focus' :: Referred' r t => Ref r (t # r) -> Lens' t (t # r)
focus' = focus

retarget :: Ref r a -> Ref r a'
retarget = rewrap


-- === Instances === --

-- Wrappers
makeWrapped ''Ref

-- Ref primitive instances
type instance Uncovered     (Ref r a) = Uncovered a
type instance Unlayered     (Ref r a) = a
type instance Deconstructed (Ref r a) = a

-- Index
type instance Index  (Ref r a) = Int
instance      HasIdx (Ref r a) where idx = wrapped' ; {-# INLINE idx #-}

-- Conversions
instance {-# OVERLAPPABLE #-}                  Castable (Ref r a)       (Ref r a)        where cast = id     ; {-# INLINE cast #-}
instance {-# OVERLAPPABLE #-} Castable a a' => Castable (Ref r a)       (Ref r' a')      where cast = rewrap ; {-# INLINE cast #-}
instance {-# OVERLAPPABLE #-}                  Castable (Ref r a)       (Ref r' Unknown) where cast = rewrap ; {-# INLINE cast #-}
instance {-# OVERLAPPABLE #-}                  Castable (Ref r Unknown) (Ref r' a')      where cast = rewrap ; {-# INLINE cast #-}
instance {-# OVERLAPPABLE #-}                  Castable (Ref r Unknown) (Ref r' Unknown) where cast = rewrap ; {-# INLINE cast #-}

