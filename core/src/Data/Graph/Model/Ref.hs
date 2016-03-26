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

--data Knowledge a = Known a
--                 | Unknown
--                 deriving (Show, Functor, Foldable, Traversable)

--newtype Ptr r (tgt :: Knowledge *) = Ptr Int deriving (Generic, NFData, Show, Eq, Ord)
--type    Loc r   = Ptr r 'Unknown
--type    Ref r a = Ptr r ('Known a)


--class ReferencedM r t m a where
--    referenceM   :: Ref r a -> a -> t -> m t
--    dereferenceM :: Ref r a -> t -> m a

--class LocatedM r t m where
--    placeM  :: Loc r -> t # r -> t -> m t
--    locateM :: Loc r -> t -> m (t # r)

--class PointedM r tgt t m a where
--    pointM  :: Ptr r tgt -> a -> t -> m t
--    followM :: Ptr r tgt -> t -> m a

--instance (LocatedM r t m, a ~ (t # r)) => PointedM r 'Unknown t m a where
--    pointM  = placeM  ; {-# INLINE pointM  #-}
--    followM = locateM ; {-# INLINE followM #-}

--instance (ReferencedM r t m a, a ~ tgt) => PointedM r ('Known tgt) t m a where
--    pointM  = referenceM   ; {-# INLINE pointM  #-}
--    followM = dereferenceM ; {-# INLINE followM #-}


data UnknownX = UnknownX -- FIXME[WD]: For greater flexibility we should implement this as data-kind - Known a / UnknownX a

-- === Definitions === --

newtype Ref r a = Ref Int deriving (Generic, NFData, Show, Eq, Ord)
type    Ptr r   = Ref r UnknownX

class Referred r t a where focus :: Ref r a -> Lens' t a
type  Referred' r t = Referred r t (t # r)

--class ReferencedM where
--    referenceM   :: Ref r a -> a -> t -> m t
--    dereferenceM :: Ref r a -> t -> m a


--class ReferencedKnownM r m t where
--    knownReferenceM   :: Ref r ('Known a) -> a -> t -> m t
--    knownDereferenceM :: Ref r ('Known a) -> t -> m a

--class ReferencedUnknownM r a m t where
--    unknownReferenceM   :: Ptr r -> a -> t -> m t
--    unknownDereferenceM :: Ptr r -> t -> m a



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
instance {-# OVERLAPPABLE #-}                  Castable (Ref r a)       (Ref r' UnknownX) where cast = rewrap ; {-# INLINE cast #-}
instance {-# OVERLAPPABLE #-}                  Castable (Ref r UnknownX) (Ref r' a')      where cast = rewrap ; {-# INLINE cast #-}
instance {-# OVERLAPPABLE #-}                  Castable (Ref r UnknownX) (Ref r' UnknownX) where cast = rewrap ; {-# INLINE cast #-}

