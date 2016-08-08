{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Model.Pointer (module Data.Graph.Model.Pointer, module X) where

import Prelude.Luna

import Data.Construction
import Data.Direction    as X
import Data.Index
import Data.Layer_OLD
import Data.Prop
import Data.Layer_OLD.Cover_OLD

data Knowledge a = Known a
                 | Unknown
                 deriving (Show, Functor, Foldable, Traversable)


----------------------
-- === Pointers === --
----------------------

-- === Definitions === --

newtype Ptr r (tgt :: Knowledge *) = Ptr Int deriving (Generic, NFData, Show, Eq, Ord)
type    Loc r   = Ptr r 'Unknown
type    Ref r a = Ptr r ('Known a)


-- === Accessors === --

-- Specific accessors

class ReferencedM r t m a where
    writeRefM :: Ref r a -> a -> t -> m t
    readRefM  :: Ref r a -> t -> m a

class LocatedM r t m where
    writeLocM :: Loc r -> t # r -> t -> m t
    readLocM  :: Loc r -> t -> m (t # r)

    default writeLocM :: ReferencedM r t m (t # r) => Loc r -> t # r -> t -> m t
    writeLocM = writeRefM ∘ retarget ; {-# INLINE writeLocM #-}

    default readLocM :: ReferencedM r t m (t # r) => Loc r -> t -> m (t # r)
    readLocM = readRefM ∘ retarget ; {-# INLINE readLocM #-}

-- General accessors

type  Pointed  r tgt t = PointedM r tgt t Identity
class PointedM r tgt t m a where
    writePtrM :: Ptr r tgt -> a -> t -> m t
    readPtrM  :: Ptr r tgt -> t -> m a

instance (LocatedM r t m, a ~ (t # r)) => PointedM r 'Unknown t m a where
    writePtrM = writeLocM ; {-# INLINE writePtrM #-}
    readPtrM  = readLocM  ; {-# INLINE readPtrM  #-}

instance (ReferencedM r t m a, a ~ tgt) => PointedM r ('Known tgt) t m a where
    writePtrM = writeRefM ; {-# INLINE writePtrM #-}
    readPtrM  = readRefM  ; {-# INLINE readPtrM  #-}

-- Pure accessors

pointed :: Pointed r tgt t a => Ptr r tgt -> Lens' t a
pointed ptr = lens (runIdentity ∘ readPtrM ptr) (runIdentity ∘∘ flip (writePtrM ptr))
{-# INLINE pointed #-}


-- === Utils === --

retarget :: Ptr r a -> Ptr r a'
retarget = rewrap ; {-# INLINE retarget #-}


-- === Instances === --

-- Wrappers
makeWrapped ''Ptr

-- Ref primitive instances
type instance Uncovered     (Ref r a) = Uncovered a
type instance Unlayered     (Ref r a) = a
type instance Deconstructed (Ref r a) = a

-- Index
type instance Index  (Ptr r a) = Int
instance      HasIdx (Ptr r a) where idx = wrapped' ; {-# INLINE idx #-}

-- Conversions
instance {-# OVERLAPPABLE #-} Castable a a'
                           => Castable (Ref r a) (Ref r' a') where cast = rewrap ; {-# INLINE cast #-}
instance {-# OVERLAPPABLE #-} Castable (Ptr r a) (Ptr r  a ) where cast = id     ; {-# INLINE cast #-}
instance {-# OVERLAPPABLE #-} Castable (Ref r a) (Ref r  a ) where cast = id     ; {-# INLINE cast #-}
instance {-# OVERLAPPABLE #-} Castable (Loc r  ) (Loc r    ) where cast = id     ; {-# INLINE cast #-}
instance {-# OVERLAPPABLE #-} Castable (Ptr r a) (Loc r'   ) where cast = rewrap ; {-# INLINE cast #-}
instance {-# OVERLAPPABLE #-} Castable (Loc r  ) (Ptr r' a') where cast = rewrap ; {-# INLINE cast #-}
instance {-# OVERLAPPABLE #-} Castable (Loc r  ) (Loc r'   ) where cast = rewrap ; {-# INLINE cast #-}








--newtype Ref r a = Ref Int deriving (Generic, NFData, Show, Eq, Ord)
--type    Loc r   = Ref r 'Unknown

    --class Referred r t a where pointed :: Ref r a -> Lens' t a
    --type  Referred' r t = Referred r t (t # r)

    --pointed' :: Referred' r t => Ref r (t # r) -> Lens' t (t # r)
    --pointed' = pointed

--class ReferencedM where
--    referenceM   :: Ref r a -> a -> t -> m t
--    dereferenceM :: Ref r a -> t -> m a


--class ReferencedKnownM r m t where
--    knownReferenceM   :: Ref r ('Known a) -> a -> t -> m t
--    knownDereferenceM :: Ref r ('Known a) -> t -> m a

--class ReferencedUnknownM r a m t where
--    unknownReferenceM   :: Ptr r -> a -> t -> m t
--    unknownDereferenceM :: Ptr r -> t -> m a
