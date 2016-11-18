{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Model.Pointer (module Data.Graph.Model.Pointer, module X) where

import Luna.Prelude

import Data.Direction    as X
import Data.Index
import Data.Layer_OLD
import Old.Data.Prop
import Data.Layer_OLD.Cover_OLD

-- TODO: refactor the import - there is no connection between graph Builder
--       and this module. In fact the GraphBuilder should be renamed
import qualified Data.Graph.Builder.Class                        as Graph.Builder


data Knowledge a = Known a
                 | Unknown
                 deriving (Show, Functor, Foldable, Traversable)


----------------------
-- === Pointers === --
----------------------

-- === Definitions === --

newtype Ptr r (tgt :: Knowledge *) = Ptr Int deriving (Generic, NFData, Show, Eq, Ord)
type    Loc r   = Ptr r 'Unknown   -- Depreciated
type    Ref r a = Ptr r ('Known a) -- Depreciated




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

-- type  Pointed  r tgt t = PointedM r tgt t Identity
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

pointed :: PointedM r tgt t Identity a => Ptr r tgt -> Lens' t a
pointed ptr = lens (runIdentity ∘ readPtrM ptr) (runIdentity ∘∘ flip (writePtrM ptr))
{-# INLINE pointed #-}


-- === Utils === --

retarget :: Ptr r a -> Ptr r a'
retarget = rewrap ; {-# INLINE retarget #-}


-- === Instances === --

-- Wrappers
makeWrapped ''Ptr

instance Num (Ptr r tgt) where fromInteger = wrap' . fromInteger ; {-# INLINE fromInteger #-}

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








-- === Definitions === --

-- TODO: Change internal implementation from (Ptr r tgt) to (RawPtr) (no params) containing Int
newtype Ptr2 r   = Ptr2 (Ptr r 'Unknown  ) deriving (Generic, NFData, Show, Eq, Ord, Num)
newtype Ref2 r a = Ref2 (Ptr r ('Known a)) deriving (Generic, NFData, Show, Eq, Ord, Num)


unsafeRefer :: Ptr2 r -> Ref2 r a
unsafeRefer = wrap . retarget . unwrap' ; {-# INLINE unsafeRefer #-}

-- === Location Accessors === --

type  Pointable  r t = PointableM r t Identity
class PointableM r t m a where
    setPtrM  :: Ptr2 r -> a -> t -> m t
    viewPtrM :: Ptr2 r      -> t -> m a

type  Referable r t = ReferableM r t Identity
class ReferableM r t m where
    setRefM  :: Ref2 r a -> a -> t -> m t
    viewRefM :: Ref2 r a      -> t -> m a
    viewPtrs :: t -> m [Ptr2 r]


-- | General interface for location handling
--   It is just a proxy class over Pointable and Referable
class LocalizableM loc t m a where
    setLocM  :: loc -> a -> t -> m t
    viewLocM :: loc -> t -> m a

instance PointableM r t m a => LocalizableM (Ptr2 r) t m a where
    setLocM  = setPtrM  ; {-# INLINE setLocM  #-}
    viewLocM = viewPtrM ; {-# INLINE viewLocM #-}

instance (ReferableM r t m, a ~ a') => LocalizableM (Ref2 r a) t m a' where
    setLocM  = setRefM  ; {-# INLINE setLocM  #-}
    viewLocM = viewRefM ; {-# INLINE viewLocM #-}


-- === Location Monads === --


-- MonadRef
class MonadRef r m where
    writeRef :: Ref2 r a -> a -> m ()
    readRef  :: Ref2 r a      -> m a
    pointers :: m [Ptr2 r]

instance (Graph.Builder.MonadBuilder g m, ReferableM t g m) => MonadRef t m where
    writeRef ref a = Graph.Builder.modifyM_ (setRefM ref a) ; {-# INLINE writeRef #-}
    readRef  ref   = viewRefM ref =<< Graph.Builder.get     ; {-# INLINE readRef  #-}
    pointers       = viewPtrs =<< Graph.Builder.get         ; {-# INLINE pointers #-}


class MonadPtr r m a where
    writePtr :: Ptr2 r -> a -> m ()
    readPtr  :: Ptr2 r      -> m a

class Referred ref m where
    refer   :: ref a -> a -> m ()
    derefer :: ref a      -> m a

class MonadAccess t m a where
    write2 :: t -> a -> m ()
    read2  :: t      -> m a


instance MonadRef r m => Referred (Ref2 r) m where
    refer   = writeRef ; {-# INLINE refer   #-}
    derefer = readRef  ; {-# INLINE derefer #-}


-- === Isntances === --

-- Wrappers
makeWrapped ''Ptr2
makeWrapped ''Ref2

-- Construction
type instance Deconstructed (Ref2 r a) = a

-- Index
type instance Index  (Ptr2 r)   = Int
type instance Index  (Ref2 r a) = Int

instance HasIdx (Ptr2 r)   where idx = wrapped' . wrapped' ; {-# INLINE idx #-}
instance HasIdx (Ref2 r a) where idx = wrapped' . wrapped' ; {-# INLINE idx #-}
