{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Construction where

import Prelude

import qualified Data.Generics.Traversable as GTraversable
-- import qualified Foreign.Marshal.Alloc     as Mem

import Data.Generics.Traversable (GTraversable)
import Data.Foldable             (traverse_)


--------------------------
-- === Construction === --
--------------------------


-- === Definition === --

class Monad m => Constructor m args a where
    construct :: args -> m a

class Monad m => Constructor1 m args a where
    construct1 :: ∀ t1. args -> m (a t1)

class Monad m => Constructor2 m args a where
    construct2 :: ∀ t1 t2. args -> m (a t1 t2)

type Constructor'  m = Constructor  m ()
type Constructor1' m = Constructor1 m ()
type Constructor2' m = Constructor2 m ()


-- === API === --

construct'  :: ∀ a m.       Constructor'  m a => m a
construct1' :: ∀ a m t1.    Constructor1' m a => m (a t1)
construct2' :: ∀ a m t1 t2. Constructor2' m a => m (a t1 t2)
construct'  = construct  () ; {-# INLINE construct'  #-}
construct1' = construct1 () ; {-# INLINE construct1' #-}
construct2' = construct2 () ; {-# INLINE construct2' #-}


-- === Defaulting === --

instance {-# OVERLAPPABLE #-} (Monad m, Constructor1 m args a)
    => Constructor m args (a t) where
    construct = construct1 ; {-# INLINE construct #-}

instance {-# OVERLAPPABLE #-} (Monad m, Constructor2 m args a)
    => Constructor1 m args (a t) where
    construct1 = construct2 ; {-# INLINE construct1 #-}



-------------------------
-- === Destruction === --
-------------------------

-- === Definition === --

class Monad m => Destructor  m a where destruct  ::           a        -> m ()
class Monad m => Destructor1 m a where destruct1 :: ∀ t1.    (a t1)    -> m ()
class Monad m => Destructor2 m a where destruct2 :: ∀ t1 t2. (a t1 t2) -> m ()


-- === Defaulting === --

instance {-# OVERLAPPABLE #-} (Monad m, Destructor1 m a)
    => Destructor m (a t) where
    destruct = destruct1 ; {-# INLINE destruct #-}

instance {-# OVERLAPPABLE #-} (Monad m, Destructor2 m a)
    => Destructor1 m (a t) where
    destruct1 = destruct2 ; {-# INLINE destruct1 #-}

instance {-# OVERLAPPABLE #-} (Monad m, GTraversable (Destructor m) a)
    => Destructor m a where
    destruct = GTraversable.gmapM_ @(Destructor m) destruct ; {-# INLINE destruct #-}



---------------------------------
-- === Shallow Destruction === --
---------------------------------

-- === Definition === --

class Monad m => ShallowDestructor m a where
    destructShallow :: a -> m ()

class Monad m => ShallowDestructor1 m a where
    destructShallow1 :: ∀ t1. (a t1) -> m ()

class Monad m => ShallowDestructor2 m a where
    destructShallow2 :: ∀ t1 t2. (a t1 t2) -> m ()


-- === Defaulting === --

instance {-# OVERLAPPABLE #-} (Monad m, ShallowDestructor1 m a)
    => ShallowDestructor m (a t) where
    destructShallow = destructShallow1 ; {-# INLINE destructShallow #-}

instance {-# OVERLAPPABLE #-} (Monad m, ShallowDestructor2 m a)
    => ShallowDestructor1 m (a t) where
    destructShallow1 = destructShallow2 ; {-# INLINE destructShallow1 #-}

instance {-# OVERLAPPABLE #-} (Monad m, GTraversable (ShallowDestructor m) a)
    => ShallowDestructor m a where
    destructShallow = GTraversable.gmapM_ @(ShallowDestructor m) destructShallow ; {-# INLINE destructShallow #-}

-- === Instances === --

-- FIXME[MK->WD]: This instance is equivalent to the GTraversable default above,
-- but can't be resolved due to instance resolution rules ((Maybe a) looks more like
-- (a t) in the ShallowDestructor1 default than a in the GTraversable one.
instance {-# OVERLAPPABLE #-} (Applicative m, ShallowDestructor m a)
    => ShallowDestructor m (Maybe a) where
    destructShallow = traverse_ destructShallow ; {-# INLINE destructShallow #-}

instance {-# OVERLAPPABLE #-} (Applicative m, ShallowDestructor m a, ShallowDestructor m b)
    => ShallowDestructor m (a, b) where
    destructShallow = \(a, b) -> destructShallow a >> destructShallow b ; {-# INLINE destructShallow #-}



-- -----------------------
-- -- === Allocator === --
-- -----------------------

-- -- === Definition === --

-- class Allocator t m a where
--     alloc :: Int -> m (Ptr a)


-- -- === Malloc === --

-- data Malloc
-- instance Allocator Malloc m a where
--     alloc = liftIO . Mem.malloc
--     {-# INLINE alloc #-}



-- -------------------------------
-- -- === Copy Construction === --
-- -------------------------------

-- -- === Definition === --

-- class CopyWith t m a where
--     copyWith :: a -> m a


-- -- === Malloc === --

-- type Copy = CopyWith Malloc
-- copy :: Copy m a => a -> m a
-- copy = copyWith @Malloc
-- {-# INLINE copy #-}

