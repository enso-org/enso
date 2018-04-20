{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Construction where

import Prelude


--------------------------
-- === Construction === --
--------------------------


-- === Definition === --

class Monad m => Constructor  args a m where construct  ::          args -> m  a
class Monad m => Constructor1 args a m where construct1 :: ∀ t1.    args -> m (a t1)
class Monad m => Constructor2 args a m where construct2 :: ∀ t1 t2. args -> m (a t1 t2)
type Constructor'  = Constructor  ()
type Constructor1' = Constructor1 ()
type Constructor2' = Constructor2 ()

class Monad m => Destructor  a m where destruct  ::           a        -> m ()
class Monad m => Destructor1 a m where destruct1 :: ∀ t1.    (a t1)    -> m ()
class Monad m => Destructor2 a m where destruct2 :: ∀ t1 t2. (a t1 t2) -> m ()


-- === API === --

new  :: ∀ a m.       Constructor'  a m => m a
new1 :: ∀ a m t1.    Constructor1' a m => m (a t1)
new2 :: ∀ a m t1 t2. Constructor2' a m => m (a t1 t2)
new  = construct  () ; {-# INLINE new  #-}
new1 = construct1 () ; {-# INLINE new1 #-}
new2 = construct2 () ; {-# INLINE new2 #-}


-- === Defaulting === --

instance {-# OVERLAPPABLE #-} (Monad m, Constructor1 args a m)
    => Constructor args (a t) m where
    construct = construct1 ; {-# INLINE construct #-}

instance {-# OVERLAPPABLE #-} (Monad m, Constructor2 args a m)
    => Constructor1 args (a t) m where
    construct1 = construct2 ; {-# INLINE construct1 #-}

instance {-# OVERLAPPABLE #-} (Monad m, Destructor1 a m)
    => Destructor (a t) m where
    destruct = destruct1 ; {-# INLINE destruct #-}

instance {-# OVERLAPPABLE #-} (Monad m, Destructor2 a m)
    => Destructor1 (a t) m where
    destruct1 = destruct2 ; {-# INLINE destruct1 #-}
