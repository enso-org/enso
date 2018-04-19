module Data.Construction where

import Prelude


--------------------------
-- === Construction === --
--------------------------


-- === Definition === --

class Monad m => Constructor  args m a where construct  ::          args -> m  a
class Monad m => Constructor1 args m a where construct1 :: ∀ t1.    args -> m (a t1)
class Monad m => Constructor2 args m a where construct2 :: ∀ t1 t2. args -> m (a t1 t2)
type Constructor'  = Constructor  ()
type Constructor1' = Constructor1 ()
type Constructor2' = Constructor2 ()

class Monad m => Destructor  m a where destruct  ::           a        -> m ()
class Monad m => Destructor1 m a where destruct1 :: ∀ t1.    (a t1)    -> m ()
class Monad m => Destructor2 m a where destruct2 :: ∀ t1 t2. (a t1 t2) -> m ()


-- === API === --

new  :: Constructor'  m a => m a
new1 :: Constructor1' m a => m (a t1)
new2 :: Constructor2' m a => m (a t1 t2)
new  = construct  () ; {-# INLINE new  #-}
new1 = construct1 () ; {-# INLINE new1 #-}
new2 = construct2 () ; {-# INLINE new2 #-}


-- === Defaulting === --

instance {-# OVERLAPPABLE #-} Constructor1 args m a
    => Constructor args m (a t) where
    construct = construct1 ; {-# INLINE construct #-}

instance {-# OVERLAPPABLE #-} Constructor2 args m a
    => Constructor1 args m (a t) where
    construct1 = construct2 ; {-# INLINE construct1 #-}

instance {-# OVERLAPPABLE #-} Destructor1 m a
    => Destructor m (a t) where
    destruct = destruct1 ; {-# INLINE destruct #-}

instance {-# OVERLAPPABLE #-} Destructor2 m a
    => Destructor1 m (a t) where
    destruct1 = destruct2 ; {-# INLINE destruct1 #-}
