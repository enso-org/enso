module Prologue.Data.Traversable (module Prologue.Data.Traversable, module X) where

import Prelude
import           Data.Kind       (Constraint)
import qualified Data.Traversable as T
import           Data.Traversable as X (Traversable, traverse, mapM, for)


type family Traversables (lst :: [* -> *]) :: Constraint where
    Traversables '[]       = ()
    Traversables (t ': ts) = (Traversable t, Traversables ts)

sequence :: (Traversable t, Applicative f) => t (f a) -> f (t a)
sequence = T.sequenceA ; {-# INLINE sequence #-}

mapM2 :: (Monad m, Traversables '[t1, t2])             => (a -> m b) -> t1 (t2 a)                -> m (t1 (t2 b))
mapM3 :: (Monad m, Traversables '[t1, t2, t3])         => (a -> m b) -> t1 (t2 (t3 a))           -> m (t1 (t2 (t3 b)))
mapM4 :: (Monad m, Traversables '[t1, t2, t3, t4])     => (a -> m b) -> t1 (t2 (t3 (t4 a)))      -> m (t1 (t2 (t3 (t4 b))))
mapM5 :: (Monad m, Traversables '[t1, t2, t3, t4, t5]) => (a -> m b) -> t1 (t2 (t3 (t4 (t5 a)))) -> m (t1 (t2 (t3 (t4 (t5 b)))))
mapM2 = mapM . mapM  ; {-# INLINE mapM2 #-}
mapM3 = mapM . mapM2 ; {-# INLINE mapM3 #-}
mapM4 = mapM . mapM3 ; {-# INLINE mapM4 #-}
mapM5 = mapM . mapM4 ; {-# INLINE mapM5 #-}
