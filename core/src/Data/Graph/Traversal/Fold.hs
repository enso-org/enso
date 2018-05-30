{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Traversal.Fold where

import Prologue hiding (Traversable, fold, fold1, traverse)

import qualified Data.Generics.Traversable as GTraversable

import Data.Generics.Traversable (GTraversable)


------------------
-- === Fold === --
------------------

-- === Definition === --

type family Result t

class Monad m => Builder t m a where
    build :: a -> m (Result t) -> m (Result t)
    build = \_ -> id ; {-# INLINE build #-}

class Monad m => Builder1 t m a where
    build1 :: ∀ t1. a t1 -> m (Result t) -> m (Result t)
    build1 = \_ -> id ; {-# INLINE build1 #-}


-- === Generics === --

gbuild :: ∀ t a m. (GTraversable (Builder t m) a, Applicative m)
      => a -> m (Result t) -> m (Result t)
gbuild = GTraversable.gfoldl' @(Builder t m) (\r d x -> r $! build @t d x) id
{-# INLINE gbuild #-}
