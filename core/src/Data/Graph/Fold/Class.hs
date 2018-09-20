{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Fold.Class where

import Prologue

import qualified Data.Generics.Traversable as GTraversable

import Data.Generics.Traversable (GTraversable)


------------------
-- === Fold === --
------------------

-- === Definition === --

type family Result t
type Transformation m t = m (Result t) -> m (Result t)

class Monad m => Builder t m a where
    build :: a -> Transformation m t
    build = \_ -> id ; {-# INLINE build #-}

class Monad m => Builder1 t m a where
    build1 :: ∀ t1. a t1 -> Transformation m t
    build1 = \_ -> id ; {-# INLINE build1 #-}

class Monad m => Builder1x t m a where
    build1x :: ∀ t1. a t1 -> Transformation m t
    build1x = \_ -> id ; {-# INLINE build1x #-}


-- === Generics === --

gbuild :: ∀ t a m. (GTraversable (Builder t m) a, Applicative m)
      => a -> Transformation m t
gbuild = GTraversable.gfoldl' @(Builder t m) (\r d -> r . build @t d) id
{-# INLINE gbuild #-}

