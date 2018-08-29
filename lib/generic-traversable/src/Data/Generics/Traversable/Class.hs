{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

module Data.Generics.Traversable.Class where

import Prelude

import Data.Kind (Type)
import GHC.Exts  (Constraint)


--------------------------
-- === GTraversable === --
--------------------------

-- === Definition === --

class GTraversable (ctx :: Type -> Constraint) a where
    gtraverse :: ∀ f. Applicative f
              => (∀ a'. ctx a' => a' -> f a') -> a -> f a
    gtraverse _ = pure ; {-# INLINE gtraverse #-}

class GTraversable1 (ctx :: (Type -> Type) -> Constraint) a where
    gtraverse1 :: ∀ f t1. Applicative f
               => (∀ a' t1'. ctx a' => a' t1' -> f (a' t1')) -> a t1 -> f (a t1)
    gtraverse1 _ = pure ; {-# INLINE gtraverse1 #-}

