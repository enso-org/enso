{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Generics.Traversable2.Class where

import Prelude hiding (Traversable, traverse)

import Control.Monad.Trans (MonadTrans, lift)
import Data.Kind           (Type)
import GHC.Exts            (Constraint)

import qualified Control.Monad.State.Strict as State



--------------------------
-- === Traversables === --
--------------------------

-- === Result === --

type family Result (a :: Type) :: Type -> Type


-- === Traversable === --

class Traversable (t :: Type) a where
    traverse :: Applicative (Result t) => a -> Result t a
    traverse = pure
    {-# INLINE traverse #-}

class TraverseElem t a where
    traverseElem :: Applicative (Result t) => a -> Result t a


-- === Traversable1 === --

class Traversable1 (t :: Type) a where
    traverse1 :: ∀ t1. Applicative (Result t) => a t1 -> Result t (a t1)
    traverse1 = pure
    {-# INLINE traverse1 #-}

class TraverseElem1 t a where
    traverseElem1 :: ∀ t1. Applicative (Result t) => a t1 -> Result t (a t1)

