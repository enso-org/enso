module Prologue.Data.OneTuple where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Fix
import           Data.Ix
import           Data.Monoid
import           Data.Semigroup
import           Prelude

newtype OneTuple a = OneTuple { fromOneTuple :: a }
    deriving ( Bounded, Enum, Eq, Foldable, Functor, Ix, Monoid, Ord, Read
             , Semigroup, Show, Traversable )

instance Applicative OneTuple where
    pure = OneTuple ; {-# INLINE pure #-}
    OneTuple f <*> OneTuple a = OneTuple $ f a ; {-# INLINE (<*>) #-}

instance Monad OneTuple where
    return = pure ; {-# INLINE return #-}
    OneTuple a >>= f = f a ; {-# INLINE (>>=) #-}

instance MonadFix OneTuple where
    mfix f = OneTuple $ fix (fromOneTuple . f) ; {-# INLINE mfix #-}
