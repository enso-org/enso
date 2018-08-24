{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Fold.Conditional where

import Prologue

import qualified Data.Graph.Fold.Class as Fold



------------------------------
-- === Conditional Fold === --
------------------------------

-- === Definition === --

data If (pass :: Bool) t
type instance Fold.Result (If _ t) = Fold.Result t


-- === Instances === --

instance Monad m            => Fold.Builder (If 'False t) m a
instance Fold.Builder t m a => Fold.Builder (If 'True  t) m a where
    build = Fold.build @t ; {-# INLINE build #-}

instance Monad m             => Fold.Builder1 (If 'False t) m a
instance Fold.Builder1 t m a => Fold.Builder1 (If 'True  t) m a where
    build1 = Fold.build1 @t ; {-# INLINE build1 #-}

