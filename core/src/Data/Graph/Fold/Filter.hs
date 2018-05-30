{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Fold.Filter where

import Prologue

import qualified Data.Graph.Fold.Class       as Fold
import qualified Data.Graph.Fold.Conditional as Fold

import Data.Property (type ( # ))
import GHC.Exts


-------------------------
-- === Filter Fold === --
-------------------------

-- === Definition === --

data Filter key val t
type instance Fold.Result (Filter _ _ t) = Fold.Result t


-- === Instances === --

instance
    ( val'  ~ (a # key)
    , fold' ~ Fold.If (val == val') t
    , Fold.Builder fold' m a
    ) => Fold.Builder (Filter key val t) m a where
    build = Fold.build @fold'
    {-# INLINE build #-}

instance
    ( val'  ~ (a # key)
    , fold' ~ Fold.If (val == val') t
    , Fold.Builder1 fold' m a
    ) => Fold.Builder1 (Filter key val t) m a where
    build1 = Fold.build1 @fold'
    {-# INLINE build1 #-}

