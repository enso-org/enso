module Data.Graph.Type.Dynamic where

import Prologue

import Data.Prop
import Data.Graph.Model.Ref


---------------------
-- === Dynamic === --
---------------------

-- Homogeneous interface

class Dynamic' t g where
    add'    :: g # t -> g -> (Ptr t, g)
    remove' :: Ptr t -> g -> g

    default add' :: Dynamic t g (g # t) => g # t -> g -> (Ptr t, g)
    add' a g = add a g & _1 %~ retarget ; {-# INLINE add' #-}

    default remove' :: Dynamic t g (g # t) => Ptr t -> g -> g
    remove' ref = remove (retarget ref :: Ref t (g # t)) ; {-# INLINE remove' #-}


-- Heterogeneous interface

class Dynamic t g a where
    add    :: a -> g -> (Ref t a, g)
    remove :: Ref t a -> g -> g

    default add :: (Dynamic' t g, a ~ (g # t)) => a -> g -> (Ref t a, g)
    add a g = add' a g & _1 %~ retarget ; {-# INLINE add #-}

    default remove :: (Dynamic' t g, a ~ (g # t)) => Ref t a -> g -> g
    remove = remove' ∘ retarget ; {-# INLINE remove #-}


-- FIXME[WD]: finish after fixing vectors in ST patch
--------------------------
---- === Reservable === --
--------------------------

--class Reservable' t g where
--    reserve' :: g -> Ptr t

--class Reservable t g a where
--    reserve :: g -> Ptr t a
