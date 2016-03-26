module Data.Graph.Type.Dynamic where

import Prologue

import Data.Prop
import Data.Graph.Model.Ptr


-----------------------
---- === Dynamic === --
-----------------------


--class Dynamic' t g where
--    add'    :: g # t -> g -> (Loc t, g)
--    remove' :: Loc t -> g -> g

--    default add' :: Dynamic t g (g # t) => g # t -> g -> (Loc t, g)
--    add' a g = add a g & _1 %~ retarget ; {-# INLINE add' #-}

--    default remove' :: Dynamic t g (g # t) => Loc t -> g -> g
--    remove' ref = remove (retarget ref :: Ref t (g # t)) ; {-# INLINE remove' #-}



--class Dynamic t g a where
--    add    :: a -> g -> (Ref t a, g)
--    remove :: Ref t a -> g -> g

--    default add :: (Dynamic' t g, a ~ (g # t)) => a -> g -> (Ref t a, g)
--    add a g = add' a g & _1 %~ retarget ; {-# INLINE add #-}

--    default remove :: (Dynamic' t g, a ~ (g # t)) => Ref t a -> g -> g
--    remove = remove' ∘ retarget ; {-# INLINE remove #-}

-- === Definitions === --

-- Homogeneous interface

type  Dynamic'  t g = DynamicM' t g Identity
class DynamicM' t g m where
    addM'    :: g # t -> g -> m (Loc t, g)
    removeM' :: Loc t -> g -> m g

    default addM'    :: (DynamicM t g m (g # t), Functor m) => g # t -> g -> m (Loc t, g)
    default removeM' :: (DynamicM t g m (g # t), Functor m) => Loc t -> g -> m g

    addM'    el g = addM el g <&> _1 %~ retarget            ; {-# INLINE addM' #-}
    removeM' ref  = removeM (retarget ref :: Ref t (g # t)) ; {-# INLINE removeM' #-}

-- Heterogeneous interface

type  Dynamic  t g = DynamicM t g Identity
class DynamicM t g m a where
    addM    :: a -> g -> m (Ref t a, g)
    removeM :: Ref t a -> g -> m g

    default addM    :: (DynamicM' t g m , a ~ (g # t), Functor m) => a -> g -> m (Ref t a, g)
    default removeM :: (DynamicM' t g m , a ~ (g # t), Functor m) => Ref t a -> g -> m g

    addM a g = addM' a g <&> _1 %~ retarget ; {-# INLINE addM #-}
    removeM = removeM' ∘ retarget           ; {-# INLINE removeM #-}


-- === Utils === --

-- Monadic

addM_ :: (DynamicM t g m a, Functor m) => a -> g -> m (Ref t a)
addM_ = fst <∘∘> addM ; {-# INLINE addM_ #-}

removeM_ :: (DynamicM t g m a, Functor m) => Ref t a -> g -> m ()
removeM_ = void ∘∘ removeM ; {-# INLINE removeM_ #-}

addM'_ :: (DynamicM' t g m, Functor m) => g # t -> g -> m (Loc t)
addM'_ = fst <∘∘> addM' ; {-# INLINE addM'_ #-}

removeM'_ :: (DynamicM' t g m, Functor m) => Loc t -> g -> m ()
removeM'_ = void ∘∘ removeM' ; {-# INLINE removeM'_ #-}

-- Pure

add :: Dynamic t g a => a -> g -> (Ref t a, g)
add = runIdentity ∘∘ addM ; {-# INLINE add #-}

remove :: Dynamic t g a => Ref t a -> g -> g
remove = runIdentity ∘∘ removeM ; {-# INLINE remove #-}

add' :: Dynamic' t g => g # t -> g -> (Loc t, g)
add' = runIdentity ∘∘ addM' ; {-# INLINE add' #-}

remove' :: Dynamic' t g => Loc t -> g -> g
remove' = runIdentity ∘∘ removeM' ; {-# INLINE remove' #-}



-- FIXME[WD]: finish after fixing vectors in ST patch
--------------------------
---- === Reservable === --
--------------------------

--class Reservable' t g where
--    reserve' :: g -> Loc t

--class Reservable t g a where
--    reserve :: g -> Loc t a
