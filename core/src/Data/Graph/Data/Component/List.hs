module Data.Graph.Data.Component.List where

import Prologue hiding (foldr, mapM)

import qualified Data.Graph.Data.Component.Class as Component
import qualified Data.Graph.Data.Layer.Layout    as Layout

import Data.Graph.Data.Component.Class (Component)



---------------------------
-- === ComponentList === --
---------------------------

-- === Definition === --

type SomeComponentList = ComponentList ()
data ComponentList comp
    = Cons !(Component.Some comp) !(ComponentList comp)
    | Nil
    deriving Show


-- === Helpers === --

type family ComponentLists ls where
    ComponentLists '[]       = '[]
    ComponentLists (l ': ls) = ComponentList l ': ComponentLists ls


-- === API === --

foldr :: (Component.Some comp -> a -> a) -> a -> ComponentList comp -> a
foldr = \f z ->
    let go Nil         = z
        go (Cons y ys) = f y $! go ys
    in  go
{-# INLINE [0] foldr #-}

foldl' :: (a -> Component.Some comp -> a) -> a -> ComponentList comp -> a
foldl' = \k s xs -> foldr (\v fn -> oneShot (\z -> z `seq` fn (k z v))) id xs s
{-# INLINE foldl' #-}

foldlM :: Monad m
       => (a -> Component.Some comp -> m a) -> a -> ComponentList comp -> m a
foldlM = \f z0 xs ->
    let f' x k z = f z x >>= k
    in  foldr f' pure xs z0
{-# INLINE foldlM #-}

mapList :: (Component.Some comp -> a) -> ComponentList comp -> [a]
mapList = \f ->
    let go = \case
            Nil         -> []
            Cons !a !as ->
                let a'  = f a
                    as' = go as
                in  a' : as'
        {-# INLINABLE go #-}
    in go
{-# INLINE mapList #-}

mapM :: Applicative m
     => (Component.Some comp -> m a) -> ComponentList comp -> m [a]
mapM = \f ->
    let go = \case
            Nil         -> pure []
            Cons !a !as -> do
                a'  <- f a
                as' <- go as
                pure $ a' : as'
        {-# INLINABLE go #-}
    in go
{-# INLINE mapM #-}

mapM_ :: Applicative m
       => (Component.Some comp -> m a) -> ComponentList comp -> m ()
mapM_ = \f -> foldr ((*>) . f) (return ())
{-# INLINE mapM_ #-}

length :: ComponentList comp -> Int
length = go 0 where
    go :: Int -> ComponentList comp -> Int
    go s = \case
        Nil -> s
        Cons _ lst -> go (s + 1) lst
{-# INLINABLE length #-}


-- === Instances === --

type instance Item (ComponentList comp) = Component.Some comp

instance Mempty  (ComponentList comp) where
    mempty = Nil
    {-# INLINE mempty #-}

instance Default (ComponentList comp) where
    def = mempty
    {-# INLINE def #-}

instance comp ~ comp'
      => Convertible [Component comp layout] (ComponentList comp') where
    convert = \case
        []     -> Nil
        (a:as) -> Cons (Layout.relayout a) $! convert as
    {-# INLINABLE convert #-}

instance comp ~ comp'
      => Convertible (ComponentList comp') [Component.Some comp] where
    convert = \case
        Nil       -> []
        Cons a as -> let as' = convert as in a : as'
    {-# INLINABLE convert #-}

instance Semigroup (ComponentList comp) where
    l <> l' = case l of
        Nil       -> l'
        Cons a as -> Cons a (as <> l')
    {-# INLINABLE (<>) #-}
