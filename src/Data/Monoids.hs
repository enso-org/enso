{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies         #-}

module Data.Monoids (module Data.Monoids, module X) where

import Prelude hiding (Monoid, mempty, mconcat)
import GHC.Exts (Constraint)

import qualified Data.Monoid    as M
import           Data.Semigroup as X (Semigroup, (<>), sconcat, stimes)

import           Data.Map.Lazy   (Map)
import qualified Data.Map.Lazy   as Map
import qualified Data.Foldable   as Foldable


--------------------
-- === Mempty === --
--------------------

-- === Definition === --

class Mempty a where
    mempty :: a
    default mempty :: M.Monoid a => a
    mempty = M.mempty ; {-# INLINE mempty #-}

instance {-# OVERLAPPABLE #-} M.Monoid a => Mempty a


-- === Utils === --

type family Mempties lst :: Constraint where
    Mempties '[]       = ()
    Mempties (a ': as) = (Mempty a, Mempties as)


-----------------------
-- === Semigroup === --
-----------------------

-- === Utils === --

mappend :: Semigroup a => a -> a -> a
mappend = (<>) ; {-# INLINE mappend #-}

mappendWith :: Semigroup a => a -> a -> a -> a
mappendWith m l r = l <> m <> r ; {-# INLINE mappendWith #-}

mappendBetween :: Semigroup a => a -> a -> a -> a
mappendBetween l r m = l <> m <> r ; {-# INLINE mappendBetween #-}

type family Semigroups lst :: Constraint where
    Semigroups '[]       = ()
    Semigroups (a ': as) = (Semigroup a, Semigroups as)

    
--------------------
-- === Monoid === --
--------------------

-- === Definition === --

class (Mempty a, Semigroup a) => Monoid a where
    mconcat :: [a] -> a
    mconcat = foldr (<>) mempty ; {-# INLINE mconcat #-}

instance {-# OVERLAPPABLE #-} (Mempty a, Semigroup a) => Monoid a


-- === Utils === --

mconcat' :: (Foldable t, Monoid a) => t a -> a
mconcat' = foldr (<>) mempty ; {-# INLINE mconcat' #-}

intersperse :: Foldable f => a -> f a -> [a]
intersperse sep a = case Foldable.toList a of
    []      -> []
    (x:xs)  -> x : prependToAll sep xs where
        prependToAll sep = \case
            []     -> []
            (x:xs) -> sep : x : prependToAll sep xs
{-# INLINE intersperse #-}

intercalate :: (Monoid a, Foldable f) => a -> f a -> a
intercalate delim l = mconcat (intersperse delim l) ; {-# INLINE intercalate #-}

intercalate' :: Monoid a => a -> [a] -> a
intercalate' = intercalate ; {-# INLINE intercalate' #-}


-- === Instances === --

type family Monoids lst :: Constraint where
    Monoids '[]       = ()
    Monoids (a ': as) = (Monoid a, Monoids as)












------------------------------------
-- === Buil-in types handling === --
------------------------------------

instance Mempty [a]       where mempty = []
instance Mempty (Maybe a) where mempty = Nothing
instance Mempty (Map k a) where mempty = Map.empty
