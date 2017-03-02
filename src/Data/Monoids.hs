{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies         #-}

module Data.Monoids (module Data.Monoids, module X) where

import Prelude hiding (Monoid, mempty, mconcat)
import GHC.Exts (Constraint)

import qualified Data.Monoid    as M
import           Data.Semigroup as X (Semigroup, (<>), sconcat, stimes)
import           Data.List      (intersperse)

import           Data.Map.Lazy   (Map)
import qualified Data.Map.Lazy   as Map


--------------------
-- === Monoid === --
--------------------

-- === Definition === --

class (Mempty a, Semigroup a) => Monoid a where
    mconcat :: [a] -> a
    mconcat = foldr (<>) mempty


-- === Utils === --

intercalate :: Monoid a => a -> [a] -> a
intercalate delim l = mconcat (intersperse delim l)
{-# INLINE intercalate #-}


-- === Instances === --

type family Monoids lst :: Constraint where
    Monoids '[]       = ()
    Monoids (a ': as) = (Monoid a, Monoids as)

instance {-# OVERLAPPABLE #-} (Mempty a, Semigroup a) => Monoid a



-----------------------
-- === Semigroup === --
-----------------------

-- === Utils === --

mappend :: Semigroup a => a -> a -> a
mappend = (<>)

type family Semigroups lst :: Constraint where
    Semigroups '[]       = ()
    Semigroups (a ': as) = (Semigroup a, Semigroups as)



--------------------
-- === Mempty === --
--------------------

-- === Definition === --

class Mempty a where
    mempty :: a
    default mempty :: M.Monoid a => a
    mempty = M.mempty


-- === Utils === --

type family Mempties lst :: Constraint where
    Mempties '[]       = ()
    Mempties (a ': as) = (Mempty a, Mempties as)


-- === Instances === --

instance {-# OVERLAPPABLE #-} M.Monoid a => Mempty a



------------------------------------
-- === Buil-in types handling === --
------------------------------------

instance Mempty [a]       where mempty = []
instance Mempty (Maybe a) where mempty = Nothing
instance Mempty (Map k a) where mempty = Map.empty
