{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Model.Edge where

import Prelude.Luna

import Data.Graph.Model.Ref
import Data.Graph.Model.Node

import Data.Container          hiding (Impossible)
import Data.Direction
import Data.Index
import Data.Prop
import Type.Bool

------------------
-- === Edge === --
------------------

-- === Abstraction === --

data Edge = Edge deriving (Show, Eq, Ord)
type family Connection src tgt


-- === Edge types === --

newtype Arrow tgt     = Arrow (Ref Node tgt)                deriving (Show, Eq, Ord, Functor, Traversable, Foldable)
data    Arc   src tgt = Arc   (Ref Node src) (Ref Node tgt) deriving (Show, Eq, Ord, Functor, Traversable, Foldable)
type    Link  a       = Arc   a a


-- === Utils === --

--class HasSource a where

arc :: Ref Node src -> Ref Node tgt -> Arc src tgt
arc = Arc

link :: Ref Node t -> Ref Node t -> Link t
link = arc

arrow :: Ref Node tgt -> Arrow tgt
arrow = Arrow


-- === Instances === --

-- Functors

instance Bifunctor Arc where bimap f g (Arc src tgt) = Arc (f <$> src) (g <$> tgt) ; {-# INLINE bimap #-}

-- Wrappers

makeWrapped ''Arrow

-- Directions

type instance Prop Target (Ref Edge a) = Ref Node (a # Target)
type instance Prop Source (Ref Edge a) = Ref Node (a # Source)

type instance Prop Target (Arc src tgt) = Ref Node tgt
type instance Prop Source (Arc src tgt) = Ref Node src
instance      HasSource (Arc src tgt) where source = lens (\(Arc src _) -> src) (\(Arc _ tgt) src -> Arc src tgt)
instance      HasTarget (Arc src tgt) where target = lens (\(Arc _ tgt) -> tgt) (\(Arc src _) tgt -> Arc src tgt)

type instance Prop Target (Arrow tgt) = (Ref Node tgt)
instance      HasTarget   (Arrow tgt) where target = wrapped'

-- Connections

type instance Connection (Ref Node a) (Ref Node b) = Arc a b

-- Conversions

instance (Castable src src', Castable tgt tgt') => Castable (Arc src tgt) (Arc src' tgt') where
    cast (Arc src tgt) = Arc (cast src) (cast tgt) ; {-# INLINE cast #-}


