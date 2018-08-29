{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Generics.Traversable2.Instances where

import           Prologue hiding (Traversable)
import qualified Prologue as P

import qualified Data.Generics.Traversable2.Deriving as Generic.Traversable

import Data.Generics.Traversable2.Class
import Data.Ratio
import Data.Word


-------------------------------
-- === Default instances === --
-------------------------------

instance Traversable t ()
instance Traversable t Bool
instance Traversable t Char
instance Traversable t Double
instance Traversable t Float
instance Traversable t Int
instance Traversable t Integer
instance Traversable t Ordering
instance Traversable t (Ratio n)
instance Traversable t Word8
instance Traversable t Word16
instance Traversable t Word32
instance Traversable t Word64

Generic.Traversable.derive ''Maybe
Generic.Traversable.derive ''Either
Generic.Traversable.derive ''(,)
Generic.Traversable.derive ''(,,)
Generic.Traversable.derive ''(,,,)
Generic.Traversable.derive ''(,,,,)
Generic.Traversable.derive ''(,,,,,)
Generic.Traversable.derive ''(,,,,,,)
Generic.Traversable.derive ''(,,,,,,,)
Generic.Traversable.derive ''(,,,,,,,,)
Generic.Traversable.derive ''(,,,,,,,,,)

instance TraverseElem t a => Traversable t [a] where
    traverse = P.traverse (traverseElem @t)
    {-# INLINE traverse #-}

