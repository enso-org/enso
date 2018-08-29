{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Generics.Traversable.Instances where

import Prelude

import qualified Data.Generics.Traversable.Deriving as GTraversable

import Data.Generics.Traversable.Class
import Data.Ratio
import Data.Word


-------------------------------
-- === Default instances === --
-------------------------------

instance GTraversable c ()
instance GTraversable c Bool
instance GTraversable c Char
instance GTraversable c Double
instance GTraversable c Float
instance GTraversable c Int
instance GTraversable c Integer
instance GTraversable c Ordering
instance GTraversable c (Ratio n)
instance GTraversable c Word8
instance GTraversable c Word16
instance GTraversable c Word32
instance GTraversable c Word64

GTraversable.derive ''Maybe
GTraversable.derive ''Either
GTraversable.derive ''(,)
GTraversable.derive ''(,,)
GTraversable.derive ''(,,,)
GTraversable.derive ''(,,,,)
GTraversable.derive ''(,,,,,)
GTraversable.derive ''(,,,,,,)
GTraversable.derive ''(,,,,,,,)
GTraversable.derive ''(,,,,,,,,)
GTraversable.derive ''(,,,,,,,,,)

instance c a => GTraversable c [a] where
    gtraverse f = go where
        go []     = pure []
        go (x:xs) = (:) <$> f x <*> go xs
    {-# INLINE gtraverse #-}

