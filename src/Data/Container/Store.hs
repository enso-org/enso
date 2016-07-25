{-# LANGUAGE RankNTypes #-}

module Data.Container.Store where

import GHC.Exts      (Constraint)
import Control.Lens
import Data.Typeable (Proxy)


-- === Definitions === --

-- | Store of type `t` of argument `a`. An example store could be `Store Vertex a` which denotes an underlying container
--   of vertexes of `a`.
type family Store t a

class HasStore t a where
    store :: Proxy t -> Lens' a (Store t a)


-- === Utils === --

type family HasStores ts a :: Constraint where
    HasStores '[]       a = ()
    HasStores (t ': ts) a = (HasStore t a, HasStores ts a)
