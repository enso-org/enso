module Data.Item where

import qualified Data.Map        as MapLazy
import qualified Data.Map.Strict as MapStrict

type family Item a

type instance Item [a] = a
type instance Item (MapLazy.Map   k v) = (k,v)
type instance Item (MapStrict.Map k v) = (k,v)
