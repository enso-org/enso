module Data.Item where

import Prelude

import qualified Data.Map        as MapLazy
import qualified Data.Map.Strict as MapStrict
import qualified Data.Text       as Strict
import qualified Data.Text.Lazy  as Lazy


type family Item a

type instance Item [a] = a
type instance Item (MapLazy.Map   k v) = (k,v)
type instance Item (MapStrict.Map k v) = (k,v)
type instance Item Strict.Text         = Char
type instance Item Lazy.Text           = Char