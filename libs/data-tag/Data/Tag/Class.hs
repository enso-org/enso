module Data.Tag.Class where

import Data.Kind

data Tag (tagType :: *) (tagInstance :: *)
type family TagOf a :: Type
