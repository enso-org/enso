module Data.Tag.Class where

import Prologue
import Data.Kind


-- === Definitions === --

data Tag (tagType :: *) (tagInstance :: *)
type family TagOf a :: Type

class ShowTag1 t where
    showTag1 :: forall a. t a -> Text

class ShowTag a where
    showTag :: a -> Text


-- === Instances === --

instance {-# OVERLAPPABLE #-} ShowTag1 t => ShowTag (t a) where
    showTag = showTag1
