{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

module Data.Tag.Class where

import Data.Kind
import Prologue


-- === Definitions === --

data Tag (tagType :: *) (tagInstance :: *) deriving (Generic)
type family TagOf a :: Type

