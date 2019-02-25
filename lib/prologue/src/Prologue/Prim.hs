module Prologue.Prim (module Prologue.Prim, module X) where

import Prelude
import GHC.Exts as X

newtype AnyData = AnyData Any
instance Show AnyData where show _ = "Any" ; {-# INLINE show #-}
