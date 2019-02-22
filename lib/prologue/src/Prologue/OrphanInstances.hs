{-# OPTIONS_GHC -Wno-orphans #-}

module Prologue.OrphanInstances where

import Data.Default
import Control.Monad.Identity


instance Default a => Default (Identity a) where
    def = Identity def ; {-# INLINE def #-}