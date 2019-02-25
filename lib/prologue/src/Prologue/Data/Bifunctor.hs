module Prologue.Data.Bifunctor (module Prologue.Data.Bifunctor, module X) where

import qualified Data.Bifunctor    as B
import           Data.Bifunctor    as X (Bifunctor, bimap)
import           Data.Bifunctor.TH as X (deriveBifunctor, deriveBifoldable, deriveBitraversable)

mapFirst  :: Bifunctor p => (a -> b) -> p a c -> p b c
mapSecond :: Bifunctor p => (b -> c) -> p a b -> p a c
mapFirst  = B.first  ; {-# INLINE mapFirst  #-}
mapSecond = B.second ; {-# INLINE mapSecond #-}
