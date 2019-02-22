module Prologue.Data.Pointed (module Prologue.Data.Pointed, module X) where

import Data.Pointed   as X (Pointed  , point)
import Data.Copointed as X (Copointed, copoint)

import Prelude (Functor, fmap, const)
import Control.Lens


type Bipointed t = (Pointed t, Copointed t)

pointed   :: Bipointed t => Iso a b (t a) (t b)
copointed :: Bipointed t => Iso (t a) (t b) a b
pointed   = iso point   copoint ; {-# INLINE pointed   #-}
copointed = iso copoint point   ; {-# INLINE copointed #-}

copointedLens :: (Copointed t, Functor t) => Lens (t a) (t b) a b
copointedLens = lens copoint (\ta b -> fmap (const b) ta) ; {-# INLINE copointedLens #-}
