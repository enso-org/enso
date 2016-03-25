{-# LANGUAGE TypeFamilies #-}

module Prologue.Control.DeepSeq (module Prologue.Control.DeepSeq, module X) where

import GHC.Prim        (Constraint)
import Control.DeepSeq as X (NFData, rnf, force)

type family NFDatas lst :: Constraint where
    NFDatas '[]       = ()
    NFDatas (a ': as) = (NFData a, NFDatas as)
