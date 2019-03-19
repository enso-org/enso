{-# LANGUAGE TypeFamilies #-}

module Prologue.Control.DeepSeq (module Prologue.Control.DeepSeq, module X) where

import GHC.Exts        (Constraint)
import Control.DeepSeq as X (NFData, rnf, force)
import GHC.IO          as X (evaluate)

type family NFDatas lst :: Constraint where
    NFDatas '[]       = ()
    NFDatas (a ': as) = (NFData a, NFDatas as)
