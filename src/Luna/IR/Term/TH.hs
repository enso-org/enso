{-# LANGUAGE NoMonomorphismRestriction #-}

module Luna.IR.Term.TH where

import Prologue

import Data.Tag.Class
import Language.Haskell.TH.Builder
import Language.Haskell.TH as TH

termMatch :: Name -> Q TH.Pat -> Q TH.Pat
termMatch tname pat = do
    -- Just x <- lookupValueName tname
    -- runIO $ print x
    y <- reify tname
    runIO $ print y
    pat
