{-# LANGUAGE NoMonomorphismRestriction #-}

module OCI.IR.Layout.TH where

import Prologue
import Language.Haskell.TH

test a = do
    runIO $ print "------"
    runIO $ print a
    iss <- reifyInstances (mkName "IsLayout") [VarT (mkName "a")]
    runIO $ print iss

    return []
