{-# LANGUAGE TemplateHaskell #-}

module OCI.IR.Term.Format where

import Prologue



type family Of    elem   ::  Type
type family Elems format :: [Type]

