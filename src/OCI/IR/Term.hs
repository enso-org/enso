{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module OCI.IR.Term where

import Prologue hiding (ConversionError)

import OCI.IR.Component
import OCI.IR.Conversion

import qualified Data.Tag as Tag



componentInstance "Term"
Tag.family "TermCons"


type SomeTerm = Term ()
