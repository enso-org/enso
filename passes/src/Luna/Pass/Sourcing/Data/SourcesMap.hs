{-# LANGUAGE NoStrict     #-}
{-# LANGUAGE NoStrictData #-}

module Luna.Pass.Sourcing.Data.SourcesMap where

import Prologue

import qualified Data.Map       as Map
import qualified Luna.IR        as IR
import qualified Luna.Pass.Attr as Attr

import System.FilePath                 (FilePath)

newtype SourcesMap = SourcesMap (Map.Map IR.Qualified FilePath)
type instance Attr.Type SourcesMap = Attr.Atomic
instance Default SourcesMap where
    def = SourcesMap def

makeLenses ''SourcesMap

