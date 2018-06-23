{-# LANGUAGE UndecidableInstances #-}

module Luna.Pass.Sourcing.ImportsPlucker where

import Prologue

import qualified Data.Graph.Data.Component.Vector    as ComponentVector
import qualified Luna.IR                             as IR
import qualified Luna.IR.Aliases                     as Uni
import qualified Luna.IR.Layer                       as Layer
import qualified Luna.Pass                           as Pass
import qualified Luna.Pass.Attr                      as Attr
import qualified Luna.Pass.Basic                     as Pass

import Luna.Pass.Data.Root
import Luna.Pass.Sourcing.Data.Unit

data ImportsPlucker

type instance Pass.Spec ImportsPlucker t = ImportsPluckerSpec t
type family ImportsPluckerSpec t where
    ImportsPluckerSpec (Pass.In  Pass.Attrs) = '[Root]
    ImportsPluckerSpec (Pass.Out Pass.Attrs) = '[Imports]
    ImportsPluckerSpec t = Pass.BasicPassSpec t

instance Pass.Interface ImportsPlucker (Pass.Pass stage ImportsPlucker)
      => Pass.Definition stage ImportsPlucker where
    definition = do
        Root root <- Attr.get
        imps <- getImports root
        Attr.put $ Imports imps

getImports :: Pass.Interface ImportsPlucker m
           => IR.SomeTerm -> m [IR.Qualified]
getImports root = Layer.read @IR.Model root >>= \case
    Uni.Unit impHub _ _ -> do
        getImports =<< IR.source impHub
    Uni.ImportHub imps -> do
        imports <- ComponentVector.toList imps
        concat <$> traverse (getImports <=< IR.source) imports
    Uni.Imp src _ -> getImports =<< IR.source src
    Uni.ImportSource (IR.Absolute name) -> return [name]
    _ -> return []
