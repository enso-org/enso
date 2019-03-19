{-# LANGUAGE NoStrict             #-}
{-# LANGUAGE NoStrictData         #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Pass.Resolve.ConsFieldResolution where

import Prologue

import qualified Data.Graph.Data.Component.Vector    as ComponentVector
import qualified Data.Graph.Data.Component.List      as ComponentList
import qualified Data.Graph.Data.Layer.Layout        as Layout
import qualified Luna.IR                             as IR
import qualified Luna.IR.Aliases                     as Uni
import qualified Luna.IR.Layer                       as Layer
import qualified Luna.Pass                           as Pass
import qualified Luna.Pass.Attr                      as Attr
import qualified Luna.Pass.Data.Stage                as TC
import qualified Luna.Pass.Resolve.Data.Resolution   as Resolution
import qualified Luna.Pass.Scheduler                 as Scheduler

import Luna.Pass.Data.Root

data ConsFieldResolution

run :: Resolution.UnitResolver -> IR.Term IR.Record -> TC.Monad ()
run resol root = do
    Scheduler.registerAttr @Resolution.ClassResolver
    Scheduler.registerAttr @Root

    Scheduler.setAttr $ resol ^. Resolution.classes
    Scheduler.setAttr $ Root $ Layout.relayout root

    Scheduler.registerPass @TC.Stage @ConsFieldResolution
    Scheduler.runPassByType @ConsFieldResolution

type instance Pass.Spec ConsFieldResolution t = ConsFieldResolutionSpec t
type family ConsFieldResolutionSpec t where
    ConsFieldResolutionSpec (Pass.In  Pass.Attrs) = '[Root, Resolution.ClassResolver]
    ConsFieldResolutionSpec (Pass.Out Pass.Attrs) = '[]
    ConsFieldResolutionSpec t = Pass.BasicPassSpec t

instance Pass.Definition TC.Stage ConsFieldResolution where
    definition = do
        Root root <- Attr.get
        resolve root

resolve :: IR.SomeTerm -> TC.Pass ConsFieldResolution ()
resolve r = Layer.read @IR.Model r >>= \case
    Uni.Record _ _ _ cs _ ->
        traverse_ (resolve <=< IR.source) =<< ComponentVector.toList cs
    Uni.Cons n args -> do
        resolver <- Attr.get @Resolution.ClassResolver
        let resolution = Resolution.resolve n resolver
        case resolution of
            Resolution.Resolved (Resolution.ClassRef m) -> do
                as <- traverse IR.source =<< ComponentVector.toList args
                c  <- IR.resolvedCons' m n n as
                IR.replace c r
                resolve c
            _ -> pure ()
    _ -> do
        inps <- IR.inputs r
        ComponentList.mapM_ (resolve <=< IR.source) inps

