{-# LANGUAGE NoStrict             #-}
{-# LANGUAGE NoStrictData         #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Pass.Resolve.ConsResolution where

import Prologue

import qualified Data.Graph.Data.Component.List   as ComponentList
import qualified Data.Graph.Data.Component.Vector as ComponentVector
import qualified Data.Graph.Data.Layer.Layout     as Layout
import qualified Luna.IR                          as IR
import qualified Luna.IR.Aliases                  as Uni
import qualified Luna.IR.Layer                    as Layer
import qualified Luna.Pass                        as Pass
import qualified Luna.Pass.Attr                   as Attr
import qualified Luna.Pass.Data.Error             as Error
import qualified Luna.Pass.Data.Stage             as TC
import qualified Luna.Pass.Sourcing.Data.Class    as Class

import Luna.Pass.Data.Root
import Luna.Pass.Data.UniqueNameGen      as NameGen
import Luna.Pass.Resolve.Data.Resolution hiding (cons)


data ConsResolution

type instance Pass.Spec ConsResolution t = ConsResolutionSpec t
type family ConsResolutionSpec t where
    ConsResolutionSpec (Pass.In  Pass.Attrs) = '[Root, ConsResolver, UniqueNameGen]
    ConsResolutionSpec (Pass.Out Pass.Attrs) = '[UniqueNameGen]
    ConsResolutionSpec t = Pass.BasicPassSpec t

instance Pass.Definition TC.Stage ConsResolution where
    definition = do
        Root root <- Attr.get
        resolveConstructors True root

resolveConstructors :: Bool -> IR.SomeTerm -> TC.Pass ConsResolution ()
resolveConstructors positive root = Layer.read @IR.Model root >>= \case
    Uni.Cons n as -> do
        args <- traverse IR.source =<< ComponentVector.toList as
        traverse (resolveConstructors positive) args
        resol <- Attr.get @ConsResolver
        let resolution = resolve n resol
        case resolution of
            Resolved consRef -> do
                new <- buildResolvedCons positive
                                         (Layout.unsafeRelayout root)
                                         consRef
                IR.replace new root
            Ambiguous conses ->
                Error.setError (Just $ Error.consAmbiguous n conses) root
            _ -> Error.setError (Just $ Error.consNotFound n) root
    Uni.Unify l r -> do
        resolveConstructors False =<< IR.source l
        resolveConstructors True  =<< IR.source r
    Uni.Lam i o -> do
        resolveConstructors False =<< IR.source i
        resolveConstructors True  =<< IR.source o
    Uni.Function _ as o -> do
        args <- traverse IR.source =<< ComponentVector.toList as
        traverse_ (resolveConstructors False) args
        resolveConstructors True =<< IR.source o
    _ -> do
        inps <- IR.inputs root
        ComponentList.mapM_ (resolveConstructors positive <=< IR.source) inps

buildResolvedCons :: Pass.Interface ConsResolution m
                  => Bool -> IR.Term IR.Cons -> ConsRef -> m IR.SomeTerm
buildResolvedCons positive term (ConsRef unit cls cons) = do
    IR.Cons n as <- IR.modelView term
    if positive then do
        let arity = cons ^. Class.arity
        args <- sequence $ take arity $ repeat (IR.var =<< NameGen.generateName)
        newCons <- IR.resolvedCons' unit cls n args
        foldM (flip IR.lam') newCons $ reverse args
    else do
        args <- traverse IR.source =<< ComponentVector.toList as
        IR.resolvedCons' unit cls n args

