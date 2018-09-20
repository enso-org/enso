{-# LANGUAGE NoStrict             #-}
{-# LANGUAGE NoStrictData         #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Pass.Resolve.AliasAnalysis where

import Prologue

import qualified Control.Monad.State.Layered      as State
import qualified Data.Graph.Data.Component.List   as ComponentList
import qualified Data.Graph.Data.Component.Vector as ComponentVector
import qualified Data.Graph.Data.Layer.Layout     as Layout
import qualified Data.Map                         as Map
import qualified Luna.IR                          as IR
import qualified Luna.IR.Aliases                  as Uni
import qualified Luna.IR.Layer                    as Layer
import qualified Luna.Pass                        as Pass
import qualified Luna.Pass.Attr                   as Attr
import qualified Luna.Pass.Data.Stage             as TC

import Data.Map                                   (Map)
import Luna.Pass.Data.Root                        (Root (Root))
import Luna.Pass.Resolve.Data.UnresolvedVariables (UnresolvedVariables)



--------------------------------
-- === AliasAnalysis Pass === --
--------------------------------

data AliasAnalysis

type instance Pass.Spec AliasAnalysis t = AliasAnalysisSpec t
type family AliasAnalysisSpec t where
    AliasAnalysisSpec (Pass.In  Pass.Attrs) = '[Root, UnresolvedVariables]
    AliasAnalysisSpec (Pass.Out Pass.Attrs) = '[UnresolvedVariables]
    AliasAnalysisSpec t = Pass.BasicPassSpec t

type AliasMap = Map IR.Name (IR.Term IR.Var)

instance Pass.Definition TC.Stage AliasAnalysis where
    definition = do
        Root root <- Attr.get
        void $ State.runT (resolveAliases root) def

reportUnknownVariable :: IR.Term IR.Var -> State.StateT AliasMap (TC.Pass AliasAnalysis) ()
reportUnknownVariable v = Attr.modify_ @UnresolvedVariables (wrapped %~ (v :))

discoverVars :: IR.SomeTerm -> State.StateT AliasMap (TC.Pass AliasAnalysis) AliasMap
discoverVars e = Layer.read @IR.Model e >>= \case
    Uni.Var n -> return $ Map.singleton n $ Layout.unsafeRelayout e
    _ -> do
        inps <- IR.inputs e
        Map.unions <$> ComponentList.mapM (discoverVars <=< IR.source) inps

resolveAliases :: IR.SomeTerm -> State.StateT AliasMap (TC.Pass AliasAnalysis) ()
resolveAliases expr = Layer.read @IR.Model expr >>= \case
    Uni.Var name -> do
        existingVar <- State.gets @AliasMap $ Map.lookup name
        case existingVar of
            Just v  -> IR.replace v expr
            Nothing -> reportUnknownVariable $ Layout.unsafeRelayout expr
    Uni.Unify l r -> do
        newVars <- discoverVars =<< IR.source l
        State.modify_ @AliasMap $ Map.union newVars
        resolveAliases =<< IR.source r
    Uni.Lam i o -> do
        newVars <- discoverVars =<< IR.source i
        aliases <- State.get @AliasMap
        out     <- IR.source o
        let localAliases = Map.union newVars aliases
        State.put @AliasMap localAliases
        resolveAliases out
        State.put @AliasMap aliases
    Uni.Function n as o -> do
        funVars <- discoverVars =<< IR.source n
        State.modify_ @AliasMap $ Map.union funVars
        out          <- IR.source o
        args         <- mapM IR.source =<< ComponentVector.toList as
        argAliases   <- Map.unions <$> traverse discoverVars args
        baseAliases  <- State.get @AliasMap
        let localAliases = Map.union argAliases baseAliases
        State.put @AliasMap localAliases
        resolveAliases out
        State.put @AliasMap baseAliases
    Uni.Acc t n -> do
        resolveAliases =<< IR.source t
    _ -> do
        inps <- ComponentList.mapM IR.source =<< IR.inputs expr
        mapM_ resolveAliases inps

