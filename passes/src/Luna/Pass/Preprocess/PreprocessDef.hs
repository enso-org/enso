{-# LANGUAGE NoStrict     #-}
{-# LANGUAGE NoStrictData #-}

module Luna.Pass.Preprocess.PreprocessDef where

import Prologue

import qualified Luna.Pass.Data.Stage                as TC
import qualified Luna.IR                             as IR
import qualified Luna.Pass.Resolve.Data.Resolution   as Resolution
import qualified Luna.Pass.Scheduler                 as Scheduler

import Luna.Pass.Data.Root          (Root (..))
import Luna.Pass.Data.UniqueNameGen (UniqueNameGen)
import Luna.Pass.Resolve.Data.Resolution (DefResolver, ConsResolver, UnitResolver)
import Luna.Pass.Resolve.Data.UnresolvedVariables (UnresolvedVariables)

import Luna.Pass.Resolve.AliasAnalysis                        (AliasAnalysis)
import Luna.Pass.Resolve.DefResolution                        (DefResolution)
import Luna.Pass.Resolve.ConsResolution                       (ConsResolution)
import Luna.Pass.Transform.Desugar.DesugarListLiterals        (DesugarListLiterals)
import Luna.Pass.Transform.Desugar.DesugarPartialApplications (DesugarPartialApplications)
import Luna.Pass.Transform.Desugar.DesugarFieldModifiers      (DesugarFieldModifiers)
import Luna.Pass.Transform.Desugar.RemoveGrouped              (RemoveGrouped)
import Luna.Pass.Transform.Desugar.TransformPatterns          (TransformPatterns)

preprocessDef :: UnitResolver -> IR.SomeTerm -> TC.Monad ()
preprocessDef resolver root = do
    Scheduler.registerPass @TC.Stage @DesugarListLiterals
    Scheduler.registerPass @TC.Stage @DesugarFieldModifiers
    Scheduler.registerPass @TC.Stage @DesugarPartialApplications
    Scheduler.registerPass @TC.Stage @RemoveGrouped
    Scheduler.registerPass @TC.Stage @TransformPatterns
    Scheduler.registerPass @TC.Stage @AliasAnalysis
    Scheduler.registerPass @TC.Stage @DefResolution
    Scheduler.registerPass @TC.Stage @ConsResolution

    Scheduler.registerAttr @UniqueNameGen
    Scheduler.registerAttr @UnresolvedVariables
    Scheduler.registerAttr @DefResolver
    Scheduler.registerAttr @ConsResolver
    Scheduler.registerAttr @Root

    Scheduler.enableAttrByType @UniqueNameGen
    Scheduler.enableAttrByType @UnresolvedVariables
    Scheduler.setAttr $ Root root
    Scheduler.setAttr $ resolver ^. Resolution.defs
    Scheduler.setAttr $ resolver ^. Resolution.conses

    Scheduler.runPassByType @DesugarListLiterals
    Scheduler.runPassByType @DesugarFieldModifiers
    Scheduler.runPassByType @DesugarPartialApplications
    Scheduler.runPassByType @RemoveGrouped
    Scheduler.runPassByType @TransformPatterns
    Scheduler.runPassByType @AliasAnalysis
    Scheduler.runPassByType @DefResolution
    Scheduler.runPassByType @ConsResolution

