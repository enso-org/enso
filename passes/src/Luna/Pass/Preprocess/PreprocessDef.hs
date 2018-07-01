module Luna.Pass.Preprocess.PreprocessDef where

import Prologue

import qualified Data.Graph.Data.Component.Vector    as ComponentVector
import qualified Data.Graph.Data.Layer.Layout        as Layout
import qualified Data.Map                            as Map
import qualified Luna.IR                             as IR
import qualified Luna.IR.Aliases                     as Uni
import qualified Luna.IR.Layer                       as Layer
import qualified Luna.Pass                           as Pass
import qualified Luna.Pass.Attr                      as Attr
import qualified Luna.Pass.Basic                     as Pass
import qualified Luna.Pass.Resolve.Data.Resolution   as Resolution
import qualified Luna.Pass.Scheduler                 as Scheduler
import qualified Luna.Pass.Sourcing.Utils            as Sourcing

import Data.Map (Map)
import Luna.Pass.Data.Root          (Root (..))
import Luna.Pass.Data.UniqueNameGen (UniqueNameGen)
import Luna.Pass.Resolve.Data.Resolution (DefResolver, ConsResolver, UnitResolver)
import Luna.Pass.Resolve.Data.UnresolvedVariables (UnresolvedVariables)

import Luna.Pass.Resolve.AliasAnalysis                        (AliasAnalysis)
import Luna.Pass.Resolve.DefResolution                        (DefResolution)
import Luna.Pass.Resolve.ConsResolution                       (ConsResolution)
import Luna.Pass.Transform.Desugar.DesugarListLiterals        (DesugarListLiterals)
import Luna.Pass.Transform.Desugar.DesugarPartialApplications (DesugarPartialApplications)
import Luna.Pass.Transform.Desugar.RemoveGrouped              (RemoveGrouped)
import Luna.Pass.Transform.Desugar.TransformPatterns          (TransformPatterns)

type Ctx stage m =
    ( Scheduler.PassRegister stage DesugarListLiterals        m
    , Scheduler.PassRegister stage DesugarPartialApplications m
    , Scheduler.PassRegister stage RemoveGrouped              m
    , Scheduler.PassRegister stage TransformPatterns          m
    , Scheduler.PassRegister stage AliasAnalysis              m
    , Scheduler.PassRegister stage DefResolution              m
    , Scheduler.PassRegister stage ConsResolution             m
    , Pass.Definition stage DesugarListLiterals
    , Pass.Definition stage DesugarPartialApplications
    , Pass.Definition stage RemoveGrouped
    , Pass.Definition stage TransformPatterns
    , Pass.Definition stage AliasAnalysis
    , Pass.Definition stage DefResolution
    , Pass.Definition stage ConsResolution
    )

preprocessDef :: forall stage m.
    ( Ctx stage m
    , Scheduler.MonadScheduler m
    ) => UnitResolver -> IR.Term IR.Function -> m ()
preprocessDef resolver root = do
    Scheduler.registerPass @stage @DesugarListLiterals
    Scheduler.registerPass @stage @DesugarPartialApplications
    Scheduler.registerPass @stage @RemoveGrouped
    Scheduler.registerPass @stage @TransformPatterns
    Scheduler.registerPass @stage @AliasAnalysis
    Scheduler.registerPass @stage @DefResolution
    Scheduler.registerPass @stage @ConsResolution

    Scheduler.registerAttr @UniqueNameGen
    Scheduler.registerAttr @UnresolvedVariables
    Scheduler.registerAttr @DefResolver
    Scheduler.registerAttr @ConsResolver
    Scheduler.registerAttr @Root

    Scheduler.enableAttrByType @UniqueNameGen
    Scheduler.enableAttrByType @UnresolvedVariables
    Scheduler.setAttr $ Root $ Layout.relayout root
    Scheduler.setAttr $ resolver ^. Resolution.defs
    Scheduler.setAttr $ resolver ^. Resolution.conses

    Scheduler.runPassByType @DesugarListLiterals
    Scheduler.runPassByType @DesugarPartialApplications
    Scheduler.runPassByType @RemoveGrouped
    Scheduler.runPassByType @TransformPatterns
    Scheduler.runPassByType @AliasAnalysis
    Scheduler.runPassByType @DefResolution
    Scheduler.runPassByType @ConsResolution
