{-# LANGUAGE NoStrict     #-}
{-# LANGUAGE NoStrictData #-}

module Luna.Pass.Flow.ProcessUnits where

import Prologue

import qualified Control.Concurrent.Future             as Future
import qualified Data.Graph.Data.Graph.Class           as Graph
import qualified Data.Graph.Data.Layer.Layout          as Layout
import qualified Data.Map                              as Map
import qualified Luna.IR                               as IR
import qualified Luna.Pass.Data.Stage                  as TC
import qualified Luna.Pass.Evaluation.Interpreter      as Interpreter
import qualified Luna.Pass.Typing.HeaderBuilder        as Serializer
import qualified Luna.Pass.Preprocess.PreprocessDef    as PreprocessDef
import qualified Luna.Pass.Resolve.ConsFieldResolution as ConsFieldResolution
import qualified Luna.Pass.Scheduler                   as Scheduler
import qualified Luna.Pass.Sourcing.Data.Unit          as Unit
import qualified Luna.Pass.Sourcing.Data.Class         as Class
import qualified Luna.Pass.Sourcing.Data.Def           as Def
import qualified Luna.Pass.Typing.Data.Target          as Target
import qualified Luna.Pass.Typing.Data.Typed           as Typed
import qualified Luna.Pass.Typing.ConsGeneration       as ConsGeneration
import qualified Luna.Pass.Typing.Typechecker          as Typechecker
import qualified Luna.Runtime                          as Runtime

import Data.Map (Map)
import Luna.Pass.Resolve.Data.Resolution (UnitResolver)



-----------------
-- === API === --
-----------------

processFunBody
    :: Target.Target
    -> UnitResolver
    -> Typed.Units
    -> Runtime.Units
    -> Serializer.GraphCopy
    -> TC.Monad (Typed.DefHeader, Runtime.Value)
processFunBody tgt resolver typedUnits evaluatedUnits copy = do
    fun <- Serializer.deserialize copy
    PreprocessDef.preprocessDef resolver  fun
    hdr <- Typechecker.runTypechecker tgt fun typedUnits
    val <- Interpreter.runInterpreter     fun evaluatedUnits
    pure (hdr, val)

processFun :: Target.Target
           -> UnitResolver
           -> Typed.Units
           -> Runtime.Units
           -> Def.Def
           -> TC.Monad (Future.Future (Typed.DefHeader, Runtime.Value))
processFun tgt resolver typedUnits evaluatedUnits def = do
    case def of
        Def.Body fun -> do
            copy <- Serializer.serialize $ Layout.relayout fun
            st  <- Graph.getState @TC.Stage
            Future.make $ flip (Graph.eval @TC.Stage) st
                            $ Scheduler.evalT
                                $ processFunBody tgt
                                                 resolver
                                                 typedUnits
                                                 evaluatedUnits
                                                 copy
        Def.Precompiled precompiled -> do
            pure $ pure $ ( wrap $ Right $ precompiled ^. Def.header
                          , precompiled ^. Def.value $ evaluatedUnits
                          )

processDefsMap
    :: (IR.Name -> Target.Target)
    -> UnitResolver
    -> Typed.Units
    -> Runtime.Units
    -> Def.DefsMap
    -> TC.Monad ( Map IR.Name Typed.Def
                , Map IR.Name (Future.Future Runtime.Value))
processDefsMap tgtCons resolver typedUnits evaluatedUnits defsMap = do
    let defs = unwrap defsMap
    zipped <- flip Map.traverseWithKey defs $ \defName docDef -> do
        let def = docDef ^. Def.documented
            tgt = tgtCons defName
        pfun <- processFun tgt resolver typedUnits evaluatedUnits def
        pure (wrap $ fst <$> pfun, snd <$> pfun)
    pure (fst <$> zipped, snd <$> zipped)

processUnits
    :: Typed.Units
    -> Runtime.Units
    -> Map IR.Qualified (UnitResolver, Unit.Unit)
    -> TC.Monad (Typed.Units, Runtime.Units)
processUnits initialTyped initialEvaluated units = mfix $ \us -> do
    let typedUnits     = fst us
        evaluatedUnits = snd us
    processedUnits <- flip Map.traverseWithKey units $ \unitName (resolver, unit) -> do
        let Unit.Unit defs classes = unit
        (tDefs, eDefs) <- processDefsMap (Target.Function unitName)
                                         resolver
                                         typedUnits
                                         evaluatedUnits
                                         defs
        processedClasses <- flip Map.traverseWithKey classes $ \clsName docCls -> do
            let cls   = docCls ^. Def.documented
                meths = cls ^. Class.methods
                root  = cls ^. Class.root
            ConsFieldResolution.run resolver root
            conses <- ConsGeneration.run unitName $ Layout.relayout root
            (tMeths, eMeths) <- processDefsMap (Target.Method unitName clsName)
                                               resolver
                                               typedUnits
                                               evaluatedUnits
                                               meths
            pure (Typed.Class tMeths conses, Runtime.Class eMeths)
        pure ( Typed.Unit   tDefs (fst <$> processedClasses)
             , Runtime.Unit eDefs (snd <$> processedClasses))
    let typed = Map.union (fst <$> processedUnits) (unwrap initialTyped)
        evald = Map.union (snd <$> processedUnits) (unwrap initialEvaluated)
    pure (wrap typed, wrap evald)

