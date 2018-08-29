{-# LANGUAGE NoStrict     #-}
{-# LANGUAGE NoStrictData #-}

module Luna.Pass.Evaluation.EvaluateUnits where

import Prologue

import qualified Data.Map                           as Map
import qualified Luna.IR                            as IR
import qualified Luna.Pass.Data.Stage               as TC
import qualified Luna.Pass.Evaluation.Interpreter   as Interpreter
import qualified Luna.Pass.Scheduler                as Scheduler
import qualified Luna.Pass.Sourcing.Data.Unit       as Unit
import qualified Luna.Pass.Sourcing.Data.Class      as Class
import qualified Luna.Pass.Sourcing.Data.Def        as Def
import qualified Luna.Runtime                       as Runtime
import qualified Control.Concurrent.Future          as Future
import qualified Data.Graph.Data.Graph.Class        as Graph
import qualified Data.Graph.Data.Layer.Layout       as Layout

import Data.Map (Map)

evaluateFun :: Runtime.Units -> Def.Def -> TC.Monad (Future.Future Runtime.Value)
evaluateFun compiledUnits def = case def of
    Def.Body fun -> do
        let int = Interpreter.runInterpreter (Layout.relayout fun)
                                             compiledUnits
        async <- Graph.async @TC.Stage $ Scheduler.evalT int
        Future.fromAsync async
    Def.Precompiled precompiled ->
        pure $ pure $ (precompiled ^. Def.value) compiledUnits

evaluateUnits :: Map IR.Qualified Unit.Unit -> TC.Monad Runtime.Units
evaluateUnits units = mfix $ \compiledUnits -> do
    evaluatedUnits <- for units $ \unit -> do
        let Unit.Unit (Def.DefsMap defs) classes = unit
        evaluatedDefs <- for defs $ \(Def.Documented _ def) ->
            evaluateFun compiledUnits def
        evaluatedClasses <- for classes $ \cls -> do
            let meths = cls ^. Def.documented . Class.methods . wrapped
            evaluatedMeths <- for meths $ \(Def.Documented _ def) ->
                evaluateFun compiledUnits def
            return $ Runtime.Class evaluatedMeths
        return $ Runtime.Unit evaluatedDefs evaluatedClasses
    return $ Runtime.Units evaluatedUnits

