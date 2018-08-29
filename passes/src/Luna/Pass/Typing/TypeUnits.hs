{-# LANGUAGE NoStrict     #-}
{-# LANGUAGE NoStrictData #-}

module Luna.Pass.Typing.TypeUnits where

import Prologue

import qualified Control.Concurrent.Future          as Future
import qualified Data.Graph.Data.Graph.Class        as Graph
import qualified Data.Graph.Data.Layer.Layout       as Layout
import qualified Data.Map                           as Map
import qualified Luna.IR                            as IR
import qualified Luna.Pass.Data.Error               as Error
import qualified Luna.Pass.Data.Stage               as TC
import qualified Luna.Pass.Scheduler                as Scheduler
import qualified Luna.Pass.Sourcing.Data.Unit       as Unit
import qualified Luna.Pass.Sourcing.Data.Class      as Class
import qualified Luna.Pass.Sourcing.Data.Def        as Def
import qualified Luna.Pass.Typing.Data.Target       as Target
import qualified Luna.Pass.Typing.Data.Typed        as Typed
import qualified Luna.Pass.Typing.ConsGeneration    as ConsGeneration
import qualified Luna.Pass.Typing.Typechecker       as Typechecker
import qualified Luna.Runtime                       as Runtime

import Data.Map (Map)

typeUnitsHC :: Map IR.Qualified Unit.Unit -> TC.Monad Typed.Units
typeUnitsHC = typeUnits

typeFun :: Target.Target -> Typed.Units -> Def.Def -> TC.Monad Typed.Def
typeFun tgt typedUnits def = case def of
    Def.Body fun -> do
        st <- Graph.getState @TC.Stage
        fut <- Future.make $ do
            {-print tgt-}
            Graph.eval @TC.Stage (Scheduler.evalT $ Typechecker.runTypechecker tgt (Layout.relayout fun) typedUnits) st
        pure $ Typed.Def $ fut
    Def.Precompiled precompiled ->
        pure $ Typed.Def $ pure $ wrap $ Right $ precompiled ^. Def.header

typeUnits :: Map IR.Qualified Unit.Unit -> TC.Monad Typed.Units
typeUnits units = mfix $ \typedUnits -> do
    processedUnits <- flip Map.traverseWithKey units $ \unitName unit -> do
        let Unit.Unit (Def.DefsMap defs) classes = unit
        processedDefs <- flip Map.traverseWithKey defs $ \defName (Def.Documented _ def) -> do
            let tgt = Target.Function unitName defName
            typeFun tgt typedUnits def
        processedClasses <- flip Map.traverseWithKey classes $ \clsName cls -> do
            let meths = cls ^. Def.documented . Class.methods . wrapped
                root  = cls ^. Def.documented . Class.root
            conses <- ConsGeneration.run unitName $ Layout.relayout root
            processedMeths <- flip Map.traverseWithKey meths $ \defName (Def.Documented _ def) -> do
                let tgt = Target.Method unitName clsName defName
                typeFun tgt typedUnits def
            return $ Typed.Class processedMeths conses
        return $ Typed.Unit processedDefs processedClasses
    return $ Typed.Units processedUnits

