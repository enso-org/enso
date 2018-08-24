{-# LANGUAGE NoStrict     #-}
{-# LANGUAGE NoStrictData #-}

module Luna.Pass.Preprocess.PreprocessUnit where

import Prologue

import qualified Control.Concurrent.Async              as Async
import qualified Data.Graph.Data.Layer.Layout          as Layout
import qualified Data.Map                              as Map
import qualified Luna.Pass.Preprocess.PreprocessDef    as PreprocessDef
import qualified Luna.Pass.Resolve.ConsFieldResolution as ConsFieldResolution
import qualified Luna.Pass.Scheduler                   as Scheduler
import qualified Luna.Pass.Sourcing.Data.Unit          as Unit
import qualified Luna.Pass.Sourcing.Data.Class         as Class
import qualified Luna.Pass.Sourcing.Data.Def           as Def
import qualified Data.Graph.Data.Graph.Class           as Graph
import qualified Luna.Pass.Data.Stage                  as TC

import Luna.Pass.Resolve.Data.Resolution (UnitResolver)

waitAll :: MonadIO m => [Async.Async ()] -> m ()
waitAll = liftIO . foldl (\r async -> Async.wait async >> r) (return ())

preprocessDef :: UnitResolver -> Def.Def -> TC.Monad ()
preprocessDef resolver def = case def of
    Def.Body fun -> do
        let prep = PreprocessDef.preprocessDef resolver $ Layout.relayout fun
        res <- Graph.local @TC.Stage $ void $ Scheduler.evalT $ prep
        return ()
    _ -> return ()

preprocessDefs :: UnitResolver -> [Def.Def] -> TC.Monad ()
preprocessDefs resolver defs = do
    for_ defs $ preprocessDef resolver
    return ()

preprocessUnit :: UnitResolver -> Unit.Unit -> TC.Monad ()
preprocessUnit resolver (Unit.Unit (Def.DefsMap defs) classes) = do
    preprocessDefs resolver $
                     view Def.documented <$> Map.elems defs
    for (Map.elems classes) $ \(Def.Documented _ cls) -> do
        let meths = cls ^. Class.methods . wrapped
        preprocessDefs resolver $
            view Def.documented <$> Map.elems meths
        ConsFieldResolution.run resolver $ cls ^. Class.root
    return ()

