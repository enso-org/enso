module Luna.Pass.Preprocess.PreprocessUnit where

import Prologue

import qualified Control.Concurrent.Async           as Async
import qualified Data.Map                           as Map
import qualified Luna.IR                            as IR
import qualified Luna.Pass.Preprocess.PreprocessDef as PreprocessDef
import qualified Luna.Pass.Scheduler                as Scheduler
import qualified Luna.Pass.Sourcing.Data.Unit       as Unit
import qualified Luna.Pass.Sourcing.Data.Class      as Class
import qualified Luna.Pass.Sourcing.Data.Def        as Def
import qualified Data.Graph.Data.Graph.Class        as Graph

import Luna.Pass.Resolve.Data.Resolution (UnitResolver)

waitAll :: MonadIO m => [Async.Async ()] -> m ()
waitAll = liftIO . foldl (\r async -> Async.wait async >> r) (return ())

asyncPreprocessDef :: forall stage m.
    ( PreprocessDef.Ctx stage m
    ) => UnitResolver -> Def.Def -> m ()--m (Maybe (Async.Async ()))
asyncPreprocessDef resolver def = case def of
    Def.Body fun -> do
        let prep = PreprocessDef.preprocessDef @stage resolver fun
        res <- Graph.local @stage $ void $ Scheduler.evalT $ prep
        {-return $ Just res-}
        return ()

    _ -> return ()--Nothing

asyncPreprocessDefs :: forall stage m.
    ( PreprocessDef.Ctx stage m
    ) => UnitResolver -> [Def.Def] -> m () -- m [Async.Async ()]
asyncPreprocessDefs resolver defs = do
    {-fmap catMaybes $ -}
    for defs $ asyncPreprocessDef @stage resolver
    return ()

preprocessUnit :: forall stage m.
    ( PreprocessDef.Ctx stage m
    ) => UnitResolver -> Unit.Unit -> m ()
preprocessUnit resolver (Unit.Unit (Def.DefsMap defs) classes) = do
    defAsyncs <- asyncPreprocessDefs @stage resolver $
                     view Def.documented <$> Map.elems defs
    clsAsyncs <- for (Map.elems classes) $ \(Def.Documented _ cls) -> do
        let meths = cls ^. Class.methods . wrapped
        asyncPreprocessDefs @stage resolver $
            view Def.documented <$> Map.elems meths
    {-let asyncs = concat $ defAsyncs : clsAsyncs-}
    {-waitAll asyncs-}
    return ()
