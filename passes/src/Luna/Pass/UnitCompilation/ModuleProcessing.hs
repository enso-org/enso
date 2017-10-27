{-# LANGUAGE OverloadedStrings #-}

module Luna.Pass.UnitCompilation.ModuleProcessing where

import Luna.Prelude hiding         (String, s, new, Constructor, Destructor, cons)
import           Data.Text32       (Text32)
import           OCI.Pass          (SubPass, Pass)
import qualified OCI.Pass          as Pass
import qualified OCI.Pass.Manager  as Pass
import qualified Luna.IR.Expr      as Term
import qualified Luna.IR.Term.Unit as Term
import qualified Luna.IR.Term.Cls  as Term
import           Luna.IR.Term.Unit (ImportHub, Import)
import           Luna.IR.Term.Cls  (Cls)
import OCI.IR.Combinators
import Luna.Builtin.Data.Class    as Class
import Luna.Builtin.Data.Module   as Module
import Luna.Builtin.Data.Function (compile, documentation, WithDocumentation (..))
import Luna.IR
import Control.Monad.State.Dependent
import Control.Monad.Raise
import Luna.Pass.Data.UniqueNameGen
import qualified Luna.Pass.UnitCompilation.DefProcessing   as DefProcessing
import qualified Luna.Pass.UnitCompilation.ClassProcessing as ClassProcessing

import           Data.Map (Map)
import qualified Data.Map as Map
import Data.Future (delay, mkFuture)

data ModuleProcessing
type instance Abstract   ModuleProcessing = ModuleProcessing
type instance Pass.Inputs     Net   ModuleProcessing = '[AnyExpr, AnyExprLink]
type instance Pass.Inputs     Layer ModuleProcessing = '[AnyExpr // Model, AnyExpr // Succs, AnyExpr // Type, AnyExprLink // Model]
type instance Pass.Inputs     Attr  ModuleProcessing = '[]
type instance Pass.Inputs     Event ModuleProcessing = '[]

type instance Pass.Outputs    Net   ModuleProcessing = '[AnyExpr, AnyExprLink]
type instance Pass.Outputs    Layer ModuleProcessing = '[AnyExpr // Model, AnyExpr // Type, AnyExprLink // Model, AnyExpr // Succs]
type instance Pass.Outputs    Attr  ModuleProcessing = '[]
type instance Pass.Outputs    Event ModuleProcessing = '[New // AnyExpr, Delete // AnyExpr, Delete // AnyExprLink, New // AnyExprLink, OnDeepDelete // AnyExpr]

type instance Pass.Preserves        ModuleProcessing = '[]

cutDoc :: (MonadPassManager m, MonadIO m) => Expr Draft -> SubPass ModuleProcessing m (Expr Draft, Maybe Text32)
cutDoc e = matchExpr e $ \case
    Documented d a -> (,Just d) <$> source a
    _              -> return (e, Nothing)

processModule :: (MonadPassManager m, MonadIO m) => Imports -> Name -> Expr Unit -> m Imports
processModule imports modName root = do
    (definedClasses, definedFunctions) <- Pass.eval' @ModuleProcessing $ do
        Term (Term.Unit _ _ cls) <- readTerm root
        klass :: Expr Cls <- unsafeGeneralize <$> source cls
        Term (Term.Cls _ _ clss meths) <- readTerm klass
        definedClasses   <- fmap Map.toList $ forM clss  $ cutDoc <=< source
        definedFunctions <- fmap Map.toList $ forM meths $ \f -> do
            (fun', doc)  <- cutDoc =<< source f
            let fun :: Expr ASGRootedFunction = unsafeGeneralize fun'
            Term (Term.ASGRootedFunction n rooted) <- readTerm fun
            return (rooted, doc)
        return (definedClasses, definedFunctions)

    let processDef   imps (n, (rooted, doc)) = fmap (WithDocumentation doc) $ liftIO $ delay $ DefProcessing.processDef modName imps n rooted
        processClass imps (n, (cls, doc))    = WithDocumentation doc <$> ClassProcessing.processClass modName imps (unsafeGeneralize cls)

    mdo defs <- mapM (processDef   allImports) definedFunctions
        clss <- mapM (processClass allImports) definedClasses
        let classesByName   = Map.fromList $ zip (fst <$> definedClasses)   clss
            functionsByName = Map.fromList $ zip (fst <$> definedFunctions) defs
            allImports      = imports & importedClasses   %~ Map.union classesByName
                                      & importedFunctions %~ Map.union functionsByName
        return $ Imports classesByName functionsByName

