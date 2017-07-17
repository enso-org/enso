{-# LANGUAGE OverloadedStrings #-}

module Luna.Pass.UnitCompilation.ModuleProcessing where

import Luna.Prelude hiding         (String, s, new, Constructor, Destructor, cons)
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
import Luna.Builtin.Data.Function (compile)
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

processModule' :: (MonadPassManager m, MonadIO m) => Imports -> CompiledWorld -> Expr Unit -> m (CompiledWorld, Imports)
processModule' baseImports world root = do
    (imports, definedClasses, definedFunctions) <- Pass.eval' @ModuleProcessing $ do
        Term (Term.Unit imports _ cls) <- readTerm root
        importHub <- source imports
        imps <- matchExpr importHub $ \case
            ImportHub imps -> return imps
            _              -> return def
        let addImport name imports i = do
              imp :: Expr Import <- unsafeGeneralize <$> source i
              Term (Term.Import _ tgt') <- readTerm imp
              tgt <- source tgt'
              matchExpr tgt $ \case
                  RootedFunction {} -> return $ imports & importedFunctions . at name ?~ (world ^?! functions . ix (generalize tgt))
                  ClsASG         {} -> return $ imports & importedClasses   . at name ?~ (world ^?! classes   . ix (generalize tgt))
        imports <- ifoldlMOf itraversed addImport baseImports imps
        klass :: Expr Cls <- unsafeGeneralize <$> source cls
        Term (Term.Cls _ _ clss meths) <- readTerm klass
        definedClasses   <- fmap Map.toList $ forM clss  $ fmap unsafeGeneralize . source
        definedFunctions <- fmap Map.toList $ forM meths $ \f -> do
            fun :: Expr RootedFunction <- unsafeGeneralize <$> source f
            Term (Term.RootedFunction rooted) <- readTerm fun
            return (fun, rooted)
        return (imports, definedClasses, definedFunctions)

    let processDef   imps (n, (_, rooted)) = liftIO $ delay $ DefProcessing.processDef imps n rooted
        processClass imps (n, cls)         = ClassProcessing.processClass imps cls

    mdo defs <- mapM (processDef allImports)   definedFunctions
        clss <- mapM (processClass allImports) definedClasses
        let classesByName   = Map.fromList $ zip (fst <$> definedClasses)   clss
            classesByIR     = Map.fromList $ zip (generalize . snd <$> definedClasses)   clss
            functionsByName = Map.fromList $ zip (fst <$> definedFunctions) defs
            functionsByIR   = Map.fromList $ zip (generalize . fst . snd <$> definedFunctions) defs
            allImports      = imports & importedClasses   %~ Map.union classesByName
                                      & importedFunctions %~ Map.union functionsByName
        return (CompiledWorld classesByIR functionsByIR, Imports classesByName functionsByName)

processModule :: (MonadPassManager m, MonadIO m) => Imports -> CompiledWorld -> Expr Unit -> m CompiledWorld
processModule = fmap fst .:. processModule'
