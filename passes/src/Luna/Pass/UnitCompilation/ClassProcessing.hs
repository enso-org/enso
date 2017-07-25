{-# LANGUAGE OverloadedStrings #-}

module Luna.Pass.UnitCompilation.ClassProcessing where

import Luna.Prelude hiding (String, s, new, Constructor, Destructor, cons)
import           OCI.Pass         (SubPass, Pass)
import qualified OCI.Pass         as Pass
import qualified OCI.Pass.Manager as Pass
import qualified Luna.IR.Expr     as Term
import OCI.IR.Combinators
import Luna.Builtin.Data.Class    as Class
import Luna.Builtin.Data.Module   as Module
import Luna.Builtin.Data.Function (compile)
import Luna.IR
import Control.Monad.State.Dependent
import Control.Monad.Raise
import Luna.Pass.Data.UniqueNameGen
import qualified Luna.Pass.UnitCompilation.RecordProcessing as RecordProcessing
import qualified Luna.Pass.UnitCompilation.MethodProcessing as MethodProcessing

import           Data.Map (Map)
import qualified Data.Map as Map

import Data.Future (delay)

data ClassProcessing
type instance Abstract   ClassProcessing = ClassProcessing
type instance Pass.Inputs     Net   ClassProcessing = '[AnyExpr, AnyExprLink]
type instance Pass.Inputs     Layer ClassProcessing = '[AnyExpr // Model, AnyExpr // Succs, AnyExpr // Type, AnyExprLink // Model]
type instance Pass.Inputs     Attr  ClassProcessing = '[]
type instance Pass.Inputs     Event ClassProcessing = '[]

type instance Pass.Outputs    Net   ClassProcessing = '[AnyExpr, AnyExprLink]
type instance Pass.Outputs    Layer ClassProcessing = '[AnyExpr // Model, AnyExpr // Type, AnyExprLink // Model, AnyExpr // Succs]
type instance Pass.Outputs    Attr  ClassProcessing = '[]
type instance Pass.Outputs    Event ClassProcessing = '[New // AnyExpr, Delete // AnyExpr, Delete // AnyExprLink, New // AnyExprLink, OnDeepDelete // AnyExpr]

type instance Pass.Preserves        ClassProcessing = '[]

processClass :: (MonadPassManager m, MonadIO m) => Imports -> Expr ClsASG -> m Class
processClass imports root = do
    (className, paramNames, records, methods) <- Pass.eval' @ClassProcessing $ do
        Term (Term.ClsASG name ps cs ds) <- readTerm root
        params <- mapM source ps
        paramNames <- forM params $ \p -> matchExpr p $ \case
            Var n -> return n
            _     -> error "Unexpected class parameter type"
        decls <- mapM source ds
        resolvedDecls <- fmap catMaybes $ forM decls $ \decl -> matchExpr decl $ \case
            ASGRootedFunction n r -> do
                n'   <- source n
                name <- matchExpr n' $ \(Var n) -> return n
                return $ Just (name, r)
            _                     -> return $ Nothing
        conses <- mapM source cs
        resolvedConses <- fmap catMaybes $ forM conses $ \cons -> matchExpr cons $ \case
            RecASG n _ -> return $ Just (n, unsafeGeneralize cons)
            _          -> return Nothing
        return (name, paramNames, resolvedConses, resolvedDecls)
    compiledRecords <- mapM (RecordProcessing.processRecord className paramNames . snd) records
    let recordsMap = Map.fromList $ zip (fst <$> records) compiledRecords
        bareClass  = Class recordsMap Map.empty
        imps       = imports & importedClasses . at className ?~ bareClass
    methodMap <- MethodProcessing.processMethods imps className paramNames (fst <$> records) methods
    return $ Class recordsMap methodMap
