{-# LANGUAGE OverloadedStrings #-}

module Luna.Pass.Resolution.ConstructorResolution where

import Luna.Prelude hiding (String, Constructor)
import Luna.Builtin.Data.Function as Function
import Luna.Builtin.Data.Class    as Class
import Luna.Builtin.Data.Module   as Module
import qualified Luna.IR.Expr     as Named
import Luna.IR
import OCI.IR.Combinators
import OCI.IR.Name                (Name)
import qualified OCI.IR.Class      as Event
import qualified Data.Map          as Map
import           Data.Map          (Map)
import           Data.Maybe        (isJust)
import           OCI.Pass        (Pass, SubPass, Inputs, Outputs, Preserves, Events)
import qualified OCI.Pass        as Pass
import           Luna.Pass.Resolution.Data.UnresolvedConses
import           Luna.Pass.Data.ExprMapping

data ImportError = SymbolNotFound
                 | SymbolAmbiguous [Name]

data ConstructorResolution
type instance Abstract  ConstructorResolution = ConstructorResolution

type instance Inputs     Net   ConstructorResolution = '[AnyExpr, AnyExprLink]
type instance Inputs     Layer ConstructorResolution = '[AnyExpr // Model, AnyExpr // Type, AnyExpr // Succs, AnyExpr // UID, AnyExprLink // Model, AnyExprLink // UID, AnyExpr // Errors]
type instance Inputs     Attr  ConstructorResolution = '[UnresolvedConses, Imports, ExprMapping]
type instance Inputs     Event ConstructorResolution = '[]

type instance Outputs    Net   ConstructorResolution = '[AnyExpr, AnyExprLink]
type instance Outputs    Layer ConstructorResolution = '[AnyExpr // Model, AnyExpr // Type, AnyExpr // Succs, AnyExpr // UID, AnyExprLink // Model, AnyExprLink // UID, AnyExpr // Errors]
type instance Outputs    Attr  ConstructorResolution = '[UnresolvedConses, ExprMapping]
type instance Outputs    Event ConstructorResolution = '[New // AnyExpr, New // AnyExprLink, Event.Import // AnyExpr, Event.Import // AnyExprLink, Delete // AnyExpr, Delete // AnyExprLink, OnDeepDelete // AnyExpr]

type instance Preserves        ConstructorResolution = '[]

runConstructorResolution :: (MonadRef m, MonadPassManager m) => Pass ConstructorResolution m
runConstructorResolution = do
    conses <- unwrap <$> getAttr @UnresolvedConses
    mapM_ resolveCons conses
    putAttr @UnresolvedConses $ UnresolvedConses []

lookupCons :: Name -> Imports -> Either ImportError Constructor
lookupCons n imps = case itoListOf (importedClasses .> itraversed <. documentedItem . Class.constructors . ix n . _1) imps of
    []       -> Left SymbolNotFound
    [(_, c)] -> Right c
    matches  -> Left . SymbolAmbiguous $ fst <$> matches

importErrorDoc :: Name -> ImportError -> Text
importErrorDoc n SymbolNotFound         = "Can't find constructor: " <> " " <> fromString (show n)
importErrorDoc n (SymbolAmbiguous mods) = "Constructor" <> " " <> fromString (show n) <> " " <> "is ambiguous." <> "\n" <> "It's exported by the following modules: " <> "\n" <> (foldl (\l r -> l <> "\n" <> r) "" $ fromString . show <$> mods)

resolveCons :: (MonadRef m, MonadPassManager m) => Expr Cons -> SubPass ConstructorResolution m ()
resolveCons c = do
    Named.Term (Named.Cons n fields) <- readTerm c
    res <- lookupCons n <$> getAttr @Imports
    case res of
        Left err   -> modifyLayer_ @Errors c (CompileError (importErrorDoc n err) [] [] :)
        Right cons -> do
            imported <- importConstructor $ cons ^. constructor
            args     <- mapM source fields
            newRoot  <- foldM (fmap generalize .: app) imported args
            substitute newRoot c
            deleteSubtree c
            modifyAttr_ @ExprMapping $ wrapped . at (unsafeGeneralize c) ?~ unsafeGeneralize newRoot
