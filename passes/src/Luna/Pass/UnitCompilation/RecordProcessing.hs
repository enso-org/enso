{-# LANGUAGE OverloadedStrings #-}

module Luna.Pass.UnitCompilation.RecordProcessing where

import Luna.Prelude hiding (String, s, new, Constructor, Destructor, cons)
import           OCI.Pass        (SubPass, Pass)
import qualified OCI.Pass         as Pass
import qualified OCI.Pass.Manager as Pass
import qualified Luna.IR.Expr     as Term
import OCI.IR.Combinators
import Luna.Builtin.Data.Class
import Luna.Builtin.Data.Function (compile)
import Luna.IR
import Control.Monad.State.Dependent
import Luna.Pass.Data.UniqueNameGen

import           Data.Map (Map)
import qualified Data.Map as Map

data RecordProcessing
type instance Abstract   RecordProcessing = RecordProcessing
type instance Pass.Inputs     Net   RecordProcessing = '[AnyExpr, AnyExprLink]
type instance Pass.Inputs     Layer RecordProcessing = '[AnyExpr // Model, AnyExpr // Succs, AnyExpr // Type, AnyExprLink // Model]
type instance Pass.Inputs     Attr  RecordProcessing = '[UniqueNameGen]
type instance Pass.Inputs     Event RecordProcessing = '[]

type instance Pass.Outputs    Net   RecordProcessing = '[AnyExpr, AnyExprLink]
type instance Pass.Outputs    Layer RecordProcessing = '[AnyExpr // Model, AnyExpr // Type, AnyExprLink // Model, AnyExpr // Succs]
type instance Pass.Outputs    Attr  RecordProcessing = '[UniqueNameGen]
type instance Pass.Outputs    Event RecordProcessing = '[New // AnyExpr, Delete // AnyExpr, Delete // AnyExprLink, New // AnyExprLink, OnDeepDelete // AnyExpr]

type instance Pass.Preserves        RecordProcessing = '[]

-- TODO[MK]: This way of handling things is highly temporary, until we have a nicer interface for groups / rooted IRs.
data LRec = LVar Name | LCons Name [LRec] deriving (Show, Eq)

processRecord :: (MonadPassManager m, MonadIO m, MonadState Cache m) => Name -> [Name] -> Expr RecASG -> m (Constructor, Destructor)
processRecord className classVars root = do
    (recordName, fields) <- Pass.eval' $ dumpRecordStructure root
    cons   <- Pass.evalWithFreshIR $ prepareConstructor   className classVars recordName fields
    decons <- Pass.evalWithFreshIR $ prepareDeconstructor className classVars fields
    return (cons, decons)

flattenApps :: MonadPassManager m => Expr Draft -> SubPass RecordProcessing m (Expr Draft, [Expr Draft])
flattenApps expr = matchExpr expr $ \case
    App f a -> do
        (func, rest) <- flattenApps =<< source f
        arg <- source a
        return (func, arg : rest)
    Grouped g -> flattenApps =<< source g
    _ -> return (expr, [])

getFieldRep :: MonadPassManager m => Expr Draft -> SubPass RecordProcessing m LRec
getFieldRep expr = matchExpr expr $ \case
    Var x -> return $ LVar x
    _     -> do
        (head, args) <- flattenApps expr
        matchExpr head $ \case
            Cons n _ -> LCons n . reverse <$> mapM getFieldRep args
            e        -> error $ "Expected a Cons, got: " ++ show e

dumpRecordStructure :: MonadPassManager m => Expr RecASG -> SubPass RecordProcessing m (Name, [LRec])
dumpRecordStructure record = do
    Term (Term.RecASG name fields) <- readTerm record
    fields <- forM fields $ \f -> do
        field :: Expr FieldASG   <- unsafeGeneralize <$> source f
        Term (Term.FieldASG _ t) <- readTerm field
        getFieldRep =<< source t
    return (name, fields)

rebuildFromStructure :: MonadPassManager m => Map Name (Expr Draft) -> LRec -> SubPass RecordProcessing m (Expr Draft)
rebuildFromStructure binds (LVar n)         = return $ fromMaybe (error "Unresolved variable in record field declaration") $ Map.lookup n binds
rebuildFromStructure binds (LCons n fields) = fmap generalize $ cons n =<< mapM (rebuildFromStructure binds) fields

flip3 :: (a -> b -> c -> d) -> b -> c -> a -> d
flip3 f b c a = f a b c

changeType :: MonadPassManager m => Expr Draft -> Expr Draft -> SubPass RecordProcessing m ()
changeType expr tp = do
    oldTp <- getLayer @Type expr >>= source
    reconnectLayer @Type tp expr
    deleteSubtree oldTp

prepareConstructor :: (MonadRef m, MonadPassManager m) => Name -> [Name] -> Name -> [LRec] -> SubPass RecordProcessing m Constructor
prepareConstructor className classVars recordName recordFields = do
    typeVars    <- mapM var classVars
    let typeVarMap = Map.fromList $ zip classVars (generalize <$> typeVars)

    fields                      <- mapM (const $ fmap generalize $ var =<< genName) recordFields
    fieldTypes                  <- mapM (rebuildFromStructure typeVarMap) recordFields
    fieldMonads :: [Expr Draft] <- mapM (const $ fmap generalize $ var =<< genName) fieldTypes
    fieldTypesInMonads          <- zipWithM (fmap generalize .: monadic) fieldTypes fieldMonads
    zipWithM_ changeType fields fieldTypesInMonads

    out                    <- generalize <$> cons recordName fields
    outType                <- cons className typeVars
    outMonad :: Expr Draft <- generalize <$> cons_ @Draft "Pure"
    outTypeInMonad         <- generalize <$> monadic outType outMonad

    fieldMakerTypes    <- flip3 foldM [outTypeInMonad] (reverse fieldTypesInMonads) $ \last@(res : ress) field -> do
        l <- lam field res
        p <- cons_ @Draft "Pure"
        m <- monadic l p
        return $ generalize m : last
    fieldMakers        <- flip3 foldM [out] (reverse fields) $ \last@(res : ress) field -> do
        l <- generalize <$> lam field res
        return $ l : last
    zipWithM_ changeType fieldMakers fieldMakerTypes

    fmap Constructor $ compile $ generalize $ head fieldMakers

prepareDeconstructor :: (MonadRef m, MonadPassManager m) => Name -> [Name] -> [LRec] -> SubPass RecordProcessing m Destructor
prepareDeconstructor className classVars recordFields = do
    typeVars           <- mapM var classVars
    wholeType          <- cons className typeVars
    let typeVarMap     =  Map.fromList $ zip classVars (generalize <$> typeVars)
    fieldTypes         <- mapM (rebuildFromStructure typeVarMap) recordFields
    monad              <- var =<< genName
    wholeTypeInMonad   <- monadic wholeType monad
    fieldTypesInMonads <- mapM (flip monadic monad) fieldTypes
    compiled           <- compile $ generalize wholeTypeInMonad
    return $ Destructor compiled $ generalize <$> fieldTypesInMonads
