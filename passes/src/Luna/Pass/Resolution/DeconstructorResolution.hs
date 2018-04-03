{-# LANGUAGE OverloadedStrings #-}

module Luna.Pass.Resolution.DeconstructorResolution where

import Luna.Prelude hiding (String, Destructor)
import Control.Monad.State.Dependent (MonadGetter, MonadSetter)
import Control.Monad.Except          (ExceptT, runExceptT, throwError)
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
import           Luna.Pass.Resolution.Data.ImportError ( ImportError (..)
                                                       , consImportErrorDoc)
import           Luna.Pass.Inference.Data.Unifications

data DeconstructorResolution
type instance Abstract  DeconstructorResolution = DeconstructorResolution

type instance Inputs     Net   DeconstructorResolution = '[AnyExpr, AnyExprLink]
type instance Inputs     Layer DeconstructorResolution = '[ AnyExpr // Model
                                                          , AnyExpr // Type
                                                          , AnyExpr // Succs
                                                          , AnyExpr // UID
                                                          , AnyExpr // Requester
                                                          , AnyExpr // Errors
                                                          , AnyExprLink // Model
                                                          , AnyExprLink // UID
                                                          ]
type instance Inputs     Attr  DeconstructorResolution = '[ NegativeConses
                                                          , Imports
                                                          , Unifications]
type instance Inputs     Event DeconstructorResolution = '[]

type instance Outputs    Net   DeconstructorResolution = '[AnyExpr, AnyExprLink]
type instance Outputs    Layer DeconstructorResolution = '[ AnyExpr // Model
                                                          , AnyExpr // Type
                                                          , AnyExpr // Succs
                                                          , AnyExpr // UID
                                                          , AnyExpr // Requester
                                                          , AnyExpr // Errors
                                                          , AnyExprLink // Model
                                                          , AnyExprLink // UID
                                                          ]
type instance Outputs    Attr  DeconstructorResolution = '[ NegativeConses
                                                          , Unifications
                                                          ]
type instance Outputs    Event DeconstructorResolution = '[ New // AnyExpr
                                                          , New // AnyExprLink
                                                          , Event.Import // AnyExpr
                                                          , Event.Import // AnyExprLink
                                                          , Delete // AnyExpr
                                                          , Delete // AnyExprLink
                                                          , OnDeepDelete // AnyExpr]

type instance Preserves        DeconstructorResolution = '[]

instance MonadGetter s m => MonadGetter s (ExceptT r m)
instance MonadSetter s m => MonadSetter s (ExceptT r m)

runDeconstructorResolution :: (MonadRef m, MonadPassManager m) => Pass DeconstructorResolution m
runDeconstructorResolution = do
    conses <- unwrap <$> getAttr @NegativeConses
    forM_ conses $ \cons -> do
        res <- resolveDeconstructor cons
        case res of
            Left err -> modifyLayer_ @Errors cons (CompileError err [] [] :)
            _        -> return ()
    putAttr @NegativeConses $ NegativeConses []

wrongArgumentsCountError :: Int -> Int -> Name -> Text
wrongArgumentsCountError expected given cons
    =  "Wrong number of arguments for constructor "
    <> fromString (show cons)
    <> ". Expected "
    <> convert (show expected)
    <> " but got "
    <> convert (show given)
    <> "."

lookupDestr :: Name -> Imports -> Either ImportError Destructor
lookupDestr n imps = case itoListOf (importedClasses .> itraversed <. Function.documentedItem . Class.constructors . ix n) imps of
    []       -> Left SymbolNotFound
    [(_, d)] -> Right $ snd d
    matches  -> Left . SymbolAmbiguous $ fst <$> matches

resolveDeconstructor :: (MonadRef m, MonadPassManager m) => Expr Cons
                     -> SubPass DeconstructorResolution m (Either Text ())
resolveDeconstructor c = do
    Named.Term (Named.Cons n fields) <- readTerm c
    res <- lookupDestr n <$> getAttr @Imports
    case res of
        Left err   -> return . Left $ consImportErrorDoc n err
        Right dest -> runExceptT $ do
            (root, argTypes) <- importDestructor dest
            let expectedArgsCount = length argTypes
                givenArgsCount    = length fields
            when (expectedArgsCount /= givenArgsCount) $
                throwError $ wrongArgumentsCountError expectedArgsCount
                                                      givenArgsCount
                                                      n
            rootTp           <- getLayer @Type c >>= source
            rootUni          <- unify rootTp root
            argTps           <- mapM (source >=> getLayer @Type >=> source) fields
            argUnis          <- zipWithM unify argTps argTypes
            let unis :: [Expr Unify] = unsafeRelayout rootUni : fmap unsafeRelayout argUnis
            modifyAttr_ @Unifications $ wrap . (unis ++) . unwrap
            forM_ unis $ reconnectLayer' @Requester (Just c)
            return ()
