module Luna.Pass.Resolution.AliasAnalysis where

import           OCI.Pass        (SubPass, Pass, Inputs, Outputs, Preserves)

import Luna.Prelude hiding (String, s, new, cons)
import qualified Luna.Prelude as P
import qualified OCI.IR.Repr.Vis as Vis
import Data.TypeDesc
import OCI.IR.Combinators
import Luna.IR hiding (expr)
import Luna.Pass.Data.ExprRoots
import Luna.Pass.Resolution.Data.UnresolvedVars
import Luna.Pass.Resolution.Data.UnresolvedConses
import OCI.Pass.Manager

import Data.TypeDesc

import qualified Data.Map   as Map
import           Data.Map   (Map)

import System.Log

data AliasAnalysis
type instance Abstract         AliasAnalysis = AliasAnalysis
type instance Inputs     Net   AliasAnalysis = '[AnyExpr, AnyExprLink]
type instance Inputs     Layer AliasAnalysis = '[AnyExpr // Model, AnyExprLink // Model, AnyExpr // Type, AnyExpr // Succs]
type instance Inputs     Attr  AliasAnalysis = '[UnresolvedVars, UnresolvedConses, NegativeConses, ExprRoots]
type instance Inputs     Event AliasAnalysis = '[]

type instance Outputs    Net   AliasAnalysis = '[AnyExpr, AnyExprLink]
type instance Outputs    Layer AliasAnalysis = '[AnyExpr // Model,  AnyExprLink // Model, AnyExpr // Type, AnyExpr // Succs]
type instance Outputs    Attr  AliasAnalysis = '[UnresolvedVars, UnresolvedConses, NegativeConses]
type instance Outputs    Event AliasAnalysis = '[New // AnyExpr, New // AnyExprLink, Delete // AnyExpr, Delete // AnyExprLink, OnDeepDelete // AnyExpr]

type instance Preserves        AliasAnalysis = '[]

reportUnknownVariable :: (MonadRef m, MonadPassManager m) => Expr Var -> SubPass AliasAnalysis m ()
reportUnknownVariable v = modifyAttr_ @UnresolvedVars $ wrap . (v :) . unwrap

reportPositiveCons :: (MonadRef m, MonadPassManager m) => Expr Cons -> SubPass AliasAnalysis m ()
reportPositiveCons c = modifyAttr_ @UnresolvedConses $ wrap . (c :) . unwrap

reportNegativeCons :: (MonadRef m, MonadPassManager m) => Expr Cons -> SubPass AliasAnalysis m ()
reportNegativeCons c = modifyAttr_ @NegativeConses $ wrap . (c :) . unwrap

runAliasAnalysis :: (MonadRef m, MonadPassManager m) => Pass AliasAnalysis m
runAliasAnalysis = do
    roots       <- unwrap <$> getAttr @ExprRoots
    mapM_ (resolveAliases def) roots

discoverVars :: (MonadRef m, MonadPassManager m) => Expr Draft -> SubPass AliasAnalysis m [(Name, Expr Var)]
discoverVars e = matchExpr e $ \case
    Cons    _ args -> do
        reportNegativeCons $ unsafeRelayout e
        concat <$> mapM (source >=> discoverVars) args
    Var     n      -> return [(n, unsafeRelayout e)]
    Grouped x      -> discoverVars =<< source x
    _              -> return []

resolveAliases :: (MonadRef m, MonadPassManager m) => Map Name (Expr Var) -> Expr Draft -> SubPass AliasAnalysis m (Map Name (Expr Var))
resolveAliases baseAliases expr = matchExpr expr $ \case
    Cons name children -> do
        reportPositiveCons $ unsafeRelayout expr
        mapM_ (resolveAliases baseAliases <=< source) children
        return baseAliases
    Var name -> do
        case Map.lookup name baseAliases of
            Just v  -> replace v expr
            Nothing -> reportUnknownVariable (unsafeRelayout expr)
        return baseAliases
    Unify l r -> do
        newVars <- discoverVars =<< source l
        let newAliases = Map.union (Map.fromList newVars) baseAliases
        resolveAliases newAliases =<< source r
        return newAliases
    Marked _ b -> resolveAliases baseAliases =<< source b
    Lam i o -> do
        newVars <- discoverVars =<< source i
        let newAliases = Map.union (Map.fromList newVars) baseAliases
        resolveAliases newAliases =<< source o
        return baseAliases
    ASGFunction n as b -> do
        selfVar <- Map.fromList <$> (discoverVars =<< source n)
        newVars <- Map.fromList . concat <$> mapM (discoverVars <=< source) as
        resolveAliases (Map.unions [selfVar, newVars, baseAliases]) =<< source b
        return $ Map.union selfVar baseAliases
    Seq l r -> do
        leftRes <- resolveAliases baseAliases =<< source l
        resolveAliases leftRes =<< source r
    _ -> do
        inps <- inputs expr >>= mapM source
        mapM_ (resolveAliases baseAliases) inps
        return baseAliases
