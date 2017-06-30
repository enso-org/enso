module Luna.Pass.Transform.Desugaring.PatternTransformation where

import           OCI.Pass        (SubPass, Pass, Inputs, Outputs, Preserves)

import Luna.Prelude hiding (String, s, new, cons)
import qualified Luna.Prelude as P
import qualified OCI.IR.Repr.Vis as Vis
import Data.TypeDesc
import OCI.IR.Combinators
import Luna.IR hiding (expr)
import Luna.Pass.Data.ExprRoots
import OCI.Pass.Manager

import Data.TypeDesc

import qualified Data.Map   as Map
import           Data.Map   (Map)

import System.Log

data PatternTransformation
type instance Abstract         PatternTransformation = PatternTransformation
type instance Inputs     Net   PatternTransformation = '[AnyExpr, AnyExprLink]
type instance Inputs     Layer PatternTransformation = '[AnyExpr // Model, AnyExprLink // Model, AnyExpr // Type, AnyExpr // Succs]
type instance Inputs     Attr  PatternTransformation = '[ExprRoots]
type instance Inputs     Event PatternTransformation = '[]

type instance Outputs    Net   PatternTransformation = '[AnyExpr, AnyExprLink]
type instance Outputs    Layer PatternTransformation = '[AnyExpr // Model,  AnyExprLink // Model, AnyExpr // Type, AnyExpr // Succs]
type instance Outputs    Attr  PatternTransformation = '[]
type instance Outputs    Event PatternTransformation = '[New // AnyExpr, New // AnyExprLink, Delete // AnyExpr, Delete // AnyExprLink, OnDeepDelete // AnyExpr]
type instance Preserves        PatternTransformation = '[]

runPatternTransformation :: (MonadRef m, MonadPassManager m) => Pass PatternTransformation m
runPatternTransformation = do
    roots <- unwrap <$> getAttr @ExprRoots
    mapM_ transformPatterns roots

dumpConsApplication :: (MonadRef m, MonadPassManager m) => Expr Draft -> SubPass PatternTransformation m (Name, [Expr Draft])
dumpConsApplication expr = matchExpr expr $ \case
    Grouped g -> dumpConsApplication =<< source g
    Cons n _  -> return (n, [])
    App f a   -> do
        (n, args) <- dumpConsApplication =<< source f
        arg <- source a
        return (n, arg : args)

flattenPattern :: (MonadRef m, MonadPassManager m) => Expr Draft -> SubPass PatternTransformation m (Expr Draft)
flattenPattern expr = matchExpr expr $ \case
    Grouped g -> flattenPattern =<< source g
    Var{}     -> return expr
    Cons{}    -> return expr
    App{}     -> do
        (name, children) <- dumpConsApplication expr
        flatChildren     <- mapM flattenPattern $ reverse children
        generalize <$> cons name flatChildren
    _         -> return expr

transformPatterns :: (MonadRef m, MonadPassManager m) => Expr Draft -> SubPass PatternTransformation m ()
transformPatterns expr = matchExpr expr $ \case
    Lam i o   -> do
        inp <- source i
        res <- flattenPattern inp
        replace res inp
        transformPatterns =<< source o
    Unify l r -> do
        left <- source l
        res  <- flattenPattern left
        replace res left
        transformPatterns =<< source r
    _ -> mapM_ (transformPatterns <=< source) =<< inputs expr
