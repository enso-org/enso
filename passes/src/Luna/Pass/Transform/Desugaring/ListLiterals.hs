{-# LANGUAGE OverloadedStrings #-}

module Luna.Pass.Transform.Desugaring.ListLiterals where

import           OCI.Pass        (SubPass, Pass)
import qualified OCI.Pass        as Pass
import Luna.Prelude hiding (String, s, new, List)
import qualified Luna.Prelude as P
import OCI.IR.Combinators
import Luna.IR
import Luna.Pass.Data.ExprRoots
import Luna.Pass.Data.UniqueNameGen

data DesugarLists
type instance Abstract   DesugarLists = DesugarLists
type instance Pass.Inputs     Net   DesugarLists = '[AnyExpr, AnyExprLink]
type instance Pass.Inputs     Layer DesugarLists = '[AnyExpr // Model, AnyExpr // Succs, AnyExpr // Type, AnyExprLink // Model]
type instance Pass.Inputs     Attr  DesugarLists = '[ExprRoots, UniqueNameGen]
type instance Pass.Inputs     Event DesugarLists = '[]

type instance Pass.Outputs    Net   DesugarLists = '[AnyExpr, AnyExprLink]
type instance Pass.Outputs    Layer DesugarLists = '[AnyExpr // Model, AnyExpr // Type, AnyExprLink // Model, AnyExpr // Succs]
type instance Pass.Outputs    Attr  DesugarLists = '[ExprRoots, UniqueNameGen]
type instance Pass.Outputs    Event DesugarLists = '[New // AnyExpr, Delete // AnyExpr, Delete // AnyExprLink, New // AnyExprLink, OnDeepDelete // AnyExpr]

type instance Pass.Preserves        DesugarLists = '[]

runDesugarLists :: (MonadRef m, MonadPassManager m) => Pass DesugarLists m
runDesugarLists = do
    roots    <- getAttr @ExprRoots
    newRoots <- forM (generalize <$> unwrap roots) (desugarLists False)
    putAttr @ExprRoots $ wrap $ unsafeGeneralize <$> newRoots

desugarLists :: (MonadRef m, MonadPassManager m) => Bool -> SomeExpr -> SubPass DesugarLists m SomeExpr
desugarLists isPattern e = do
    f <- inputs e
    matchExpr e $ \case
        List elts -> do
            mapM_ (desugarLists isPattern <=< source) =<< inputs e
            properList <- properListRep isPattern =<< (fmap Just <$> mapM source elts)
            replace properList e
            return  properList
        Tuple elts -> do
            mapM_ (desugarLists isPattern <=< source) =<< inputs e
            properTuple <- properTupleRep isPattern =<< (fmap Just <$> mapM source elts)
            replace properTuple e
            return  properTuple
        Unify l r -> do
            desugarLists True      =<< source l
            desugarLists isPattern =<< source r
            return e
        Lam i o -> do
            desugarLists True      =<< source i
            desugarLists isPattern =<< source o
            return e
        ASGFunction _ as b -> do
            mapM_ (desugarLists True <=< source) as
            desugarLists isPattern =<< source b
            return e
        _ -> do
            mapM_ (desugarLists isPattern <=< source) =<< inputs e
            return e

properListRep :: (MonadRef m, MonadPassManager m) => Bool -> [Maybe SomeExpr] -> SubPass DesugarLists m SomeExpr
properListRep isPattern elts = do
    (binds, elems) <- prepareBinders elts
    list           <- mkListOf elems
    if isPattern then return list else makeLams binds list

mkListOf :: (MonadRef m, MonadPassManager m) => [SomeExpr] -> SubPass DesugarLists m SomeExpr
mkListOf []         = cons'_ "Empty"
mkListOf (e : elts) = prepend e =<< mkListOf elts where
    prepend e rest = do
        prep  <- cons'_ "Prepend"
        withE <- app' prep e
        app' withE rest

properTupleRep :: (MonadRef m, MonadPassManager m) => Bool -> [Maybe SomeExpr] -> SubPass DesugarLists m SomeExpr
properTupleRep isPattern elts = do
    (binds, elems) <- prepareBinders elts
    tuple          <- mkTupleOf elems
    if isPattern then return tuple else makeLams binds tuple

mkTupleOf :: (MonadRef m, MonadPassManager m) => [SomeExpr] -> SubPass DesugarLists m SomeExpr
mkTupleOf elts = go elts where
    size     = length elts
    consName = convert $ "Tuple" <> show size
    go []         = cons'_ consName
    go (e : elts) = flip app' e =<< go elts

prepareBinders :: (MonadRef m, MonadPassManager m) => [Maybe SomeExpr] -> SubPass DesugarLists m ([SomeExpr], [SomeExpr])
prepareBinders []               = return ([], [])
prepareBinders (Just e : elts)  = matchExpr e $ \case
    Blank   -> prepareBinders $ Nothing : elts
    Missing -> prepareBinders $ Nothing : elts
    _     -> (_2 %~ (e:)) <$> prepareBinders elts
prepareBinders (Nothing : elts) = do
    (binds, es) <- prepareBinders elts
    v           <- var' =<< genName
    return (v : binds, v : es)

makeLams :: (MonadRef m, MonadPassManager m) => [SomeExpr] -> SomeExpr -> SubPass DesugarLists m SomeExpr
makeLams []     o = return o
makeLams (a:as) o = do
    newO <- makeLams as o
    lam' a newO
