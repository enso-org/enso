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
    newRoots <- forM (generalize <$> unwrap roots) desugarLists
    putAttr @ExprRoots $ wrap $ unsafeGeneralize <$> newRoots

desugarLists :: (MonadRef m, MonadPassManager m) => SomeExpr -> SubPass DesugarLists m SomeExpr
desugarLists e = do
    f <- inputs e
    mapM_ (desugarLists <=< source) f
    matchExpr e $ \case
        List elts -> do
            properList <- properListRep =<< (mapM . mapM) source elts
            replace properList e
            return properList
        _ -> return e

properListRep :: (MonadRef m, MonadPassManager m) => [Maybe SomeExpr] -> SubPass DesugarLists m SomeExpr
properListRep lsts = uncurry makeLams =<< buildRep lsts where
    buildRep []     = ([],) <$> cons'_ "Empty"
    buildRep (e:es) = do
        (binds, rep) <- buildRep es
        case e of
            Nothing -> do
                v <- var' =<< genName
                r <- prepend v rep
                return (v : binds, r)
            Just elt -> do
                matchExpr elt $ \case
                    Blank -> do
                        v <- var' =<< genName
                        r <- prepend v rep
                        return (v : binds, r)
                    _     -> (binds,) <$> prepend elt rep

    prepend e rest = do
        prep  <- cons'_ "Prepend"
        withE <- app' prep e
        app' withE rest

    makeLams []     o = return o
    makeLams (a:as) o = do
        newO <- makeLams as o
        lam' a newO

