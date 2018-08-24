{-# LANGUAGE NoStrict             #-}
{-# LANGUAGE NoStrictData         #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Pass.Transform.Desugar.DesugarListLiterals where

import Prologue

import qualified Control.Monad.State              as State
import qualified Data.Graph.Data.Component.List   as ComponentList
import qualified Data.Graph.Data.Component.Vector as ComponentVector
import qualified Data.Graph.Data.Layer.Layout     as Layout
import qualified Luna.IR                          as IR
import qualified Luna.IR.Aliases                  as Uni
import qualified Luna.IR.Layer                    as Layer
import qualified Luna.Pass                        as Pass
import qualified Luna.Pass.Attr                   as Attr
import qualified Luna.Pass.Basic                  as Pass
import qualified Luna.Pass.Data.Stage             as TC
import qualified Luna.Pass.Data.UniqueNameGen     as NameGen

import Luna.Pass.Data.Root (Root (Root))



--------------------------------------
-- === DesugarListLiterals Pass === --
--------------------------------------

data DesugarListLiterals

type instance Pass.Spec DesugarListLiterals t = DesugarListLiteralsSpec t
type family DesugarListLiteralsSpec t where
    DesugarListLiteralsSpec (Pass.In  Pass.Attrs)
        = '[Root, NameGen.UniqueNameGen]
    DesugarListLiteralsSpec (Pass.Out Pass.Attrs) = '[NameGen.UniqueNameGen]
    DesugarListLiteralsSpec t = Pass.BasicPassSpec t

instance Pass.Definition TC.Stage DesugarListLiterals where
    definition = do
        Root root <- Attr.get
        newRoot <- desugarLists False root
        Attr.put $ Root newRoot

desugarLists :: Bool -> IR.SomeTerm -> TC.Pass DesugarListLiterals IR.SomeTerm
desugarLists isPattern e = Layer.read @IR.Model e >>= \case
    Uni.List elts' -> do
        elts <- traverse IR.source =<< ComponentVector.toList elts'
        newElts <- traverse (desugarLists isPattern) elts
        properList <- properListRep isPattern (Just <$> newElts)
        IR.replace properList e
        return properList
    Uni.Tuple elts' -> do
        elts <- traverse IR.source =<< ComponentVector.toList elts'
        newElts <- traverse (desugarLists isPattern) elts
        properTuple <- properTupleRep isPattern (Just <$> newElts)
        IR.replace properTuple e
        return properTuple
    Uni.Unify l r -> do
        desugarLists True      =<< IR.source l
        desugarLists isPattern =<< IR.source r
        return e
    Uni.Lam i o -> do
        desugarLists True      =<< IR.source i
        desugarLists isPattern =<< IR.source o
        return e
    Uni.Function _ as b -> do
        mapM_ (desugarLists True <=< IR.source) =<< ComponentVector.toList as
        desugarLists isPattern =<< IR.source b
        return e
    _ -> do
        ComponentList.mapM_ (desugarLists isPattern <=< IR.source) =<< IR.inputs e
        return e

properListRep :: Bool -> [Maybe IR.SomeTerm] -> TC.Pass DesugarListLiterals IR.SomeTerm
properListRep isPattern elts = do
    (binds, elems) <- prepareBinders elts
    list           <- mkListOf elems
    if isPattern then return list else makeLams binds list

mkListOf :: [IR.SomeTerm] -> TC.Pass DesugarListLiterals IR.SomeTerm
mkListOf [] = IR.cons' "Empty" []
mkListOf (e:elts) = prepend e =<< mkListOf elts where
    prepend e rest = do
        prep  <- IR.cons "Prepend" []
        withE <- IR.app prep e
        IR.app' withE rest

properTupleRep :: Bool -> [Maybe IR.SomeTerm] -> TC.Pass DesugarListLiterals IR.SomeTerm
properTupleRep isPattern elts = do
    (binds, elems) <- prepareBinders elts
    tuple <- mkTupleOf elems
    if isPattern then return tuple else makeLams binds tuple

mkTupleOf :: [IR.SomeTerm] -> TC.Pass DesugarListLiterals IR.SomeTerm
mkTupleOf elts = flip (foldlM IR.app') elts =<< IR.cons' consName [] where
    size     = length elts
    consName = convert $ "Tuple" <> show size

prepareBinders :: [Maybe IR.SomeTerm] -> TC.Pass DesugarListLiterals ([IR.SomeTerm], [IR.SomeTerm])
prepareBinders []               = return ([], [])
prepareBinders (Just e : elts)  = Layer.read @IR.Model e >>= \case
    Uni.Blank   -> prepareBinders $ Nothing : elts
    Uni.Missing -> prepareBinders $ Nothing : elts
    _ -> (_2 %~ (e:)) <$> prepareBinders elts
prepareBinders (Nothing : elts) = do
    (binds, es) <- prepareBinders elts
    v           <- IR.var' =<< NameGen.generateName
    return (v : binds, v : es)

makeLams :: [IR.SomeTerm] -> IR.SomeTerm -> TC.Pass DesugarListLiterals IR.SomeTerm
makeLams []     o = return o
makeLams (a:as) o = do
    newO <- makeLams as o
    IR.lam' a newO

