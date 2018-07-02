{-# LANGUAGE UndecidableInstances #-}

module Luna.Pass.Transform.Desugar.DesugarPartialApplications where

import Prologue

import qualified Data.Graph.Data.Component.List   as ComponentList
import qualified Data.Graph.Data.Component.Vector as ComponentVector
import qualified Data.Graph.Data.Layer.Layout     as Layout
import qualified Data.Map                         as Map
import qualified Data.Mutable.Class               as Mutable
import qualified Data.Vector.Storable.Foreign     as Vector
import qualified Luna.IR                          as IR
import qualified Luna.IR.Aliases                  as Uni
import qualified Luna.IR.Layer                    as Layer
import qualified Luna.Pass                        as Pass
import qualified Luna.Pass.Attr                   as Attr
import qualified Luna.Pass.Basic                  as Pass

import Data.Map                     (Map)
import Luna.Pass.Data.Root
import Luna.Pass.Data.UniqueNameGen as NameGen


data DesugarPartialApplications

type instance Pass.Spec DesugarPartialApplications t = DesugarPartialApplicationsSpec t
type family DesugarPartialApplicationsSpec t where
    DesugarPartialApplicationsSpec (Pass.In  Pass.Attrs) = '[Root, UniqueNameGen]
    DesugarPartialApplicationsSpec (Pass.Out Pass.Attrs) = '[Root, UniqueNameGen]
    DesugarPartialApplicationsSpec t = Pass.BasicPassSpec t

instance ( Pass.Interface DesugarPartialApplications
              (Pass.Pass stage DesugarPartialApplications)
         , IR.DeleteSubtree (Pass.Pass stage DesugarPartialApplications)
         ) => Pass.Definition stage DesugarPartialApplications where
    definition = do
        Root root <- Attr.get
        root'   <- desugarSections root
        newRoot <- desugarBlanks root'
        Attr.put $ Root newRoot

desugarSections ::
    ( Pass.Interface DesugarPartialApplications m
    , IR.DeleteSubtree m
    ) => IR.SomeTerm -> m IR.SomeTerm
desugarSections root = do
    inps <- IR.inputs root
    ComponentList.mapM_ (desugarSections <=< IR.source) inps
    Layer.read @IR.Model root >>= \case
        Uni.AccSection ns -> do
            x     <- IR.var' =<< NameGen.generateName
            names <- Mutable.toList ns
            accs  <- foldM IR.acc' (x :: IR.SomeTerm) names
            l     <- IR.lam' x accs
            IR.replace l root
            return l
        Uni.SectionLeft o a -> do
            bl   <- IR.blank
            op   <- IR.source o
            arg  <- IR.source a
            app1 <- IR.app op bl
            app2 <- IR.app' app1 arg
            IR.replace app2 root
            return app2
        Uni.SectionRight o a -> do
            op  <- IR.source o
            arg <- IR.source a
            app <- IR.app' op arg
            IR.replace app root
            return app
        _ -> return root

desugarBlanks :: ( Pass.Interface DesugarPartialApplications m
           , IR.DeleteSubtree m
           ) => IR.SomeTerm -> m IR.SomeTerm
desugarBlanks root = do
    res <- partialDesugarBlanks root
    replaceWithLam root res

desugarBlanks_ ::
    ( Pass.Interface DesugarPartialApplications m
    , IR.DeleteSubtree m
    ) => IR.SomeTerm -> m [IR.Term IR.Var]
desugarBlanks_ root = desugarBlanks root >> return []

replaceWithLam :: ( Pass.Interface DesugarPartialApplications m
                  , IR.DeleteSubtree m
                  ) => IR.SomeTerm -> [IR.Term IR.Var] -> m IR.SomeTerm
replaceWithLam e vars = if null vars then return e else do
    tmpBlank <- IR.blank'
    newNode  <- foldM (flip IR.lam') tmpBlank vars
    IR.substitute newNode e
    IR.replace e tmpBlank
    return newNode

partialDesugarBlanks ::
    ( Pass.Interface DesugarPartialApplications m
    , IR.DeleteSubtree m
    ) => IR.SomeTerm -> m [IR.Term IR.Var]
partialDesugarBlanks e = Layer.read @IR.Model e >>= \case
    Uni.Blank -> do
        v <- IR.var =<< NameGen.generateName
        IR.replace v e
        -- FIXME[WD]: this unsafeRelayout is nonsensical,
        -- needed due to a bug in the IR.var smart constructor.
        return [Layout.unsafeRelayout v]
    Uni.Grouped g -> desugarBlanks_ =<< IR.source g
    Uni.Marked _ b -> desugarBlanks_ =<< IR.source b
    Uni.Lam _ o -> desugarBlanks_ =<< IR.source o
    Uni.Function _ _ b -> desugarBlanks_ =<< IR.source b
    Uni.Unify _ r -> desugarBlanks_ =<< IR.source r
    Uni.Seq l r -> do
        desugarBlanks_ =<< IR.source l
        desugarBlanks_ =<< IR.source r
    Uni.App f a -> (<>) <$> (partialDesugarBlanks =<< IR.source f)
                        <*> (partialDesugarBlanks =<< IR.source a)
    Uni.Acc v _ -> partialDesugarBlanks =<< IR.source v
    Uni.Match t cls -> do
        desugarBlanks_ =<< IR.source t
        traverse_ (desugarBlanks_ <=< IR.source) =<< ComponentVector.toList cls
        return []
    _ -> do
        inps <- IR.inputs e
        ComponentList.mapM_ (desugarBlanks_ <=< IR.source) inps
        return []



