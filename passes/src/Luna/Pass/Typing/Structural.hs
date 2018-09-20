{-# LANGUAGE NoStrict             #-}
{-# LANGUAGE NoStrictData         #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Pass.Typing.Structural where

import Prologue

import qualified Data.Graph.Data.Component.List        as ComponentList
import qualified Data.Graph.Data.Component.Vector      as ComponentVector
import qualified Data.Graph.Data.Layer.Layout          as Layout
import qualified Luna.IR                               as IR
import qualified Luna.IR.Aliases                       as Uni
import qualified Luna.IR.Layer                         as Layer
import qualified Luna.Pass                             as Pass
import qualified Luna.Pass.Attr                        as Attr
import qualified Luna.Pass.Data.Layer.Requester        as Requester
import qualified Luna.Pass.Data.Stage                  as TC
import qualified Luna.Pass.Data.UniqueNameGen          as NameGen
import qualified Luna.Pass.Typing.Base                 as TC
import qualified Luna.Pass.Typing.Data.AccQueue        as AccQueue
import qualified Luna.Pass.Typing.Data.AppQueue        as AppQueue
import qualified Luna.Pass.Typing.Data.UniQueue        as UniQueue

import Luna.Pass.Data.Root (Root (..))

data StructuralTyping

type instance Pass.Spec StructuralTyping t = StructuralTypingSpec t
type family StructuralTypingSpec t where
    StructuralTypingSpec (Pass.In  Pass.Attrs) = '[ Root
                                                  , NameGen.UniqueNameGen
                                                  , AppQueue.AppQueue
                                                  , AccQueue.AccQueue
                                                  , UniQueue.UniQueue
                                                  ]
    StructuralTypingSpec (Pass.Out Pass.Attrs) = '[ NameGen.UniqueNameGen
                                                  , AppQueue.AppQueue
                                                  , AccQueue.AccQueue
                                                  , UniQueue.UniQueue
                                                  ]
    StructuralTypingSpec t = TC.BasePassSpec t

instance Pass.Definition TC.Stage StructuralTyping where
    definition = do
        Root root <- Attr.get
        getStructuralType root
        return ()

getStructuralType :: IR.SomeTerm -> TC.Pass StructuralTyping IR.SomeTerm
getStructuralType expr = do
    tp <- IR.source =<< Layer.read @IR.Type expr
    Layer.read @IR.Model tp >>= \case
        Uni.Top -> attachStructuralType expr
        _       -> return tp

baseMod :: IR.Qualified
baseMod = "Std.Base"

attachStructuralType :: IR.SomeTerm -> TC.Pass StructuralTyping IR.SomeTerm
attachStructuralType expr = do
    tp :: IR.SomeTerm <- Layer.read @IR.Model expr >>= \case
        IR.UniTermNumber n -> do
            isInt <- IR.isInteger n
            let cName = if isInt then "Int" else "Real"
            IR.resolvedCons' baseMod cName cName ([] :: [IR.SomeTerm])
        Uni.RawString s -> do
            IR.resolvedCons' baseMod "Text" "Text" ([] :: [IR.SomeTerm])
        Uni.FmtString s -> do
            parts   <- traverse IR.source =<< ComponentVector.toList s
            partTps <- traverse getStructuralType parts
            txt <- IR.resolvedCons' baseMod "Text" "Text" ([] :: [IR.SomeTerm])
            unis <- traverse (IR.unify txt) partTps
            traverse_ (Requester.setRequester $ Just expr) unis
            UniQueue.registers $ Layout.unsafeRelayout <$> unis
            return txt
        Uni.Acc a n' -> do
            at <- getStructuralType =<< IR.source a
            n <- IR.source n'
            acc <- IR.acc' at n
            Requester.setRequester (Just expr) acc
            AccQueue.register $ Layout.unsafeRelayout acc
            return acc
        Uni.App f a -> do
            at <- getStructuralType =<< IR.source a
            ft <- getStructuralType =<< IR.source f
            app <- IR.app' ft at
            Requester.setRequester (Just expr) app
            AppQueue.register $ Layout.unsafeRelayout app
            return app
        Uni.Lam i o -> do
            ot <- getStructuralType =<< IR.source o
            it <- getStructuralType =<< IR.source i
            IR.lam' it ot
        Uni.Unify l r -> do
            lt <- getStructuralType =<< IR.source l
            rt <- getStructuralType =<< IR.source r
            uni <- IR.unify lt rt
            Requester.setRequester (Just expr) uni
            UniQueue.register $ Layout.unsafeRelayout uni
            return rt
        Uni.Function n as' o -> do
            as   <- traverse IR.source =<< ComponentVector.toList as'
            args <- traverse getStructuralType as
            ot   <- getStructuralType =<< IR.source o
            nt   <- getStructuralType =<< IR.source n
            lamt <- foldM (flip IR.lam') ot (reverse args)
            uni  <- IR.unify nt lamt
            Requester.setRequester (Just expr) uni
            UniQueue.register $ Layout.unsafeRelayout uni
            return lamt
        Uni.Grouped g  -> getStructuralType =<< IR.source g
        Uni.Marked _ b -> getStructuralType =<< IR.source b
        Uni.Seq a b -> do
            getStructuralType =<< IR.source a
            getStructuralType =<< IR.source b
        Uni.Match a cls -> do
            at       <- getStructuralType =<< IR.source a
            clauses  <- traverse IR.source =<< ComponentVector.toList cls
            clausest <- traverse getStructuralType clauses
            clauseOuts <- for clausest $ \ct -> do
                retT <- IR.app ct at
                Requester.setRequester (Just expr) retT
                AppQueue.register $ Layout.unsafeRelayout retT
                return retT
            outTp <- IR.var' =<< NameGen.generateName
            unis  <- traverse (IR.unify outTp) clauseOuts
            UniQueue.registers $ Layout.unsafeRelayout <$> unis
            traverse_ (Requester.setRequester $ Just expr) unis
            return outTp
        _ -> do
            inps <- IR.inputs expr
            ComponentList.mapM_ (getStructuralType <=< IR.source) inps
            IR.var' =<< NameGen.generateName
    oldTp <- Layer.read @IR.Type expr >>= IR.source
    IR.reconnectLayer @IR.Type tp expr
    IR.deleteSubtree oldTp
    return tp

