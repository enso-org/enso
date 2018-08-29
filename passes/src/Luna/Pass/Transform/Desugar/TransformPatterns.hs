{-# LANGUAGE NoStrict             #-}
{-# LANGUAGE NoStrictData         #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Pass.Transform.Desugar.TransformPatterns where

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

import Luna.Pass.Data.Root (Root (Root))



------------------------------------
-- === TransformPatterns Pass === --
------------------------------------

data TransformPatterns

type instance Pass.Spec TransformPatterns t = TransformPatternsSpec t
type family TransformPatternsSpec t where
    TransformPatternsSpec (Pass.In  Pass.Attrs) = '[Root]
    TransformPatternsSpec t = Pass.BasicPassSpec t

instance Pass.Definition TC.Stage TransformPatterns where
    definition = do
        Root root <- Attr.get
        transformPatterns root

dumpConsApplication :: IR.SomeTerm -> TC.Pass TransformPatterns (IR.Name, [IR.SomeTerm])
dumpConsApplication expr = Layer.read @IR.Model expr >>= \case
    Uni.Grouped g -> dumpConsApplication =<< IR.source g
    Uni.Cons n _  -> return (n, [])
    Uni.App f a   -> do
        (n, args) <- dumpConsApplication =<< IR.source f
        arg <- IR.source a
        return (n, arg : args)

flattenPattern :: IR.SomeTerm -> TC.Pass TransformPatterns IR.SomeTerm
flattenPattern expr = Layer.read @IR.Model expr >>= \case
    Uni.Grouped g -> flattenPattern =<< IR.source g
    Uni.Var{}     -> return expr
    Uni.Cons{}    -> return expr
    Uni.App{}     -> do
        (name, children) <- dumpConsApplication expr
        flatChildren     <- mapM flattenPattern $ reverse children
        IR.cons' name flatChildren
    _ -> return expr

transformPatterns :: IR.SomeTerm -> TC.Pass TransformPatterns ()
transformPatterns expr = Layer.read @IR.Model expr >>= \case
    Uni.Lam i o -> do
        inp <- IR.source i
        res <- flattenPattern inp
        IR.replace res inp
        transformPatterns =<< IR.source o
    Uni.Unify l r -> do
        left <- IR.source l
        res  <- flattenPattern left
        IR.replace res left
        transformPatterns =<< IR.source r
    Uni.Function n as b -> do
        args <- ComponentVector.toList as
        for_ args $ \a -> do
            arg <- IR.source a
            res <- flattenPattern arg
            IR.replace res arg
        transformPatterns =<< IR.source b
    _ -> do
        inps <- IR.inputs expr
        ComponentList.mapM_ (transformPatterns <=< IR.source) inps

