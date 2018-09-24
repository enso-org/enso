{-# LANGUAGE NoStrict             #-}
{-# LANGUAGE NoStrictData         #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Pass.Typing.MethodImporter where

import Prologue

import qualified Data.Graph.Data.Component.List   as ComponentList
import qualified Data.Graph.Data.Component.Vector as ComponentVector
import qualified Data.Graph.Data.Layer.Layout     as Layout
import qualified Data.Graph.Store                 as Store
import qualified Data.Set                         as Set
import qualified Luna.IR                          as IR
import qualified Luna.IR.Aliases                  as Uni
import qualified Luna.IR.Layer                    as Layer
import qualified Luna.Pass                        as Pass
import qualified Luna.Pass.Attr                   as Attr
import qualified Luna.Pass.Data.Error             as Error
import qualified Luna.Pass.Data.Layer.Requester   as Requester
import qualified Luna.Pass.Data.Stage             as TC
import qualified Luna.Pass.Data.UniqueNameGen     as NameGen
import qualified Luna.Pass.Typing.Base            as TC
import qualified Luna.Pass.Typing.Data.AccQueue   as AccQueue
import qualified Luna.Pass.Typing.Data.AppQueue   as AppQueue
import qualified Luna.Pass.Typing.Data.Progress   as Progress
import qualified Luna.Pass.Typing.Data.Target     as Target
import qualified Luna.Pass.Typing.Data.Typed      as Typed
import qualified Luna.Pass.Typing.Data.UniQueue   as UniQueue
import qualified Luna.Syntax.Prettyprint as Prettyprint

import Data.Either         (lefts, rights)
import Luna.Pass.Data.Root (Root (..))


data MethodImporter

type instance Pass.Spec MethodImporter t = MethodImporterSpec t
type family MethodImporterSpec t where
    MethodImporterSpec (Pass.In  Pass.Attrs) = '[ Root
                                                , Typed.Units
                                                , Target.Target
                                                , AppQueue.AppQueue
                                                , AccQueue.AccQueue
                                                , UniQueue.UniQueue
                                                ]
    MethodImporterSpec (Pass.Out Pass.Attrs) = '[ Progress.Progress
                                                , AppQueue.AppQueue
                                                , AccQueue.AccQueue
                                                , UniQueue.UniQueue
                                                ]
    MethodImporterSpec t = TC.BasePassSpec t

instance Pass.Definition TC.Stage MethodImporter where
    definition = run

run :: TC.Pass MethodImporter ()
run = do
    AccQueue.AccQueue accs <- Attr.get
    res <- traverse deepSolve accs
    Attr.put $ Progress.Progress $ any isRight res
    Attr.put $ AccQueue.AccQueue $ lefts res <> concat (rights res)

deepSolve :: IR.Term IR.Acc
          -> TC.Pass MethodImporter (Either (IR.Term IR.Acc) [IR.Term IR.Acc])
deepSolve root = do
    res <- solve root
    case res of
        Left t   -> return res
        Right ts -> do
            rs <- traverse deepSolve ts
            return $ Right $ lefts rs <> concat (rights rs)

solve :: IR.Term IR.Acc
      -> TC.Pass MethodImporter (Either (IR.Term IR.Acc) [IR.Term IR.Acc])
solve expr = do
    IR.Acc t n' <- IR.modelView expr
    n <- IR.source n' >>= \a -> Layer.read @IR.Model a >>= \case
        Uni.Var name -> return name
        b            -> do
            foo  <- Prettyprint.run @Prettyprint.Simple def expr
            expr <- Prettyprint.run @Prettyprint.Simple def a
            error $ "MethodImporter.solve: unknown name " <> convert expr <> " on " <> convert foo
    target     <- IR.source t
    req        <- Requester.getRequester expr
    arising    <- Requester.getArising expr
    Layer.read @IR.Model target >>= \case
        Uni.ResolvedCons unit cls _ _ -> do
            compilationTarget <- Attr.get @Target.Target
            replacement <- if compilationTarget == Target.Method unit cls n then do
                    Root root <- Attr.get
                    rootTp    <- IR.source =<< Layer.read @IR.Type root
                    return $ Right (rootTp, [])
                else do
                    importMethodDef req arising unit cls n
            case replacement of
                Left e -> do
                    Requester.setRequester Nothing expr
                    IR.deleteSubtree expr
                    traverse_ (Error.setError $ Just e) req
                    return $ Right []
                Right (imported, new) -> do
                    ap <- IR.app imported target
                    AppQueue.register $ Layout.unsafeRelayout ap
                    IR.replace ap expr
                    return $ Right new
        Uni.Lam{} -> do
            Requester.setRequester Nothing expr
            IR.deleteSubtree expr
            for_ req $ Error.setError $ Just
                          $ Error.cannotCallMethodOnAFunction n
                              & Error.arisingFrom .~ arising
            return $ Right []
        _ -> return $ Left expr

importMethodDef :: Maybe IR.SomeTerm -> [Target.Target] -> IR.Qualified
                -> IR.Name -> IR.Name
                -> TC.Pass MethodImporter (Either Error.CompileError
                                (IR.SomeTerm, [IR.Term IR.Acc]))
importMethodDef req arising mod cls n = do
    resolution <- unwrap <$> Typed.requestMethod mod cls n
    case resolution of
        Left e -> return $ Left $ e & Error.arisingFrom .~ arising
        Right rooted -> do
            hdr <- Store.deserialize rooted
            IR.DefHeader tp' unis' accs' apps' <- IR.modelView hdr
            tp <- IR.source tp'
            unis <- traverse IR.source =<< ComponentVector.toList unis'
            accs <- traverse IR.source =<< ComponentVector.toList accs'
            apps <- traverse IR.source =<< ComponentVector.toList apps'
            UniQueue.registers $ Layout.unsafeRelayout <$> unis
            AppQueue.registers $ Layout.unsafeRelayout <$> apps
            traverse_ (Requester.setRequester req) unis
            traverse_ (Requester.setRequester req) accs
            traverse_ (Requester.setRequester req) apps
            IR.deleteSubtreeWithWhitelist (Set.fromList $ [tp] <> unis <> accs <> apps) hdr
            return $ Right (tp, Layout.unsafeRelayout <$> accs)

