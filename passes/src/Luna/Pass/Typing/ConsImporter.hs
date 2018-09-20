{-# LANGUAGE NoStrict             #-}
{-# LANGUAGE NoStrictData         #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Pass.Typing.ConsImporter where

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
import qualified Luna.Pass.Typing.Data.Typed      as Typed
import qualified Luna.Pass.Typing.Data.UniQueue   as UniQueue

import Luna.Pass.Data.Root (Root (..))


data ConsImporter

type instance Pass.Spec ConsImporter t = ConsImporterSpec t
type family ConsImporterSpec t where
    ConsImporterSpec (Pass.In  Pass.Attrs) = '[ Root
                                              , Typed.Units
                                              , UniQueue.UniQueue
                                              ]
    ConsImporterSpec (Pass.Out Pass.Attrs) = '[UniQueue.UniQueue]
    ConsImporterSpec t = TC.BasePassSpec t

instance Pass.Definition TC.Stage ConsImporter where
    definition = do
        Root root <- Attr.get
        importConses root

importConses :: IR.SomeTerm -> TC.Pass ConsImporter ()
importConses expr = Layer.read @IR.Model expr >>= \case
    Uni.ResolvedCons u cls n as -> do
        resolution <- Typed.requestCons u cls n
        case resolution of
            Nothing -> do
                Error.setError
                    (Just $ Error.unexpectedConsTypeNotFound u cls n)
                    expr
            Just rooted -> do
                cs <- Store.deserialize rooted
                IR.ResolvedCons _ _ _ typedAs <- IR.modelView cs

                tp    <- IR.source =<< Layer.read @IR.Type cs
                oldTp <- IR.source =<< Layer.read @IR.Type expr
                uniTp <- Layout.unsafeRelayout <$> IR.unify tp oldTp
                Requester.setRequester (Just expr) uniTp
                UniQueue.register uniTp

                oldArgs <- traverse IR.source =<< ComponentVector.toList as
                newArgs <- traverse IR.source =<< ComponentVector.toList typedAs
                oldArgTypes <- traverse (IR.source <=< Layer.read @IR.Type) oldArgs
                newArgTypes <- traverse (IR.source <=< Layer.read @IR.Type) newArgs

                unis <- zipWithM IR.unify oldArgTypes newArgTypes
                traverse_ (Requester.setRequester $ Just expr) unis
                UniQueue.registers $ Layout.unsafeRelayout <$> unis

                IR.deleteSubtree cs
    _ -> do
        inps <- IR.inputs expr
        ComponentList.mapM_ (importConses <=< IR.source) inps

