{-# LANGUAGE NoStrict             #-}
{-# LANGUAGE NoStrictData         #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Pass.Typing.DefImporter where

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


data DefImporter

type instance Pass.Spec DefImporter t = DefImporterSpec t
type family DefImporterSpec t where
    DefImporterSpec (Pass.In  Pass.Attrs) = '[ Root
                                             , Typed.Units
                                             , AppQueue.AppQueue
                                             , AccQueue.AccQueue
                                             , UniQueue.UniQueue
                                             ]
    DefImporterSpec (Pass.Out Pass.Attrs) = '[ AppQueue.AppQueue
                                             , AccQueue.AccQueue
                                             , UniQueue.UniQueue
                                             ]
    DefImporterSpec t = TC.BasePassSpec t

instance Pass.Definition TC.Stage DefImporter where
    definition = do
        Root root <- Attr.get
        importDefs root

importDefs :: IR.SomeTerm -> TC.Pass DefImporter ()
importDefs expr = Layer.read @IR.Model expr >>= \case
    Uni.ResolvedDef mod n -> do
        resolution <- unwrap <$> Typed.requestDef mod n
        case resolution of
            Left e -> Error.setError (Just e) expr
            Right rooted -> do
                hdr <- Store.deserialize rooted
                IR.DefHeader tp' unis' accs' apps' <- IR.modelView hdr
                tp <- IR.source tp'
                unis <- traverse IR.source =<< ComponentVector.toList unis'
                accs <- traverse IR.source =<< ComponentVector.toList accs'
                apps <- traverse IR.source =<< ComponentVector.toList apps'
                UniQueue.registers $ Layout.unsafeRelayout <$> unis
                AccQueue.registers $ Layout.unsafeRelayout <$> accs
                AppQueue.registers $ Layout.unsafeRelayout <$> apps
                traverse_ (Requester.setRequester $ Just expr) unis
                traverse_ (Requester.setRequester $ Just expr) accs
                traverse_ (Requester.setRequester $ Just expr) apps
                oldTp <- IR.source =<< Layer.read @IR.Type expr
                IR.replace tp oldTp
                IR.deleteSubtreeWithWhitelist (Set.fromList $ unis <> accs <> apps) hdr
    _ -> do
        inps <- IR.inputs expr
        ComponentList.mapM_ (importDefs <=< IR.source) inps

