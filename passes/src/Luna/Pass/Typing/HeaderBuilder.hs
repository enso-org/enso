{-# LANGUAGE NoStrict             #-}
{-# LANGUAGE NoStrictData         #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Pass.Typing.HeaderBuilder where

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
import qualified Luna.Pass.Scheduler              as Scheduler
import qualified Luna.Pass.Typing.Base            as TC
import qualified Luna.Pass.Typing.Data.AccQueue   as AccQueue
import qualified Luna.Pass.Typing.Data.AppQueue   as AppQueue
import qualified Luna.Pass.Typing.Data.Target     as Target
import qualified Luna.Pass.Typing.Data.Typed      as Typed
import qualified Luna.Pass.Typing.Data.UniQueue   as UniQueue
import qualified Luna.Syntax.Prettyprint as Prettyprint

import Luna.Pass.Data.Root (Root (..))

data HeaderBuilder

type instance Pass.Spec HeaderBuilder t = HeaderBuilderSpec t
type family HeaderBuilderSpec t where
    HeaderBuilderSpec (Pass.In  Pass.Attrs) = '[ Root
                                               , AppQueue.AppQueue
                                               , AccQueue.AccQueue
                                               , UniQueue.UniQueue
                                               , Target.Target
                                               ]
    HeaderBuilderSpec (Pass.Out Pass.Attrs) = '[ Typed.DefHeader
                                               , AppQueue.AppQueue
                                               , AccQueue.AccQueue
                                               , UniQueue.UniQueue
                                               ]
    HeaderBuilderSpec t = TC.BasePassSpec t

instance Pass.Definition TC.Stage HeaderBuilder where
    definition = do
        Root root <- Attr.get
        tgt       <- Attr.get @Target.Target
        AppQueue.AppQueue apps <- Attr.get
        AccQueue.AccQueue accs <- Attr.get
        UniQueue.UniQueue unis <- Attr.get
        traverse_ (Requester.setRequester Nothing) apps
        traverse_ (Requester.setRequester Nothing) accs
        traverse_ (Requester.setRequester Nothing) unis
        traverse_ (Requester.pushArising tgt) apps
        traverse_ (Requester.pushArising tgt) accs
        traverse_ (Requester.pushArising tgt) unis
        tmpBlank <- IR.blank
        IR.substitute tmpBlank root
        hdr <- IR.defHeader root unis accs apps
        err <- Error.getError root
        r <- case err of
            Just e -> do
                IR.replace root tmpBlank
                IR.deleteSubtreeWithWhitelist (Set.singleton root) hdr
                pure $ Typed.DefHeader $ Left $ e & Error.failedAt %~ (tgt :)
            Nothing -> do
                prerooted  <- Store.serialize
                    (Layout.unsafeRelayout hdr :: IR.Term IR.DefHeader)
                IR.replace root tmpBlank
                let hdrInps = Set.fromList $ concat [ [root]
                                                    , Layout.relayout <$> apps
                                                    , Layout.relayout <$> accs
                                                    , Layout.relayout <$> unis
                                                    ]
                IR.deleteSubtreeWithWhitelist hdrInps hdr
                copyHdr <- Store.deserialize prerooted
                IR.DefHeader croot' cunis' caccs' capps' <- IR.modelView copyHdr
                croot <- IR.source croot'
                cunis <- traverse IR.source =<< ComponentVector.toList cunis'
                caccs <- traverse IR.source =<< ComponentVector.toList caccs'
                capps <- traverse IR.source =<< ComponentVector.toList capps'
                ctype <- IR.source =<< Layer.read @IR.Type croot
                chdr  <- IR.defHeader ctype cunis caccs capps
                IR.deleteSubtree copyHdr
                rooted <- Store.serialize $ Layout.unsafeRelayout chdr
                IR.deleteSubtree chdr
                pure $ Typed.DefHeader $ Right rooted

        Attr.put r

newtype GraphCopy = GraphCopy (Store.Rooted IR.SomeTerm)
makeLenses ''GraphCopy
type instance Attr.Type GraphCopy = Attr.Atomic
instance Default GraphCopy where
    def = wrap $ wrap ""

data Serializer

type instance Pass.Spec Serializer t = SerializerSpec t
type family SerializerSpec t where
    SerializerSpec (Pass.In  Pass.Attrs) = '[Root]
    SerializerSpec (Pass.Out Pass.Attrs) = '[GraphCopy]
    SerializerSpec t = TC.BasePassSpec t

instance Pass.Definition TC.Stage Serializer where
    definition = do
        Root r <- Attr.get
        tmpBlank <- IR.blank
        IR.substitute tmpBlank r
        rooted <- Store.serialize r
        IR.replace r tmpBlank
        Attr.put $ GraphCopy rooted

data Deserializer

type instance Pass.Spec Deserializer t = DeserializerSpec t
type family DeserializerSpec t where
    DeserializerSpec (Pass.In  Pass.Attrs) = '[GraphCopy]
    DeserializerSpec (Pass.Out Pass.Attrs) = '[Root]
    DeserializerSpec t = TC.BasePassSpec t

instance Pass.Definition TC.Stage Deserializer where
    definition = do
        GraphCopy rooted <- Attr.get
        r <- Store.deserialize rooted
        Attr.put $ Root r

serialize :: IR.SomeTerm -> TC.Monad GraphCopy
serialize r = do
    Scheduler.registerAttr @Root
    Scheduler.registerAttr @GraphCopy
    Scheduler.enableAttrByType @GraphCopy
    Scheduler.setAttr $ Root r
    Scheduler.registerPass @TC.Stage @Serializer
    Scheduler.runPassByType @Serializer
    Scheduler.getAttr @GraphCopy

deserialize :: GraphCopy -> TC.Monad IR.SomeTerm
deserialize r = do
    Scheduler.registerAttr @Root
    Scheduler.registerAttr @GraphCopy
    Scheduler.enableAttrByType @Root
    Scheduler.setAttr r
    Scheduler.registerPass @TC.Stage @Deserializer
    Scheduler.runPassByType @Deserializer
    unwrap <$> Scheduler.getAttr @Root

