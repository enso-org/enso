{-# LANGUAGE NoStrict             #-}
{-# LANGUAGE NoStrictData         #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Pass.Typing.ConsGeneration where

import Prologue

import qualified Data.Graph.Data.Component.List        as ComponentList
import qualified Data.Graph.Data.Component.Vector      as ComponentVector
import qualified Data.Graph.Data.Layer.Layout          as Layout
import qualified Data.Graph.Store                      as Store
import qualified Data.Map                              as Map
import qualified Data.Mutable.Storable.SmallAutoVector as SmallVector
import qualified Luna.IR                               as IR
import qualified Luna.IR.Aliases                       as Uni
import qualified Luna.IR.Layer                         as Layer
import qualified Luna.Pass                             as Pass
import qualified Luna.Pass.Attr                        as Attr
import qualified Luna.Pass.Data.Stage                  as TC
import qualified Luna.Pass.Scheduler                   as Scheduler
import qualified Luna.Pass.Sourcing.Data.Unit          as Unit
import qualified Luna.Pass.Typing.Base                 as TC

import Data.Graph.Store    (Rooted)
import Data.Map            (Map)
import Luna.Pass.Data.Root (Root (..))

newtype ConsMap = ConsMap (Map IR.Name (Rooted (IR.Term IR.ResolvedCons)))
type instance Attr.Type ConsMap = Attr.Atomic
instance Default ConsMap where
    def = ConsMap def
makeLenses ''ConsMap


run :: IR.Qualified -> IR.SomeTerm
    -> TC.Monad (Map IR.Name (Rooted (IR.Term IR.ResolvedCons)))
run uname root = do
    Scheduler.registerAttr @Root
    Scheduler.registerAttr @Unit.Name
    Scheduler.registerAttr @ConsMap

    Scheduler.setAttr $ Unit.Name uname
    Scheduler.setAttr $ Root root
    Scheduler.enableAttrByType @ConsMap

    Scheduler.registerPass @TC.Stage @ConsGeneration
    Scheduler.runPassByType @ConsGeneration

    unwrap <$> Scheduler.getAttr @ConsMap

data ConsGeneration

type instance Pass.Spec ConsGeneration t = ConsGenerationSpec t
type family ConsGenerationSpec t where
    ConsGenerationSpec (Pass.In  Pass.Attrs) = '[ Root, Unit.Name ]
    ConsGenerationSpec (Pass.Out Pass.Attrs) = '[ ConsMap ]
    ConsGenerationSpec t = TC.BasePassSpec t

instance Pass.Definition TC.Stage ConsGeneration where
    definition = do
        Root root <- Attr.get
        Unit.Name uname <- Attr.get
        conses <- genClassConses uname root
        Attr.put $ ConsMap $ Map.fromList conses

getParamNames :: [IR.SomeTerm] -> TC.Pass ConsGeneration [IR.Name]
getParamNames ts = fmap catMaybes $ for ts $ Layer.read @IR.Model >=> \case
    Uni.Var n -> pure $ Just n
    _         -> pure Nothing

genClassConses :: IR.Qualified -> IR.SomeTerm
               -> TC.Pass ConsGeneration [(IR.Name, Rooted (IR.Term IR.ResolvedCons))]
genClassConses uname cls = Layer.read @IR.Model cls >>= \case
    Uni.Record _ cname params conses _ -> do
        pars <- traverse IR.source =<< ComponentVector.toList params
        paramNames <- getParamNames pars
        cs <- traverse IR.source =<< ComponentVector.toList conses
        traverse (genCons uname cname paramNames) cs
    _ -> pure []

genCons :: IR.Qualified -> IR.Name -> [IR.Name] -> IR.SomeTerm
        -> TC.Pass ConsGeneration (IR.Name, Rooted (IR.Term IR.ResolvedCons))
genCons unitName clsName paramNames root = Layer.read @IR.Model root >>= \case
    Uni.RecordCons n fs -> do
        fields    <- traverse IR.source =<< ComponentVector.toList fs
        paramVars <- traverse IR.var' paramNames
        let varMap = Map.fromList $ zip paramNames paramVars
        retTp      <- IR.resolvedCons unitName clsName clsName paramVars
        fieldTypes <- catMaybes <$> traverse (processField varMap) fields
        consFields <- fmap concat $ for fieldTypes $ \(count, tp) -> do
            blanks <- sequence $ take count $ repeat IR.blank
            for_ blanks $ \b -> do
                oldTp <- IR.source =<< Layer.read @IR.Type b
                IR.replace tp oldTp
            pure blanks
        retCons <- Layout.unsafeRelayout <$> IR.resolvedCons unitName clsName n consFields
        oldTp <- IR.source =<< Layer.read @IR.Type retCons
        IR.replace retTp oldTp
        rooted  <- Store.serialize retCons
        IR.deleteSubtree retCons
        pure (n, rooted)
    _ -> error "Should not happen."

processField :: Map IR.Name IR.SomeTerm -> IR.SomeTerm
             -> TC.Pass ConsGeneration (Maybe (Int, IR.SomeTerm))
processField varMap root = Layer.read @IR.Model root >>= \case
    Uni.RecordFields ns tp' -> do
        tp <- IR.source tp'
        tmpBlank <- IR.blank
        IR.substitute tmpBlank tp
        rooted <- Store.serialize tp
        IR.replace tp tmpBlank
        tpCopy  <- Store.deserialize rooted
        fixedVars <- reconnectVars varMap tpCopy
        count <- length <$> SmallVector.toList ns
        pure $ Just (if count == 0 then 1 else count, fixedVars)
    _ -> pure Nothing

reconnectVars :: Map IR.Name IR.SomeTerm -> IR.SomeTerm
              -> TC.Pass ConsGeneration IR.SomeTerm
reconnectVars varMap root = Layer.read @IR.Model root >>= \case
    Uni.Var n -> do
        case Map.lookup n varMap of
            Just t -> do
                IR.replace t root
                pure t
            Nothing -> pure root
    _ -> do
        inps <- IR.inputs root
        ComponentList.mapM_ (reconnectVars varMap <=< IR.source) inps
        pure root

