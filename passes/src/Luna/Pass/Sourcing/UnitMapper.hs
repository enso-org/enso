{-# LANGUAGE UndecidableInstances #-}

module Luna.Pass.Sourcing.UnitMapper where

import Prologue

import qualified Data.Graph.Data.Component.Vector    as ComponentVector
import qualified Data.Graph.Data.Layer.Layout        as Layout
import qualified Data.Map                            as Map
import qualified Luna.IR                             as IR
import qualified Luna.IR.Aliases                     as Uni
import qualified Luna.IR.Layer                       as Layer
import qualified Luna.Pass                           as Pass
import qualified Luna.Pass.Attr                      as Attr
import qualified Luna.Pass.Basic                     as Pass
import qualified Luna.Pass.Scheduler                 as Scheduler
import qualified Luna.Pass.Sourcing.Data.Def         as Def
import qualified Luna.Pass.Sourcing.Utils            as Sourcing

import Data.Map (Map)
import Luna.Pass.Data.Root
import Luna.Pass.Sourcing.Data.Unit
import Luna.Pass.Sourcing.Data.Class
import Luna.Pass.Sourcing.Data.Def
import Luna.Pass.Sourcing.ClassProcessor

data UnitMapper

type instance Pass.Spec UnitMapper t = UnitMapperSpec t
type family UnitMapperSpec t where
    UnitMapperSpec (Pass.In  Pass.Attrs) = '[Root]
    UnitMapperSpec (Pass.Out Pass.Attrs) = '[PartiallyMappedUnit]
    UnitMapperSpec t = Pass.BasicPassSpec t

data PartiallyMappedUnit = PartiallyMappedUnit
    { _defs :: DefsMap
    , _clss :: Map IR.Name (Documented (IR.Term IR.Record))
    } deriving Show
type instance Attr.Type PartiallyMappedUnit = Attr.Atomic
instance Default PartiallyMappedUnit where
    def = PartiallyMappedUnit def def

makeLenses ''PartiallyMappedUnit

instance Pass.Interface UnitMapper (Pass.Pass stage UnitMapper)
      => Pass.Definition stage UnitMapper where
    definition = do
        Root root <- Attr.get
        unitMap   <- partiallyMapUnit $ Layout.unsafeRelayout root
        Attr.put unitMap

partiallyMapUnit :: Pass.Interface UnitMapper m
        => IR.Term IR.Unit -> m PartiallyMappedUnit
partiallyMapUnit root = do
    IR.Unit _ _ cls <- IR.model root
    klass <- IR.source cls
    Layer.read @IR.Model klass >>= \case
        Uni.Record _ _ _ _ decls' -> do
            decls <- traverse IR.source =<< ComponentVector.toList decls'
            foldM registerDecl def decls
        _ -> return def

registerDecl :: Pass.Interface UnitMapper m
             => PartiallyMappedUnit -> IR.SomeTerm -> m PartiallyMappedUnit
registerDecl map t = do
    (doc, root) <- Sourcing.cutDoc t
    Layer.read @IR.Model root >>= \case
        Uni.Function n _ _ -> do
            IR.source n >>= Layer.read @IR.Model >>= \case
                Uni.Var name -> do
                    let documented =
                          Documented doc (Def.Body $ Layout.unsafeRelayout root)
                    return $ map & defs . wrapped . at name .~ Just documented
                _ -> return map
        Uni.Record _ n _ _ _ -> do
            let documented = Documented doc (Layout.unsafeRelayout root)
            return $ map & clss . at n .~ Just documented
        _ -> return map


mapUnit :: forall stage m.
        ( Scheduler.MonadScheduler m
        , Scheduler.PassRegister stage ClassProcessor m
        , Pass.Definition stage ClassProcessor
        , Scheduler.PassRegister stage UnitMapper m
        , Pass.Definition stage UnitMapper
        ) => IR.Term IR.Unit -> m Unit
mapUnit root = do
    Scheduler.registerAttr @Root
    Scheduler.registerAttr @PartiallyMappedUnit
    Scheduler.registerAttr @Class
    Scheduler.enableAttrByType @Class
    Scheduler.enableAttrByType @PartiallyMappedUnit

    Scheduler.setAttr $ Root $ Layout.relayout root

    Scheduler.registerPass @stage @ClassProcessor
    Scheduler.registerPass @stage @UnitMapper

    Scheduler.runPassByType @UnitMapper

    PartiallyMappedUnit defs clss <- Scheduler.getAttr

    classMap <- (traverse.traverse) mapClass clss
    return $ Unit defs classMap


mapClass :: Scheduler.MonadScheduler m
         => IR.Term IR.Record -> m Class
mapClass root = do
    Scheduler.setAttr $ Root $ Layout.relayout root
    Scheduler.runPassByType @ClassProcessor
    Scheduler.getAttr @Class
