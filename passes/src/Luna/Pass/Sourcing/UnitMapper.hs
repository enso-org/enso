{-# LANGUAGE NoStrict             #-}
{-# LANGUAGE NoStrictData         #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Pass.Sourcing.UnitMapper where

import Prologue

import qualified Data.Graph.Data.Component.Vector as ComponentVector
import qualified Data.Graph.Data.Layer.Layout     as Layout
import qualified Luna.IR                          as IR
import qualified Luna.IR.Aliases                  as Uni
import qualified Luna.IR.Layer                    as Layer
import qualified Luna.Pass                        as Pass
import qualified Luna.Pass.Attr                   as Attr
import qualified Luna.Pass.Data.Error             as Error
import qualified Luna.Pass.Data.Stage             as TC
import qualified Luna.Pass.Scheduler              as Scheduler
import qualified Luna.Pass.Sourcing.Data.Def      as Def
import qualified Luna.Pass.Sourcing.Data.Unit     as Unit
import qualified Luna.Pass.Sourcing.Utils         as Sourcing

import Data.Map                          (Map)
import Luna.Pass.Data.Root
import Luna.Pass.Sourcing.ClassProcessor
import Luna.Pass.Sourcing.Data.Class     hiding (root)
import Luna.Pass.Sourcing.Data.Def       hiding (documented)
import Luna.Pass.Sourcing.Data.Unit      hiding (root)

data UnitMapper

type instance Pass.Spec UnitMapper t = UnitMapperSpec t
type family UnitMapperSpec t where
    UnitMapperSpec (Pass.In  Pass.Attrs) = '[Root, Unit.Name]
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

instance Pass.Definition TC.Stage UnitMapper where
    definition = do
        Root root <- Attr.get
        Unit.Name unitName <- Attr.get
        unitMap   <- partiallyMapUnit unitName $ Layout.unsafeRelayout root
        Attr.put unitMap

partiallyMapUnit :: IR.Qualified -> IR.Term IR.Unit -> TC.Pass UnitMapper PartiallyMappedUnit
partiallyMapUnit unitName root = do
    IR.Unit _ _ cls <- IR.modelView root
    klass <- IR.source cls
    Layer.read @IR.Model klass >>= \case
        Uni.Record _ _ _ _ decls' -> do
            decls <- traverse IR.source =<< ComponentVector.toList decls'
            foldM (registerDecl unitName) def decls
        _ -> return def

registerDecl :: IR.Qualified -> PartiallyMappedUnit -> IR.SomeTerm -> TC.Pass UnitMapper PartiallyMappedUnit
registerDecl unitName map t = do
    (doc, root) <- Sourcing.cutDoc t
    Layer.read @IR.Model root >>= \case
        Uni.Function n _ _ -> do
            IR.source n >>= Layer.read @IR.Model >>= \case
                Uni.Var name -> do
                    let documented =
                          Documented doc (Def.Body $ Layout.unsafeRelayout root)
                    when_ (isJust $ map ^. defs . wrapped . at name) $ do
                        let error = Error.duplicateFunctionDefinition
                                unitName name
                        Error.setError (Just error) root
                    return $ map & defs . wrapped . at name .~ Just documented
                _ -> return map
        Uni.Record _ n _ _ _ -> do
            let documented = Documented doc (Layout.unsafeRelayout root)
            return $ map & clss . at n .~ Just documented
        _ -> return map

mapUnit :: IR.Qualified -> IR.Term IR.Unit -> TC.Monad Unit
mapUnit unitName root = do
    Scheduler.registerAttr @Root
    Scheduler.registerAttr @PartiallyMappedUnit
    Scheduler.registerAttr @Class
    Scheduler.registerAttr @Unit.Name
    Scheduler.enableAttrByType @Class
    Scheduler.enableAttrByType @PartiallyMappedUnit

    Scheduler.setAttr $ Root $ Layout.relayout root
    Scheduler.setAttr $ Unit.Name $ unitName

    Scheduler.registerPass @TC.Stage @ClassProcessor
    Scheduler.registerPass @TC.Stage @UnitMapper

    Scheduler.runPassByType @UnitMapper

    PartiallyMappedUnit defs' clss' <- Scheduler.getAttr

    classMap <- (traverse.traverse) mapClass clss'
    return $ Unit defs' classMap


mapClass :: Scheduler.MonadScheduler m
         => IR.Term IR.Record -> m Class
mapClass root = do
    Scheduler.setAttr $ Root $ Layout.relayout root
    Scheduler.runPassByType @ClassProcessor
    Scheduler.getAttr @Class

