{-# LANGUAGE UndecidableInstances #-}
module Luna.Pass.Sourcing.UnitLoader where

import Prologue

import qualified Control.Monad.Exception              as Exception
import qualified Data.Graph.Data.Component.List       as ComponentList
import qualified Data.Graph.Data.Component.Vector     as ComponentVector
import qualified Data.Graph.Data.Layer.Layout         as Layout
import qualified Data.Map                             as Map
import qualified Luna.IR                              as IR
import qualified Luna.IR.Aliases                      as Uni
import qualified Luna.IR.Layer                        as Layer
import qualified Luna.Pass                            as Pass
import qualified Luna.Pass.Attr                       as Attr
import qualified Luna.Pass.Basic                      as Pass
import qualified Luna.Pass.Scheduler                  as Scheduler
import qualified Luna.Syntax.Text.Parser.Data.Result  as Parser
import qualified Luna.Syntax.Text.Parser.Data.Invalid as Parser
import qualified Luna.Syntax.Text.Parser.Pass         as Parser
import qualified Luna.Syntax.Text.Parser.Pass         as Parser
import qualified Luna.Syntax.Text.Source              as Parser

import Control.Monad.Exception              (MonadException)
import Luna.Pass.Data.Root
import Luna.Pass.Sourcing.Data.Unit as Unit
import Luna.Pass.Sourcing.ImportsPlucker

type UnitRequestStack = [IR.Qualified]

data UnitLoadingError
    = UnitSourcesNotFound UnitRequestStack IR.Qualified
    | ImportsCycleError UnitRequestStack
    deriving (Show)

instance Exception UnitLoadingError

init ::
    forall stage m.
    ( Scheduler.PassRegister stage Parser.Parser m
    , Pass.Definition stage Parser.Parser
    , Scheduler.PassRegister stage ImportsPlucker m
    , Pass.Definition stage ImportsPlucker
    , Scheduler.MonadScheduler m
    ) => m ()
init = do
    Scheduler.registerAttr @Parser.Source
    Scheduler.registerAttr @Parser.Invalids
    Scheduler.registerAttr @Parser.Result
    Scheduler.registerAttr @Imports
    Scheduler.registerAttr @Root

    Scheduler.registerPass @stage @Parser.Parser
    Scheduler.registerPass @stage @ImportsPlucker

resetParserState :: Scheduler.MonadScheduler m => m ()
resetParserState = do
    Scheduler.enableAttrByType @Parser.Source
    Scheduler.enableAttrByType @Parser.Invalids
    Scheduler.enableAttrByType @Parser.Result
    Scheduler.enableAttrByType @Imports

loadUnitIfMissing ::
    ( Scheduler.MonadScheduler m
    , Exception.Throws UnitLoadingError m
    ) => Map.Map IR.Qualified FilePath -> [IR.Qualified] -> IR.Qualified -> m ()
loadUnitIfMissing sourcesMap stack modName = do
    UnitRefsMap m <- Scheduler.getAttr
    when (Map.notMember modName m) $ loadUnit sourcesMap stack modName


loadUnit ::
    forall m.
    ( Scheduler.MonadScheduler m
    , Exception.Throws UnitLoadingError m
    ) => Map.Map IR.Qualified FilePath -> [IR.Qualified] -> IR.Qualified -> m ()
loadUnit sourcesMap stack modName = do
    when (modName `elem` stack) $
        (Exception.throw $ ImportsCycleError $ modName : stack :: m ())
    resetParserState
    srcPath <- Exception.fromJust (UnitSourcesNotFound stack modName)
                                  (Map.lookup modName sourcesMap)
    src <- readFile srcPath

    Scheduler.setAttr @Parser.Source $ convert src
    Scheduler.runPassByType @Parser.Parser

    root <- unwrap <$> Scheduler.getAttr @Parser.Result

    Scheduler.setAttr $ Root $ Layout.relayout root

    Scheduler.runPassByType @ImportsPlucker

    imports <- Scheduler.getAttr @Imports

    let unitRef = UnitRef (Unit.Graph root) imports
    Scheduler.modifyAttr_ @UnitRefsMap $ wrapped . at modName .~ Just unitRef

    traverse_ (loadUnitIfMissing sourcesMap (modName : stack)) (unwrap imports)
