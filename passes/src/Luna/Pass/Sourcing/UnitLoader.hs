{-# LANGUAGE NoStrict             #-}
{-# LANGUAGE NoStrictData         #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Pass.Sourcing.UnitLoader where

import Prologue hiding (init)

import qualified Control.Monad.Exception              as Exception
import qualified Data.Graph.Data.Component.List       as ComponentList
import qualified Data.Graph.Data.Component.Vector     as ComponentVector
import qualified Data.Graph.Data.Layer.Layout         as Layout
import qualified Data.Map                             as Map
import qualified Data.Set                             as Set
import qualified Luna.IR                              as IR
import qualified Luna.IR.Aliases                      as Uni
import qualified Luna.IR.Layer                        as Layer
import qualified Luna.Pass                            as Pass
import qualified Luna.Pass.Attr                       as Attr
import qualified Luna.Pass.Basic                      as Pass
import qualified Luna.Pass.Data.Stage                 as TC
import qualified Luna.Pass.Scheduler                  as Scheduler
import qualified Luna.Syntax.Text.Parser.State.Result  as Parser
import qualified Luna.Syntax.Text.Parser.State.Invalid as Parser
import qualified Luna.Pass.Parsing.Parser             as Parser
import qualified Luna.Syntax.Text.Source              as Parser
import qualified System.IO                            as IO

import Control.Monad.Exception              (MonadException)
import Data.Set                             (Set)
import Luna.Pass.Data.Root
import Luna.Pass.Sourcing.Data.Unit as Unit
import Luna.Pass.Sourcing.ImportsPlucker

type UnitRequestStack = [IR.Qualified]

data UnitLoadingError
    = UnitSourcesNotFound UnitRequestStack IR.Qualified
    | ImportsCycleError UnitRequestStack
    deriving (Show)

instance Exception UnitLoadingError

initHC :: TC.Monad ()
initHC = init

init :: TC.Monad ()
init = do
    Scheduler.registerAttr @Parser.Source
    Scheduler.registerAttr @Parser.Invalids
    Scheduler.registerAttr @Parser.Result
    Scheduler.registerAttr @Imports
    Scheduler.registerAttr @Root

    Scheduler.registerPass @TC.Stage @Parser.Parser
    Scheduler.registerPass @TC.Stage @ImportsPlucker

resetParserState :: TC.Monad ()
resetParserState = do
    Scheduler.enableAttrByType @Parser.Source
    Scheduler.enableAttrByType @Parser.Invalids
    Scheduler.enableAttrByType @Parser.Result
    Scheduler.enableAttrByType @Imports

loadUnitIfMissing :: Set IR.Qualified
                  -> Map.Map IR.Qualified FilePath
                  -> [IR.Qualified]
                  -> IR.Qualified
                  -> TC.Monad ()
loadUnitIfMissing = \knownModules sourcesMap stack modName -> do
    UnitRefsMap m <- Scheduler.getAttr
    when (Map.notMember modName m && Set.notMember modName knownModules)
        $ loadUnit knownModules sourcesMap stack modName

readUnit
    :: FilePath
    -> IR.Qualified
    -> TC.Monad UnitRef
readUnit srcPath name = do
    resetParserState
    fileHandle <- liftIO $ IO.openFile srcPath IO.ReadMode
    liftIO $ IO.hSetEncoding fileHandle IO.utf8
    src <- liftIO $ IO.hGetContents fileHandle

    Scheduler.setAttr @Parser.Source $ convert src
    Scheduler.runPassByType @Parser.Parser

    Parser.Result root <- Scheduler.getAttr @Parser.Result

    Scheduler.setAttr $ Root $ Layout.relayout root

    Scheduler.runPassByType @ImportsPlucker

    imports <- Scheduler.getAttr @Imports

    pure $ UnitRef (Unit.Graph $ Layout.unsafeRelayout root) imports


loadUnit :: Set IR.Qualified
         -> Map.Map IR.Qualified FilePath
         -> [IR.Qualified]
         -> IR.Qualified
         -> TC.Monad ()
loadUnit knownModules sourcesMap stack modName = do
    when (modName `elem` stack) $
        Exception.throw $ ImportsCycleError $ modName : stack :: TC.Monad ()

    srcPath <- Exception.fromJust
        (UnitSourcesNotFound stack modName)
        (Map.lookup modName sourcesMap)

    unitRef <- readUnit srcPath modName
    Scheduler.modifyAttr_ @UnitRefsMap $ wrapped . at modName .~ Just unitRef

    traverse_
        (loadUnitIfMissing knownModules sourcesMap (modName : stack))
        (unwrap $ unitRef ^. Unit.imports)

