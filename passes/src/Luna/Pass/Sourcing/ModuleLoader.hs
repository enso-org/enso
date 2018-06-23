{-# LANGUAGE UndecidableInstances #-}
module Luna.Pass.Sourcing.ModuleLoader where

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
import Luna.Pass.Sourcing.Data.ModulesMap

newtype Imports = Imports [IR.Qualified]
type instance Attr.Type Imports = Attr.Atomic
instance Default Imports where
    def = Imports def
makeLenses ''Imports

type ModuleRequestStack = [IR.Qualified]

data ModuleLoadingError
    = ModuleSourcesNotFound ModuleRequestStack IR.Qualified
    | ImportsCycleError ModuleRequestStack
    deriving (Show)

instance Exception ModuleLoadingError

data ImportsPlucker

type instance Pass.Spec ImportsPlucker t = ImportsPluckerSpec t
type family ImportsPluckerSpec t where
    ImportsPluckerSpec (Pass.In  Pass.Attrs) = '[Root]
    ImportsPluckerSpec (Pass.Out Pass.Attrs) = '[Imports]
    ImportsPluckerSpec t = Pass.BasicPassSpec t

instance Pass.Interface ImportsPlucker (Pass.Pass stage ImportsPlucker)
      => Pass.Definition stage ImportsPlucker where
    definition = do
        Root root <- Attr.get
        imps <- getImports root
        Attr.put $ Imports imps

getImports :: Pass.Interface ImportsPlucker m
           => IR.SomeTerm -> m [IR.Qualified]
getImports root = Layer.read @IR.Model root >>= \case
    Uni.Unit impHub _ _ -> do
        getImports =<< IR.source impHub
    Uni.ImportHub imps -> do
        imports <- ComponentVector.toList imps
        concat <$> traverse (getImports <=< IR.source) imports
    Uni.Imp src _ -> getImports =<< IR.source src
    Uni.ImportSource (IR.Absolute name) -> return [name]
    _ -> return []

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

loadModuleIfMissing ::
    ( Scheduler.MonadScheduler m
    , Exception.Throws ModuleLoadingError m
    ) => Map.Map IR.Qualified FilePath -> [IR.Qualified] -> IR.Qualified -> m ()
loadModuleIfMissing sourcesMap stack modName = do
    ModulesMap m <- Scheduler.getAttr
    when (Map.notMember modName m) $ loadModule sourcesMap stack modName


loadModule ::
    forall m.
    ( Scheduler.MonadScheduler m
    , Exception.Throws ModuleLoadingError m
    ) => Map.Map IR.Qualified FilePath -> [IR.Qualified] -> IR.Qualified -> m ()
loadModule sourcesMap stack modName = do
    when (modName `elem` stack) $ (Exception.throw $ ImportsCycleError $ modName : stack :: m ())
    resetParserState
    srcPath <- Exception.fromJust (ModuleSourcesNotFound stack modName)
                                  (Map.lookup modName sourcesMap)
    src <- readFile srcPath

    Scheduler.setAttr @Parser.Source $ convert src
    Scheduler.runPassByType @Parser.Parser

    root <- unwrap <$> Scheduler.getAttr @Parser.Result

    Scheduler.modifyAttr_ @ModulesMap $ wrapped . at modName .~ Just root

    Scheduler.setAttr $ Root $ Layout.relayout root

    Scheduler.runPassByType @ImportsPlucker

    imports <- unwrap <$> Scheduler.getAttr @Imports

    traverse_ (loadModuleIfMissing sourcesMap (modName : stack)) imports
