{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Prologue

import qualified Control.Concurrent.Async            as Async
import qualified Control.Monad.State                 as State
import qualified Control.Monad.State.Layered         as State
import qualified Data.Graph.Data.Component.Vector    as ComponentVector
import qualified Data.Graph.Data.Graph.Class         as Graph
import qualified Data.Graph.Data.Layer.Layout        as Layout
import qualified Data.List                           as List
import qualified Data.Map                            as Map
import qualified Data.Set                            as Set
import qualified Data.Tag                            as Tag
import qualified Data.Text                           as Text
import qualified Luna.Debug.IR.Visualizer            as Vis
import qualified Luna.IR                             as IR
import qualified Luna.IR.Aliases                     as Uni
import qualified Luna.IR.Layer                       as Layer
import qualified Luna.Pass                           as Pass
import qualified Luna.Pass.Attr                      as Attr
import qualified Luna.Pass.Basic                     as Pass
import qualified Luna.Pass.Scheduler                 as Scheduler
import qualified Luna.Shell                          as Shell
import qualified Luna.Syntax.Text.Parser.Data.Result as Parser
import qualified Luna.Syntax.Text.Parser.Pass        as Parser
import qualified Luna.Syntax.Text.Source             as Parser
import qualified OCI.Pass.Definition.Interface       as Pass
import qualified System.Environment                  as System
import qualified Text.PrettyPrint.ANSI.Leijen        as Doc

import Data.Map  (Map)
import Data.Set  (Set)
import Luna.Pass (Pass)

import Data.Graph.Data.Component.Class (unsafeNull)

import Luna.Syntax.Text.Parser.Data.CodeSpan (CodeSpan)
import Luna.Syntax.Text.Parser.Data.Invalid  (Invalids)

import           Data.Graph.Component.Node.Destruction
import qualified Data.Graph.Data.Graph.Class           as Graph

import           Luna.Pass.Data.Root
import           Luna.Pass.Data.UniqueNameGen
import qualified Luna.Pass.Preprocess.PreprocessDef as PreprocessDef
import           Luna.Pass.Resolve.AliasAnalysis
import qualified Luna.Pass.Resolve.Data.Resolution  as Res

import qualified Data.Bimap                         as Bimap
import qualified Luna.Pass.Evaluation.EvaluateUnits as EvaluateUnits
import qualified Luna.Pass.Evaluation.Interpreter   as Interpreter
import qualified Luna.Pass.Sourcing.Data.Class      as Class
import qualified Luna.Pass.Sourcing.Data.Def        as Def
import qualified Luna.Pass.Sourcing.Data.Unit       as Unit
import qualified Luna.Pass.Sourcing.UnitLoader      as ModLoader
import qualified Luna.Pass.Sourcing.UnitMapper      as UnitMap
import qualified Luna.Package                       as Package
import qualified Path                               as Path

import qualified Luna.Runtime             as Runtime
import qualified Luna.Runtime.Data.Future as Future
import qualified Luna.Std                 as Std

import qualified Luna.Pass.Preprocess.PreprocessUnit as PreprocessUnit
----------------------
-- === TestPass === --
----------------------

-- === Definition === --

data TestPass = TestPass

type instance Pass.Spec TestPass t = TestPassSpec t
type family TestPassSpec t where
    TestPassSpec (Pass.In Pass.Attrs) = '[Root]
    TestPassSpec (Pass.Out Pass.Attrs) = '[ModuleMap]
    TestPassSpec t = Pass.BasicPassSpec t

data VisPass = VisPass

type instance Pass.Spec VisPass t = VisPassSpec t
type family VisPassSpec t where
    VisPassSpec (Pass.In Pass.Attrs) = '[Root, VisName]
    VisPassSpec t = Pass.BasicPassSpec t

data ModuleMap = ModuleMap
    { _functions :: Map.Map IR.Name (IR.Term IR.Function)
    , _classes   :: Map.Map IR.Name (IR.Term IR.Record)
    } deriving (Show)
type instance Attr.Type ModuleMap = Attr.Atomic
instance Default ModuleMap where
    def = ModuleMap def def


-- === Attrs === --

newtype VisName = VisName String
type instance Attr.Type VisName = Attr.Atomic
instance Default VisName where
    def = VisName ""

newtype World = World (IR.Term IR.Unit)
type instance Attr.Type World = Attr.Atomic
instance Default World where
    def = World unsafeNull

instance Pass.Interface TestPass (Pass stage TestPass)
      => Pass.Definition stage TestPass where
    definition = do
        Root root <- Attr.get
        print root
        u@(IR.Unit imphub units cls) <- IR.model (Layout.unsafeRelayout root :: IR.Term IR.Unit)
        clas <- IR.source cls
        Layer.read @IR.Model clas >>= \case
            Uni.Record isNative name params conss decls' -> do
                decls <- traverse IR.source =<< ComponentVector.toList decls'
                ds <- for decls $ \d -> Layer.read @IR.Model d >>= \case
                    Uni.Function nvar' _ _ -> do
                        nvar :: IR.Term IR.Var <- Layout.unsafeRelayout <$> IR.source nvar'
                        (IR.Var n) <- IR.model nvar
                        return $ Just (n, Layout.unsafeRelayout d)
                    _ -> return Nothing
                Attr.put $ ModuleMap (Map.fromList $ catMaybes ds) def


instance Pass.Interface VisPass (Pass stage VisPass)
      => Pass.Definition stage VisPass where
    definition = do
        Root root <- Attr.get
        VisName n <- Attr.get
        print root
        Vis.visualizeSubtree n root



data ShellCompiler

type instance Graph.Components      ShellCompiler          = '[IR.Terms, IR.Links]
type instance Graph.ComponentLayers ShellCompiler IR.Links = '[IR.Target, IR.Source]
type instance Graph.ComponentLayers ShellCompiler IR.Terms
   = '[IR.Users, IR.Model, IR.Type, CodeSpan]


makeLenses ''ModuleMap
---------------------
-- === Testing === --
---------------------

main :: IO ()
main = Graph.encodeAndEval @ShellCompiler $ Scheduler.evalT $ do
    pstd <- Path.parseAbsDir "/Users/marcinkostrzewa/code/luna-core/stdlib/Std"
    ptest <- Path.parseAbsDir "/Users/marcinkostrzewa/luna/projects/OpenCV"
    stdSourcesMap <- fmap Path.toFilePath . Bimap.toMapR <$> Package.findPackageSources pstd
    testSourcesMap <- fmap Path.toFilePath . Bimap.toMapR <$> Package.findPackageSources ptest
    ModLoader.init @ShellCompiler
    (finalize, stdUnitRef) <- liftIO Std.stdlib
    Scheduler.registerAttr @Unit.UnitRefsMap
    Scheduler.setAttr $ Unit.UnitRefsMap $ Map.singleton "Std.Primitive" stdUnitRef
    ModLoader.loadUnit (Map.union stdSourcesMap testSourcesMap) [] "OpenCV.Main"
    for Std.stdlibImports $
        ModLoader.loadUnit (Map.union stdSourcesMap testSourcesMap) []
    Unit.UnitRefsMap mods <- Scheduler.getAttr

    units <- for mods $ \u -> case u ^. Unit.root of
        Unit.Graph r       -> UnitMap.mapUnit @ShellCompiler r
        Unit.Precompiled u -> pure u

    let unitResolvers   = Map.mapWithKey Res.resolverFromUnit units
        importResolvers = Map.mapWithKey (Res.resolverForUnit unitResolvers) $ over wrapped ("Std.Base" :) . over wrapped ("Std.Primitive" :) . view Unit.imports <$> mods

    for (Map.toList importResolvers) $ \(unitName, resolver) -> do
        let Just unit = Map.lookup unitName units
        PreprocessUnit.preprocessUnit @ShellCompiler resolver unit

    computedUnits <- EvaluateUnits.evaluateUnits @ShellCompiler units
    fun <- Runtime.lookupSymbol computedUnits "OpenCV.Main" "main"
    liftIO $ Runtime.runIO fun

    return ()

-- stdlibPath :: IO FilePath
-- stdlibPath = do
--     env     <- Map.fromList <$> System.getEnvironment
--     exePath <- fst <$> splitExecutablePath
--     let (<</>>)        = (FilePath.</>)  -- purely for convenience,
--                                          -- because </> is defined elswhere
--         parent         = let p = FilePath.takeDirectory
--                          in \x -> if FilePath.hasTrailingPathSeparator x
--                                   then p (p x)
--                                   else p x
--         defaultStdPath = (parent . parent . parent $ exePath)
--                     <</>> "config"
--                     <</>> "env"
--         envStdPath     = Map.lookup Package.lunaRootEnv env
--         stdPath        = fromMaybe defaultStdPath envStdPath <</>> "Std"
--     exists <- doesDirectoryExist stdPath
--     if exists
--         then putStrLn $ "Found the standard library at: " <> stdPath
--         else die $ "Standard library not found. Set the "
--                 <> Package.lunaRootEnv
--                 <> " environment variable"
--     return stdPath

