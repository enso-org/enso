{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Prologue

import qualified Control.Monad.State                 as State
import qualified Control.Monad.State.Layered         as State
import qualified Control.Concurrent.Async            as Async
import qualified Data.Graph.Data.Graph.Class         as Graph
import qualified  Data.Graph.Data.Component.Vector   as ComponentVector
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
import qualified Data.Graph.Data.Layer.Layout    as Layout

import Data.Map  (Map)
import Data.Set  (Set)
import Luna.Pass (Pass)

import Data.Graph.Data.Component.Class (unsafeNull)

import Luna.Syntax.Text.Parser.Data.CodeSpan (CodeSpan)
import Luna.Syntax.Text.Parser.Data.Invalid (Invalids)

import qualified Data.Graph.Data.Graph.Class          as Graph
import Data.Graph.Component.Node.Destruction

import Luna.Pass.Transform.Desugar.RemoveGrouped
import Luna.Pass.Transform.Desugar.TransformPatterns
import Luna.Pass.Transform.Desugar.DesugarPartialApplications
import Luna.Pass.Transform.Desugar.DesugarListLiterals
import Luna.Pass.Resolve.Data.UnresolvedVariables
import Luna.Pass.Resolve.AliasAnalysis
import Luna.Pass.Data.Root
import Luna.Pass.Data.UniqueNameGen
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
        Vis.displayVisualization n root



data ShellCompiler

type instance Graph.Components      ShellCompiler          = '[IR.Terms, IR.Links]
type instance Graph.ComponentLayers ShellCompiler IR.Links = '[IR.Target, IR.Source]
type instance Graph.ComponentLayers ShellCompiler IR.Terms
   = '[IR.Users, IR.Model, IR.Type, CodeSpan]


makeLenses ''ModuleMap
---------------------
-- === Testing === --
---------------------

funDiscoverFlow src = do
    Scheduler.registerAttr @World
    Scheduler.enableAttrByType @World

    Scheduler.registerAttr @ModuleMap
    Scheduler.enableAttrByType @ModuleMap

    Scheduler.registerAttr @UniqueNameGen
    Scheduler.enableAttrByType @UniqueNameGen

    Scheduler.registerAttr @VisName
    Scheduler.enableAttrByType @VisName

    Scheduler.registerAttr @Root
    Scheduler.enableAttrByType @Root

    Scheduler.registerAttr @UnresolvedVariables
    Scheduler.enableAttrByType @UnresolvedVariables

    Scheduler.registerAttr @Parser.Source
    Scheduler.enableAttrByType @Parser.Source

    Scheduler.registerAttr @Invalids
    Scheduler.enableAttrByType @Invalids

    Scheduler.registerAttr @Parser.Result
    Scheduler.enableAttrByType @Parser.Result

    Scheduler.registerPass @ShellCompiler @VisPass
    Scheduler.registerPass @ShellCompiler @TestPass
    Scheduler.registerPass @ShellCompiler @Parser.Parser

    Scheduler.setAttr @Parser.Source (convert src)
    Scheduler.runPassByType @Parser.Parser
    Just r <- fmap (Layout.unsafeRelayout . unwrap) <$> Scheduler.lookupAttr @Parser.Result
    Scheduler.setAttr $ Root r

    Scheduler.setAttr $ VisName "parsed"
    Scheduler.runPassByType @VisPass

    Scheduler.runPassByType @TestPass

    Just modmap <- Scheduler.lookupAttr @ModuleMap
    print modmap
    return (modmap, r)

funDesugarFlow root = do
    Scheduler.registerAttr @World
    Scheduler.enableAttrByType @World

    Scheduler.registerAttr @ModuleMap
    Scheduler.enableAttrByType @ModuleMap

    Scheduler.registerAttr @UniqueNameGen
    Scheduler.enableAttrByType @UniqueNameGen

    Scheduler.registerAttr @VisName
    Scheduler.enableAttrByType @VisName

    Scheduler.registerAttr @Root
    Scheduler.enableAttrByType @Root

    Scheduler.registerAttr @UnresolvedVariables
    Scheduler.enableAttrByType @UnresolvedVariables

    Scheduler.registerAttr @Parser.Source
    Scheduler.enableAttrByType @Parser.Source

    Scheduler.registerAttr @Invalids
    Scheduler.enableAttrByType @Invalids

    Scheduler.registerAttr @Parser.Result
    Scheduler.enableAttrByType @Parser.Result

    Scheduler.registerPass @ShellCompiler @VisPass
    Scheduler.registerPass @ShellCompiler @TestPass
    Scheduler.registerPass @ShellCompiler @Parser.Parser
    Scheduler.registerPass @ShellCompiler @RemoveGrouped
    Scheduler.registerPass @ShellCompiler @AliasAnalysis
    Scheduler.registerPass @ShellCompiler @TransformPatterns
    Scheduler.registerPass @ShellCompiler @DesugarPartialApplications
    Scheduler.registerPass @ShellCompiler @DesugarListLiterals

    Scheduler.setAttr $ Root root

    Scheduler.runPassByType @DesugarListLiterals
    Scheduler.runPassByType @DesugarPartialApplications
    Scheduler.runPassByType @RemoveGrouped
    Scheduler.runPassByType @TransformPatterns
    Scheduler.runPassByType @AliasAnalysis

    Scheduler.setAttr $ VisName $ show root
    Scheduler.runPassByType @VisPass

visFlow name root = do
    Scheduler.registerAttr @World
    Scheduler.enableAttrByType @World

    Scheduler.registerAttr @ModuleMap
    Scheduler.enableAttrByType @ModuleMap

    Scheduler.registerAttr @UniqueNameGen
    Scheduler.enableAttrByType @UniqueNameGen

    Scheduler.registerAttr @VisName
    Scheduler.enableAttrByType @VisName

    Scheduler.registerAttr @Root
    Scheduler.enableAttrByType @Root

    Scheduler.registerAttr @UnresolvedVariables
    Scheduler.enableAttrByType @UnresolvedVariables

    Scheduler.registerAttr @Parser.Source
    Scheduler.enableAttrByType @Parser.Source

    Scheduler.registerAttr @Invalids
    Scheduler.enableAttrByType @Invalids

    Scheduler.registerAttr @Parser.Result
    Scheduler.enableAttrByType @Parser.Result

    Scheduler.registerPass @ShellCompiler @VisPass
    Scheduler.registerPass @ShellCompiler @TestPass
    Scheduler.registerPass @ShellCompiler @Parser.Parser
    Scheduler.registerPass @ShellCompiler @RemoveGrouped
    Scheduler.registerPass @ShellCompiler @AliasAnalysis
    Scheduler.registerPass @ShellCompiler @TransformPatterns
    Scheduler.registerPass @ShellCompiler @DesugarPartialApplications
    Scheduler.registerPass @ShellCompiler @DesugarListLiterals

    Scheduler.setAttr $ Root root

    Scheduler.setAttr $ VisName name
    Scheduler.runPassByType @VisPass

main :: IO ()
main = Graph.encodeAndEval @ShellCompiler $ do
    {-let lunafile :: String = unlines [ "def foo x:"-}
                                     {-, "    (((((None)))))"-}
                                     {-, "def bar y:"-}
                                     {-, "    (((None)))"-}
                                     {-]-}
    let lunafilePath = "/Users/marcinkostrzewa/code/luna/stdlib/Std/src/Graphics2D.luna"
    lunafile <- readFile lunafilePath
    {-let lunafile :: String = unlines [ "def foo a b c:"-}
                                     {-, "    x = a: a + b"-}
                                     {-, "    y = x b"-}
                                     {-, "    c"-}
                                     {-]-}
    (modMap, root) <- Scheduler.evalT $ funDiscoverFlow lunafile
    asyncs <- for (modMap ^. functions) $ Graph.async @ShellCompiler . Scheduler.evalT . funDesugarFlow . Layout.relayout
    liftIO $ for_ asyncs Async.wait
    Scheduler.evalT $ visFlow "after" root
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
--         envStdPath     = Map.lookup Project.lunaRootEnv env
--         stdPath        = fromMaybe defaultStdPath envStdPath <</>> "Std"
--     exists <- doesDirectoryExist stdPath
--     if exists
--         then putStrLn $ "Found the standard library at: " <> stdPath
--         else die $ "Standard library not found. Set the "
--                 <> Project.lunaRootEnv
--                 <> " environment variable"
--     return stdPath

