{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Prologue

import qualified Control.Concurrent.Async            as Async
import qualified Control.Monad.State                 as State
import qualified Control.Monad.State.Layered         as State
import qualified Data.Graph.Data.Component.Vector    as ComponentVector
import qualified Data.Graph.Data.Graph.Class         as Graph
import qualified  Data.Graph.Data.Component.Vector   as ComponentVector
import qualified Data.Graph.Data.Component.Class       as Component

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

import qualified Data.Graph.Store as Store
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
import Luna.Pass.Resolve.AliasAnalysis
import Luna.Pass.Data.Root
import Luna.Pass.Data.UniqueNameGen

import qualified Luna.Pass.Sourcing.UnitLoader as ModLoader
import qualified Luna.Pass.Sourcing.Data.Unit  as Unit
import qualified Luna.Pass.Sourcing.Data.Def   as Def
import qualified Luna.Pass.Sourcing.Data.Class as Class
import qualified Luna.Pass.Sourcing.UnitMapper as UnitMap
import qualified Luna.Pass.Evaluation.Interpreter as Interpreter
import qualified Luna.Pass.Evaluation.EvaluateUnits as EvaluateUnits
import qualified Luna.Pass.Typing.TypeUnits as TypeUnits
import qualified Luna.Pass.Typing.Data.Typed as Typed
import qualified Luna.Pass.Typing.Typechecker as Typechecker
import qualified Luna.Package as Package
import qualified Data.Bimap as Bimap
import qualified Path as Path

import qualified Luna.Runtime as Runtime
import qualified Luna.Std as Std


import qualified Luna.Pass.Preprocess.PreprocessUnit as PreprocessUnit
import qualified Luna.Pass.Flow.ProcessUnits as ProcessUnits
import qualified Foreign.Memory.Manager as MM
import Luna.Pass.Data.Layer.Requester (Requester)

import qualified Luna.Pass.Data.Stage as TC

import qualified Data.Time.Clock as Time
----------------------
-- === TestPass === --
----------------------

-- === Definition === --

data TestPass2 = TestPass2

type instance Pass.Spec TestPass2 t = TestPass2Spec t
type family TestPass2Spec t where
    TestPass2Spec (Pass.Out Pass.Attrs) = '[]
    TestPass2Spec t = Pass.BasicPassSpec t

data TestPass = TestPass

type instance Pass.Spec TestPass t = TestPassSpec t
type family TestPassSpec t where
    TestPassSpec (Pass.Out Pass.Attrs) = '[Root]
    TestPassSpec t = Pass.BasicPassSpec t

data VisPass = VisPass

type instance Pass.Spec VisPass t = VisPassSpec t
type family VisPassSpec t where
    VisPassSpec (Pass.In Pass.Attrs) = '[Root, VisName]
    VisPassSpec t = Pass.BasicPassSpec t

data VisHdr = VisHdr

type instance Pass.Spec VisHdr t = VisHdrSpec t
type family VisHdrSpec t where
    VisHdrSpec (Pass.In Pass.Attrs) = '[Typed.DefHeader, VisName]
    VisHdrSpec t = Pass.BasicPassSpec t

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

instance ( Pass.Interface TestPass (Pass stage TestPass)
         {-, Store.Serializer IR.Terms (Pass stage TestPass)-}
         {-, Store.Deserializer (Pass stage TestPass)-}
         , IR.DeleteSubtree (Pass stage TestPass)
         ) => Pass.Definition stage TestPass where
    definition = do
        a <- IR.var "a"
        b <- IR.var "b"
        pl <- IR.resolvedDef "Std.Primitive" "primIntAdd"
        ap1 <- IR.app pl a
        ap2 <- IR.app ap1 b
        ac  <- IR.acc' ap2 "succ"

        Attr.put $ Root ac

        {-real1 <- IR.blank-}
        {-real2 <- IR.blank-}
        {-real3 <- IR.blank-}
        {-l1    <- IR.lam real2 real3-}
        {-l2    <- IR.lam real1 l1-}
        {-fhd   <- IR.grouped l2-}
        {-r <- Store.serialize (Layout.relayout fhd :: IR.SomeTerm)-}
        {-rr <- Store.deserialize r-}
        {-print fhd-}
        {-print rr-}
        {-Vis.visualizeAll "haha!"-}
        return ()


instance ( Pass.Interface TestPass2 (Pass stage TestPass2)
         {-, Store.Serializer IR.Terms (Pass stage TestPass2)-}
         {-, Store.Deserializer (Pass stage TestPass2)-}
         , IR.DeleteSubtree (Pass stage TestPass2)
         ) => Pass.Definition stage TestPass2 where
    definition = do
        a <- IR.var "a"
        g1 <- IR.grouped a
        ap <- IR.app g1 a

        bl <- IR.blank

        g2 <- IR.grouped bl
        b <- IR.var "b"
        IR.replace b bl
        ap2 <- IR.app b g2

        ap3 <- IR.app ap ap2

        IR.deleteSubtree ap3

        {-real1 <- IR.blank-}
        {-real2 <- IR.blank-}
        {-real3 <- IR.blank-}
        {-l1    <- IR.lam real2 real3-}
        {-l2    <- IR.lam real1 l1-}
        {-fhd   <- IR.grouped l2-}
        {-r <- Store.serialize (Layout.relayout fhd :: IR.SomeTerm)-}
        {-rr <- Store.deserialize r-}
        {-print fhd-}
        {-print rr-}
        {-Vis.visualizeAll "haha!"-}
        return ()

instance Pass.Definition TC.Stage VisHdr where
    definition = do
        hdr <- unwrap <$> Attr.get @Typed.DefHeader
        case hdr of
            Left c -> print c
            Right r -> do
                x <- Store.deserialize r
                VisName n <- Attr.get
                print x
                Vis.visualizeSubtree n x
                IR.deleteSubtree x




data ShellCompiler

type instance Graph.Components      ShellCompiler          = '[IR.Terms, IR.Links]
type instance Graph.ComponentLayers ShellCompiler IR.Links = '[IR.Target, IR.Source]
type instance Graph.ComponentLayers ShellCompiler IR.Terms
   = '[IR.Users, IR.Model, IR.Type, CodeSpan, Requester]


makeLenses ''ModuleMap
---------------------
-- === Testing === --
---------------------

{-main' :: IO ()-}
{-main' = Graph.encodeAndEval @ShellCompiler $ Scheduler.evalT $ do-}
    {-Scheduler.registerPass @ShellCompiler @TestPass2-}
    {-Scheduler.runPassByType @TestPass2-}

{-tu :: Map IR.Qualified Unit.Unit -> Scheduler.SchedulerT (Graph.GraphT ShellCompiler IO) Typed.Units-}
{-tu = TypeUnits.typeUnits @ShellCompiler-}

timeIt :: MonadIO m => String -> m r -> m r
timeIt label act = do
    t1 <- liftIO Time.getCurrentTime
    r <- act
    t2 <- liftIO Time.getCurrentTime
    putStrLn $ label <> ": " <> show (Time.diffUTCTime t2 t1)
    return r


main :: IO ()
main = Graph.encodeAndEval @TC.Stage $ Scheduler.evalT $ do
    {-Scheduler.registerPass @ShellCompiler @TestPass-}
    {-for_ [1..] $ \i -> do-}
        {-print i-}
    {-Scheduler.runPassByType @TestPass-}

    pstd <- Path.parseAbsDir "/Users/marcinkostrzewa/code/luna-core/stdlib/Std"
    ptest <- Path.parseAbsDir "/Users/marcinkostrzewa/luna/projects/OpenCV"
    stdSourcesMap <- timeIt "STD SRC DISCOVERY" $ fmap Path.toFilePath . Bimap.toMapR <$> Package.findPackageSources pstd
    testSourcesMap <- timeIt "PROJECT SRC DISCOVERY" $ fmap Path.toFilePath . Bimap.toMapR <$> Package.findPackageSources ptest
    timeIt "MODLOADER INIT" $ ModLoader.initHC
    (finalize, stdUnitRef) <- timeIt "BUILD STD" $ Std.stdlib @TC.Stage
    Scheduler.registerAttr @Unit.UnitRefsMap
    Scheduler.setAttr $ Unit.UnitRefsMap $ Map.singleton "Std.Primitive" stdUnitRef
    mods <- timeIt "LOAD MODS" $ do
        ModLoader.loadUnit def (Map.union stdSourcesMap testSourcesMap) [] "OpenCV.Main"
        for Std.stdlibImports $
            ModLoader.loadUnit def (Map.union stdSourcesMap testSourcesMap) []
        Unit.UnitRefsMap mods <- Scheduler.getAttr
        return mods

    units <- timeIt "MAP MODS" $ do
        flip Map.traverseWithKey mods $ \n u -> case u ^. Unit.root of
            Unit.Graph r -> UnitMap.mapUnit n r
            Unit.Precompiled u -> pure u


    let unitResolvers   = Map.mapWithKey Res.resolverFromUnit units
        importResolvers = Map.mapWithKey (Res.resolverForUnit unitResolvers) $ over wrapped ("Std.Base" :) . over wrapped ("Std.Primitive" :) . view Unit.imports <$> mods

        unitsWithResolvers = Map.mapWithKey (\n u -> (importResolvers Map.! n, u)) units


    fun <- timeIt "TC" $ do
        (typedUnits, computedUnits) <- ProcessUnits.processUnits def def unitsWithResolvers
        Typed.getDef "OpenCV.Main" "main" typedUnits
        Runtime.lookupSymbol computedUnits "OpenCV.Main" "main"
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

