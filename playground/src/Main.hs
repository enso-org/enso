{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Prologue
import qualified Luna.Shell as Shell
import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map  as Map
import qualified Data.Text as Text
import qualified System.Environment as System
import qualified Text.PrettyPrint.ANSI.Leijen as Doc

import qualified Control.Monad.State as State

import qualified Luna.Pass as Pass
import qualified Luna.Pass.Attr as Attr
import qualified Luna.Pass.Scheduler as Scheduler
import qualified Luna.Runner as Runner
import qualified Luna.Syntax.Text.Parser.Pass as Parser
import qualified Luna.Syntax.Text.Parser.Data.Result as Parser
{-import qualified Luna.Syntax.Text.Parser.IR.Class as Parser-}
import qualified Luna.Syntax.Text.Source as Parser
import qualified Luna.IR                        as IR
import qualified Luna.IR.Layer as Layer
import qualified OCI.IR.Layout as IR

import qualified Data.PtrList.Mutable as PtrList

import qualified Data.Tag as Tag
import Data.Set (Set)
import qualified Data.Set as Set


import qualified Luna.Debug.IR.Visualizer as Vis
-------------------
-- === Shell === --
-------------------


data XD = XD

type instance Pass.Spec XD t = XDSpec t
type family XDSpec t where
    XDSpec (Pass.In Pass.Attrs) = '[World]
    XDSpec t = Pass.BasicPassSpec t

newtype World = World (IR.Term IR.Unit)
type instance Attr.Type World = Attr.Atomic
instance Default World where
    def = undefined

instance Pass.Definition XD where
    definition = do
        World root <- Attr.get @World
        print root
        Vis.displayVisualization "foo" root
        u@(IR.Unit imphub units cls) <- IR.model root
        length <$> IR.inputs u >>= print
        IR.source imphub >>= Layer.read @IR.Model >>= \h -> do
              length <$> IR.inputs h >>= print
              print "to import hub!"
        bar <- IR.var "bar"
        foo <- IR.acc bar "foo"
        length <$> (IR.inputs =<< Layer.read @IR.Model bar) >>= print
        length <$> (IR.inputs =<< Layer.read @IR.Model foo) >>= print
        return ()

main :: IO ()
main = Scheduler.runManual reg sched where
    reg = do
        Runner.registerAll
        Parser.registerStatic
    sched = do
        let lunafilePath = "/Users/marcinkostrzewa/code/luna/stdlib/Std/src/System.luna"
        lunafile <- readFile lunafilePath

        Parser.registerDynamic
        Scheduler.registerAttr @World
        Scheduler.enableAttrByType @World
        Scheduler.registerPass @XD
        Scheduler.setAttr @Parser.Source (convert lunafile)
        Scheduler.runPassByType @Parser.Parser
        Just r <- fmap unwrap <$> Scheduler.lookupAttr @Parser.Result
        Scheduler.setAttr $ World r
        print "parsed"
        Scheduler.runPassByType @XD

Pass.cache_phase1 ''XD
Pass.cache_phase2 ''XD

stdlibPath :: IO FilePath
stdlibPath = do
    env     <- Map.fromList <$> Env.getEnvironment
    exePath <- fst <$> splitExecutablePath
    let (<</>>)        = (FilePath.</>)  -- purely for convenience,
                                         -- because </> is defined elswhere
        parent         = let p = FilePath.takeDirectory
                         in \x -> if FilePath.hasTrailingPathSeparator x
                                  then p (p x)
                                  else p x
        defaultStdPath = (parent . parent . parent $ exePath)
                    <</>> "config"
                    <</>> "env"
        envStdPath     = Map.lookup Project.lunaRootEnv env
        stdPath        = fromMaybe defaultStdPath envStdPath <</>> "Std"
    exists <- doesDirectoryExist stdPath
    if exists
        then putStrLn $ "Found the standard library at: " <> stdPath
        else die $ "Standard library not found. Set the "
                <> Project.lunaRootEnv
                <> " environment variable"
    return stdPath
