{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Prologue

import qualified Control.Monad.State                 as State
import qualified Control.Monad.State.Layered         as State
import qualified Data.Graph.Data.Graph.Class         as Graph
import qualified Data.List                           as List
import qualified Data.Map                            as Map
import qualified Data.Set                            as Set
import qualified Data.Tag                            as Tag
import qualified Data.Text                           as Text
import qualified Luna.Debug.IR.Visualizer            as Vis
import qualified Luna.IR                             as IR
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

-- instance Pass.Interface XD (Pass stage XD) => Pass.Definition stage XD where
--     definition = do
--         World root <- Attr.get @World
--         print root
--         Vis.displayVisualization "foo" root
--         u@(IR.Unit imphub units cls) <- IR.model root
--         print =<< IR.inputs root
--         x <- Layer.read @IR.Source imphub
--         print =<< IR.inputs x
--         bar <- IR.var "bar"
--         foo <- IR.acc bar "foo"
--         print =<< IR.inputs bar
--         print =<< IR.inputs foo
--         return ()

test :: ∀ stage m.
    (Pass.Interface XD m, State.Getter (Layer.ByteOffset IR.Term IR.Model) m) => m ()
test = do
    World root <- Attr.get @World
    print root
    Vis.displayVisualization "foo" root
    u@(IR.Unit imphub units cls) <- IR.model root
    print =<< IR.inputs root
    x <- Layer.read @IR.Source imphub
    print =<< IR.inputs x
    bar <- IR.var "bar"
    foo <- IR.acc bar "foo"
    print =<< IR.inputs bar
    print =<< IR.inputs foo
    return ()

main :: IO ()
main = pure () -- Graph.encodeAndEval @Pass.Compilation $ Scheduler.evalT $ do
        -- let lunafilePath = "/Users/marcinkostrzewa/code/luna/stdlib/Std/src/System.luna"
        -- lunafile <- readFile lunafilePath
        -- Scheduler.registerAttr @World
        -- Scheduler.enableAttrByType @World
        -- Scheduler.registerPass @Pass.Compilation @XD
        -- Scheduler.setAttr @Parser.Source (convert lunafile)
        -- Scheduler.runPassByType @Parser.Parser
        -- Just r <- fmap unwrap <$> Scheduler.lookupAttr @Parser.Result
        -- Scheduler.setAttr $ World r
        -- print "parsed"
        -- Scheduler.runPassByType @XD


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
