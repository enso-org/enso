{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE OverloadedLists    #-}

module Luna.Shell where

import           Luna.Prelude        hiding (String, seq, cons, Constructor)
import qualified Luna.Prelude        as P
import qualified Data.Map            as Map
import           Data.Map            (Map)
import qualified Data.TreeSet        as TreeSet
import qualified Data.Bimap          as Bimap

import Luna.Builtin.Std
import qualified OCI.Pass           as Pass
import           OCI.Pass           (SubPass, Preserves, Inputs, Outputs)
import qualified OCI.IR.Repr.Vis    as Vis
import OCI.IR.Name.Qualified
import Luna.IR
import Luna.IR.Term.Unit (UnitSet)
import Luna.IR.Term.Cls  (Cls)
import Luna.Syntax.Text.Parser.Errors (Invalids)
import qualified Luna.IR.Term.Unit  as Term
import qualified Luna.IR.Term.Cls   as Term
import Luna.Builtin.Data.Module     as Module
import Luna.Builtin.Data.Class
import Luna.Builtin.Data.LunaEff
import Luna.Builtin.Data.LunaValue  as LunaValue
import qualified Luna.Builtin.Data.Function   as Function

import Luna.Pass.Data.UniqueNameGen
import Luna.Pass.Data.ExprRoots

import qualified Luna.Syntax.Text.Parser.Parser   as Parser
import qualified Luna.Syntax.Text.Source          as Source
import qualified Luna.Syntax.Text.Parser.Parsing  as Parsing
import qualified Luna.Syntax.Text.Parser.Class    as Parsing
import qualified Luna.Syntax.Text.Parser.Marker   as Parser (MarkedExprMap)
import qualified Luna.Syntax.Text.Parser.CodeSpan as CodeSpan
import qualified Luna.Syntax.Text.Layer.Loc       as Loc

import Luna.Test.IR.Runner
import Data.TypeDesc
import System.IO.Unsafe

import qualified Luna.Pass.Transform.Desugaring.RemoveGrouped  as RemoveGrouped
import qualified Luna.Pass.UnitCompilation.ModuleProcessing    as ModuleProcessing
import qualified Luna.Pass.Sourcing.UnitLoader as UL

import qualified Luna.Project       as Project
import qualified Luna.Compilation   as Project
import           System.Directory   (getCurrentDirectory)
import qualified Path               as Path
import qualified System.Environment as Env

data ShellTest
type instance Abstract ShellTest = ShellTest
type instance Inputs  Net   ShellTest = '[AnyExpr]
type instance Outputs Net   ShellTest = '[AnyExpr]
type instance Inputs  Layer ShellTest = '[AnyExpr // Model, AnyExpr // UID, Link' AnyExpr // UID, Link' AnyExpr // Model, AnyExpr // Succs]
type instance Outputs Layer ShellTest = '[]
type instance Inputs  Attr  ShellTest = '[Parser.ReparsingStatus, WorldExpr]
type instance Outputs Attr  ShellTest = '[]
type instance Inputs  Event ShellTest = '[]
type instance Outputs Event ShellTest = '[New // AnyExpr]
type instance Preserves     ShellTest = '[]

main :: HasCallStack => IO ()
main = void $ runPM True $ do
    stdPath  <- (<> "/Std/") <$> liftIO (Env.getEnv "LUNA_HOME")
    mainPath <- liftIO $ getCurrentDirectory

    (world, modules, _) <- Project.compileProject (Map.fromList [("Std", stdPath), ("Main", mainPath)]) [["Main", "Main"]]

    main <- Pass.eval' @Project.ProjectCompilation $ do
        let mainModule = Map.lookup ["Main", "Main"] modules
        case mainModule of
            Just unit -> do
                Term (Term.Unit _ _ cls) <- readTerm unit
                klass :: Expr Cls <- unsafeGeneralize <$> source cls
                Term (Term.Cls _ _ _ meths) <- readTerm klass
                main <- mapM (fmap unsafeGeneralize . source) $ Map.lookup "main" meths
                return main
            Nothing -> return Nothing

    let mainFun = case main of
          Just m  -> world ^. functions . at m
          Nothing -> Nothing

    putStrLn "Running main..."

    case mainFun of
        Just f  -> do
            res <- liftIO $ runIO $ runError $ LunaValue.force $ f ^. Function.value
            case res of
                Left err -> error $ "Luna encountered runtime error: " ++ err
                _        -> return ()
        Nothing -> error "Function main not found in module Main"
