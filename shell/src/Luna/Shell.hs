{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE CPP                #-}

module Luna.Shell where

import           Luna.Prelude        hiding (String, seq, cons, Constructor)
import qualified Luna.Prelude        as P
import qualified Data.Map            as Map
import           Data.Map            (Map)
import qualified Data.TreeSet        as TreeSet
import qualified Data.Bimap          as Bimap

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

import Luna.IR.Layer.Errors
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

import qualified Luna.Project                  as Project
import qualified Luna.Compilation              as Project
import           System.Directory              (doesDirectoryExist, getCurrentDirectory)
import qualified System.FilePath               as FilePath
import           System.FilePath               (FilePath)
import qualified System.Environment            as Env
import           System.Environment.Executable (splitExecutablePath)
import           System.Exit                   (die)

import Data.Layout as Layout
import qualified Data.Text.Terminal as Terminal
import qualified Path as Path

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

errorsEnumerator :: Doc Terminal.TermText
#ifdef mingw32_HOST_OS
errorsEnumerator = "-"
#else
errorsEnumerator = "•"
#endif

stackItemEnumerator :: Doc Terminal.TermText
#ifdef mingw32_HOST_OS
stackItemEnumerator = "-"
#else
stackItemEnumerator = "↳ "
#endif

colon :: Doc Terminal.TermText
colon = ":"

formatStack :: [ModuleTagged ErrorSource] -> Doc Terminal.TermText
formatStack items = enumsCol Layout.<+> modsCol Layout.<+> colonsCol Layout.<+> defCol where
    colonsCol = Layout.nested $ foldl (</>) mempty (colon <$ items)
    enumsCol  = Layout.nested $ foldl (</>) mempty (stackItemEnumerator <$ items)

    modsCol  = Layout.nested $ foldl (</>) mempty (convert      . view moduleTag <$> items)
    defCol   = Layout.nested $ foldl (</>) mempty (formatSource . view contents  <$> items)

    formatSource (FromFunction n) = convert n
    formatSource (FromMethod c n) = convert c <> "." <> convert n

formatError :: CompileError -> Doc Terminal.TermText
formatError (CompileError txt reqStack stack) = Layout.nested (convert txt) </> (Layout.indented $ Layout.nested $ arisingBlock </> requiredBlock) where
    arisingFrom   = "Arising from:"
    requiredBy    = "Required by:"
    arisingStack  = Layout.nested (formatStack $ reverse stack)
    requiredStack = Layout.nested (formatStack reqStack)
    arisingBlock  = arisingFrom </> Layout.indented arisingStack
    requiredBlock = if null reqStack then Layout.phantom else requiredBy  </> Layout.indented requiredStack

formatErrors :: [CompileError] -> Doc Terminal.TermText
formatErrors errs = foldl (<//>) mempty items where
    items = Layout.nested . (errorsEnumerator Layout.<+>) . formatError <$> errs


stdlibPath :: IO FilePath
stdlibPath = do
    env     <- Map.fromList <$> Env.getEnvironment
    exePath <- fst <$> splitExecutablePath
    let (<</>>)        = (FilePath.</>)  -- purely for convenience, because </> is defined elswhere
        parent         = let p = FilePath.takeDirectory in \x -> if FilePath.hasTrailingPathSeparator x then p (p x) else p x
        defaultStdPath = (parent . parent . parent $ exePath) <</>> "config" <</>> "env"
        envStdPath     = Map.lookup "LUNA_HOME" env
        stdPath        = fromMaybe defaultStdPath envStdPath <</>> "Std"
    exists <- doesDirectoryExist stdPath
    if exists
        then putStrLn $ "Found the standard library at: " <> stdPath
        else die "Standard library not found. Set the LUNA_HOME environment variable"
    return stdPath

main :: IO ()
main = do
    mainPath' <- getCurrentDirectory
    mainPath  <- Path.parseAbsDir mainPath'
    let mainName = Project.getProjectName mainPath
    stdPath   <- stdlibPath
    (_, std)  <- Project.prepareStdlib  (Map.fromList [("Std", stdPath)])
    dependencies <- Project.listDependencies mainPath
    let libs = Map.fromList $ [("Std", stdPath), (mainName, mainPath')] ++ dependencies
    Right (_, imp) <- Project.requestModules libs [[mainName, "Main"]] std
    let mainFun = imp ^? Project.modules . ix [mainName, "Main"] . importedFunctions . ix "main" . Function.documentedItem
    case mainFun of
        Just (Left e)  -> do
            putStrLn "Luna encountered the following compilation errors:"
            Terminal.putStrLn $ Layout.concatLineBlock $ Layout.render $ formatErrors e
            putStrLn ""
            liftIO $ die "Compilation failed."
        Just (Right f) -> do
            putStrLn "Running main..."
            res <- liftIO $ runIO $ runError $ LunaValue.force $ f ^. Function.value
            case res of
                Left err -> error $ "Luna encountered runtime error: " ++ err
                _        -> return ()
        Nothing -> error "Function main not found in module Main."
