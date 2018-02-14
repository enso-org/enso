{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE OverloadedLists    #-}

module Luna.Compilation where

import           Luna.Prelude        hiding (String, seq, cons, Constructor)
import qualified Luna.Prelude        as P
import qualified Data.Map            as Map
import           Data.Map            (Map)
import qualified Data.Set            as Set
import qualified Data.TreeSet        as TreeSet
import qualified Data.Bimap          as Bimap
import           Control.Monad.Raise
import           Control.Monad.State  (runStateT, MonadState)
import           Control.Monad.Except (runExceptT, MonadError, throwError)

import Luna.Std (stdlib, stdlibImports)
import qualified OCI.Pass           as Pass
import           OCI.Pass           (SubPass, Preserves, Inputs, Outputs)
import qualified OCI.IR.Repr.Vis    as Vis
import OCI.IR.Name.Qualified
import Luna.IR
import Luna.IR.Term.Unit (UnitSet)
import Luna.IR.Term.Cls  (Cls)
import qualified Luna.IR.Term.Unit as Term
import qualified Luna.IR.Term.Unit as Unit
import qualified Luna.IR.Term.Cls  as Term
import Luna.Builtin.Data.Module     as Module
import Luna.Builtin.Data.Class
import Luna.Builtin.Data.LunaEff
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
import qualified Data.Text.Position               as Pos

import Luna.Test.IR.Runner
import Data.TypeDesc
import System.IO.Unsafe

import qualified Luna.Pass.Transform.Desugaring.RemoveGrouped  as RemoveGrouped
import qualified Luna.Pass.UnitCompilation.ModuleProcessing    as ModuleProcessing
import qualified Luna.Pass.Sourcing.UnitLoader as UL
import           Luna.Syntax.Text.Parser.Errors      (Invalids)

import qualified Luna.Project       as Project
import           System.Directory   (getCurrentDirectory)
import qualified Path               as Path
import qualified System.Environment as Env

import System.Log (dropLogs)

data ProjectCompilation
type instance Abstract ProjectCompilation = ProjectCompilation
type instance Inputs  Net   ProjectCompilation = '[AnyExpr]
type instance Outputs Net   ProjectCompilation = '[AnyExpr]
type instance Inputs  Layer ProjectCompilation = '[AnyExpr // Model, AnyExpr // UID, Link' AnyExpr // UID, Link' AnyExpr // Model, AnyExpr // Succs]
type instance Outputs Layer ProjectCompilation = '[]
type instance Inputs  Attr  ProjectCompilation = '[Parser.ReparsingStatus, WorldExpr]
type instance Outputs Attr  ProjectCompilation = '[]
type instance Inputs  Event ProjectCompilation = '[]
type instance Outputs Event ProjectCompilation = '[New // AnyExpr]
type instance Preserves     ProjectCompilation = '[]

data ModuleCompilationError = ModuleSourcesNotFound QualName
                            | ImportsCycleError [QualName]
                            deriving (Show)

data CompiledModules = CompiledModules { _modules :: Map QualName Imports
                                       , _prims   :: Imports
                                       }
makeLenses ''CompiledModules

instance Default CompiledModules where
    def = CompiledModules def def

initPM = do
    runRegs

    Loc.init
    attachLayer 5 (getTypeDesc @Pos.Range)         (getTypeDesc @AnyExpr)
    CodeSpan.init
    attachLayer 5 (getTypeDesc @CodeSpan.CodeSpan) (getTypeDesc @AnyExpr)
    initNameGen
    setAttr (getTypeDesc @Parser.MarkedExprMap)   $ (mempty :: Parser.MarkedExprMap)
    setAttr (getTypeDesc @Parser.ParsedExpr)      $ (error "Data not provided: ParsedExpr")
    setAttr (getTypeDesc @Parser.ReparsingStatus) $ (mempty :: Parser.ReparsingStatus)
    setAttr (getTypeDesc @WorldExpr)     (undefined :: WorldExpr)
    setAttr (getTypeDesc @Source.Source) (undefined :: Source.Source)
    setAttr (getTypeDesc @Invalids)               $ (mempty :: Invalids)
    setAttr (getTypeDesc @UnitSet)                  (undefined :: UnitSet)
    setAttr (getTypeDesc @UL.UnitsToLoad)           (mempty    :: UL.UnitsToLoad)
    setAttr (getTypeDesc @UL.SourcesManager)        (undefined :: UL.SourcesManager)
    Pass.eval' initWorld

prepareStdlib :: Map Name FilePath -> IO (IO (), CompiledModules)
prepareStdlib srcs = mdo
    let system  = Imports def $ Function.WithDocumentation def . Right <$> std
        initial = CompiledModules def system
    (cln, std) <- stdlib $ unionsImports $ Map.elems $ modules
    Right (_, res@(CompiledModules modules _)) <- requestModules srcs stdlibImports initial
    return (cln, res)

requestModules :: Map Name FilePath -> [QualName] -> CompiledModules -> IO (Either ModuleCompilationError (Map QualName Imports, CompiledModules))
requestModules libs modules cached = runEitherT $ flip runStateT cached $ requestModules' libs modules

requestModules' :: (MonadState CompiledModules m, MonadError ModuleCompilationError m, MonadIO m) => Map Name FilePath -> [QualName] -> m (Map QualName Imports)
requestModules' libs modules = do
    sources <- liftIO $ mapM (Project.findProjectSources <=< Path.parseAbsDir) libs
    let mkSourcesMap libName sources = foldl (\m (p, n) -> Map.insert (fromList . (libName :) . toList $ n) (UL.Source (Path.toFilePath p) def) m) def (Bimap.toList sources)
        sourcesMgr = UL.fsSourceManager $ Map.unions $ uncurry mkSourcesMap <$> Map.toList sources
    res <- mapM (getOrCompileModule sourcesMgr []) modules
    return $ Map.fromList $ zip modules res

getOrCompileModule :: (MonadState CompiledModules m, MonadError ModuleCompilationError m, MonadIO m) => UL.SourcesManager -> [QualName] -> QualName -> m Imports
getOrCompileModule srcs stack current = do
    scope <- use modules
    case scope ^? ix current of
        Just i -> return i
        _      -> requestModule srcs stack current

requestModule :: (MonadState CompiledModules m, MonadError ModuleCompilationError m, MonadIO m) => UL.SourcesManager -> [QualName] -> QualName -> m Imports
requestModule srcs stack current = do
    putStrLn $ "Requested module: " <> convert current
    case dropWhile (/= current) stack of
        [] -> return ()
        _  -> throwError $ ImportsCycleError (current : takeWhile (/= current) stack)

    codeE <- dropLogs $ srcs ^. UL.readCode $ current
    code  <- case codeE of
        Left  _ -> throwError $ ModuleSourcesNotFound current
        Right c -> return c
    Right dependencies <- liftIO $ runPM False $ do
        initPM
        Pass.eval' @UL.UnitLoader $ do
            u      <- UL.parseUnit code
            imphub <- u @^. Term.imports
            imps   <- readWrappedSources (unsafeGeneralize imphub :: UL.UnresolvedImportHubType)
            forM imps $ \imp -> do
                src <- imp @^. Term.termUnresolvedImport_source
                Term.Absolute path <- src @. wrapped
                return path
    let baseIncludedDeps = if "Std.Base" == current
                           then dependencies
                           else Set.toList $ Set.insert "Std.Base" $ Set.fromList dependencies

    deps <- fmap Map.fromList $ forM baseIncludedDeps $ \dep -> do
        scope <- use modules
        case scope ^? ix dep of
            Just i  -> return (dep, i)
            Nothing -> (dep,) <$> requestModule srcs (current : stack) dep
    std <- use prims
    putStrLn $ "Compiling module: " <> convert current
    Right mod <- liftIO $ runPM False $ do
        initPM
        u <- Pass.eval' @UL.UnitLoader $ do
            u   <- UL.parseUnit code
            cls <- u @^. Unit.cls
            UL.partitionASGCls (unsafeGeneralize cls :: Expr ClsASG)
            return u
        ModuleProcessing.processModule (unionsImports $ std : Map.elems deps) (convert current) u
    modules . at current .= Just mod
    return mod
