{-# LANGUAGE OverloadedStrings #-}
module Luna.Project where

import Luna.Prelude hiding (FilePath)

import Luna.IR.Term.Unit as Unit
import Control.Monad.State.Dependent
import OCI
import OCI.IR.Name.Qualified (QualName)
import qualified OCI.IR.Name.Qualified as Qual

import           Control.Arrow          ((&&&))
import           Control.Exception.Safe (try, tryAny)
import           Data.Bimap             (Bimap)
import qualified Data.Bimap             as Bimap
import           Path                   (Path, Abs, Rel, File, Dir, (</>))
import qualified Path                   as Path
import qualified System.Directory       as Dir
import           System.Environment     (getEnv)
import qualified System.FilePath        as FP
import qualified System.FilePath.Find   as Find


---------------------
-- === Project === --
---------------------

projectSourcesForFile :: Path Abs File
                      -> IO (Maybe (Bimap (Path Abs File) QualName))
projectSourcesForFile file = do
    projectRoot <- findProjectRootForFile file
    mapM findProjectSources projectRoot

lunaProjectExt :: String
lunaProjectExt = ".lunaproject"

findProjectRootForFile :: Path Abs File -> IO (Maybe (Path Abs Dir))
findProjectRootForFile = findProjectRoot . Path.parent

data ProjectNotFoundException = ProjectNotFoundException (Path Abs File)
    deriving Show

instance Exception ProjectNotFoundException where
    displayException (ProjectNotFoundException file) =
        "File \"" <> Path.toFilePath file <> "\" is not a part of any project."

projectRootForFile :: Path Abs File -> IO (Path Abs Dir)
projectRootForFile file = do
    maybeRoot <- findProjectRoot $ Path.parent file
    maybe (throwM (ProjectNotFoundException file)) return maybeRoot

findProjectFileForFile :: Path Abs File -> IO (Maybe (Path Abs File))
findProjectFileForFile = findProjectFile . Path.parent

getRelativePathForModule :: Path Abs File
                         -> Path Abs File
                         -> IO (Maybe (Path Rel File))
getRelativePathForModule projectFile =
    fmap eitherToMaybe . try . Path.stripProperPrefix (Path.parent projectFile)
    where
        eitherToMaybe :: Either Path.PathException (Path Rel File)
                      -> Maybe (Path Rel File)
        eitherToMaybe = either (const Nothing) Just

getLunaProjectsFromDir :: Path Abs Dir -> IO [Path Abs File]
getLunaProjectsFromDir dir = do
    filesInDir <- Dir.listDirectory (Path.toFilePath dir)
    files      <- mapM Path.parseRelFile filesInDir
    return . map (dir </>)
           $ filter (\file -> Path.fileExtension file == lunaProjectExt) files

findProjectFile :: Path Abs Dir -> IO (Maybe (Path Abs File))
findProjectFile dir = getLunaProjectsFromDir dir >>= \case
    [] -> let parentDir = Path.parent dir in
        if parentDir == dir then return Nothing else findProjectFile parentDir
    [projectFile] -> return $ Just projectFile
    _             -> return Nothing

findProjectRoot :: Path Abs Dir -> IO (Maybe (Path Abs Dir))
findProjectRoot dir = getLunaProjectsFromDir dir >>= \case
    [] -> let parentDir = Path.parent dir in
        if parentDir == dir then return Nothing else findProjectRoot parentDir
    [projectFile] -> return $ Just dir
    _             -> return Nothing

getProjectName :: Path Abs Dir -> Name
getProjectName = convert . FP.takeBaseName . FP.takeDirectory . Path.toFilePath

lunaFileExt :: String
lunaFileExt = ".luna"

mkQualName :: Name -> Path Rel File -> QualName
mkQualName projectName file = qualName
    where
        qualName        = Qual.mkQualName
                              (convert (convert projectName : path))
                              (convert moduleName)
        path            = filter (/= ".") $ FP.splitDirectories dir
        moduleName      = FP.dropExtensions filename
        (dir, filename) = FP.splitFileName (Path.toFilePath file)

assignQualName :: Path Abs Dir -> Path Abs File -> (Path Abs File, QualName)
assignQualName project filePath = (filePath, qualName)
    where
        qualName         = mkQualName (getProjectName project) relFileName
        Just relFileName = Path.stripDir (project </> sourceDirectory) filePath

sourceDirectory :: Path Rel Dir
sourceDirectory = $(Path.mkRelDir "src")

findProjectSources :: Path Abs Dir -> IO (Bimap (Path Abs File) QualName)
findProjectSources project = do
    let srcDir            = Path.toFilePath $ project </> sourceDirectory
        lunaFilePredicate = Find.extension Find.~~? lunaFileExt
    lunaFiles    <- Find.find Find.always lunaFilePredicate srcDir
    lunaFilesAbs <- mapM Path.parseAbsFile lunaFiles
    let modules  = map (assignQualName project) lunaFilesAbs
    return $ Bimap.fromList modules

localLibsPath :: Path Rel Dir
localLibsPath = $(Path.mkRelDir "local_libs")

listDependencies :: Path Abs Dir -> IO [(Name, FP.FilePath)]
listDependencies projectSrc = do
    let lunaModules     = projectSrc </> localLibsPath
        lunaModulesPath = Path.toFilePath lunaModules
    dependencies <- tryAny $ Dir.listDirectory lunaModulesPath
    case dependencies of
        Left exc         -> return []
        Right directDeps -> do
            indirectDeps <- forM directDeps $ \proj -> do
                path <- Path.parseRelDir proj
                listDependencies (lunaModules </> path)
            return $ map (convert &&& (lunaModulesPath FP.</>)) directDeps
                  <> concat indirectDeps

projectImportPaths :: Path Abs Dir -> IO [(Name, FP.FilePath)]
projectImportPaths projectRoot = do
    lunaroot     <- Dir.canonicalizePath =<< getEnv lunaRootEnv
    dependencies <- listDependencies projectRoot
    let importPaths = ("Std", lunaroot <> "/Std/")
                    : (getProjectName &&& Path.toFilePath) projectRoot
                    : dependencies
    return importPaths

lunaRootEnv :: String
lunaRootEnv = "LUNA_LIBS_PATH"
