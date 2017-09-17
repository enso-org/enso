module Luna.Project where

import Luna.Prelude hiding (FilePath)

import Luna.IR.Term.Library
import Luna.IR.Term.Unit as Unit
import Control.Monad.State.Dependent
import OCI
-- import OCI.IR.Name.Path
import OCI.IR.Name.Qualified (QualName)
import qualified OCI.IR.Name.Qualified as Qual

import           Data.Bimap           (Bimap)
import qualified Data.Bimap           as Bimap
import qualified Path                 as Path
import qualified System.Directory     as Dir
import qualified System.FilePath      as FP
import qualified System.FilePath.Find as Find


---------------------
-- === Project === --
---------------------

-- === Management === --

-- openUnitSources :: Path -> m ()
-- openUnitSources = undefined
--
-- closeUnitSources :: Path -> m ()
-- closeUnitSources = undefined
--
-- getUnitSources :: Path -> m Text
-- getUnitSources = undefined

projectSourcesForFile :: Path.Path Path.Abs Path.File -> IO (Maybe (Bimap (Path.Path Path.Abs Path.File) QualName))
projectSourcesForFile file = do
    projectRoot <- findProjectRootForFile file
    mapM findProjectSources projectRoot

lunaProjectExt :: String
lunaProjectExt = ".lunaproject"

findProjectRootForFile :: Path.Path Path.Abs Path.File -> IO (Maybe (Path.Path Path.Abs Path.Dir))
findProjectRootForFile file = findProjectRoot (Path.parent file)

findProjectRoot :: Path.Path Path.Abs Path.Dir -> IO (Maybe (Path.Path Path.Abs Path.Dir))
findProjectRoot dir = do
    filesInDir <- Dir.listDirectory (Path.toFilePath dir)
    files      <- mapM Path.parseRelFile filesInDir

    let lunaProjects = filter (\file -> Path.fileExtension file == lunaProjectExt) files
    case lunaProjects of
        []            -> do
            let parentDir = Path.parent dir
            if parentDir == dir
            then return Nothing
            else findProjectRoot parentDir
        [projectFile] -> return $ Just dir
        _             -> return Nothing

getProjectName :: Path.Path Path.Abs Path.Dir -> Name
getProjectName = convert . FP.takeBaseName . FP.takeDirectory . Path.toFilePath

lunaFileExt :: String
lunaFileExt = ".luna"

mkQualName :: Path.Path Path.Rel Path.File -> QualName
mkQualName file = qualName
    where
        qualName        = Qual.mkQualName (convert path) (convert moduleName)
        path            = filter (/= ".") $ FP.splitDirectories dir
        moduleName      = FP.dropExtensions filename
        (dir, filename) = FP.splitFileName (Path.toFilePath file)

assignQualName :: Path.Path Path.Abs Path.Dir -> Path.Path Path.Abs Path.File -> (Path.Path Path.Abs Path.File, QualName)
assignQualName srcDir filePath = (filePath, qualName)
    where
        qualName         = mkQualName relFileName
        Just relFileName = Path.stripDir srcDir filePath

sourceDirectory :: Path.Path Path.Rel Path.Dir
sourceDirectory = $(Path.mkRelDir "src")

findProjectSources :: Path.Path Path.Abs Path.Dir -> IO (Bimap (Path.Path Path.Abs Path.File) QualName)
findProjectSources project = do
    let srcDir            = project Path.</> sourceDirectory
        lunaFilePredicate = Find.extension Find.~~? lunaFileExt
    lunaFiles    <- Find.find Find.always lunaFilePredicate (Path.toFilePath srcDir)
    lunaFilesAbs <- mapM Path.parseAbsFile lunaFiles
    let modules  = map (assignQualName srcDir) lunaFilesAbs
    return $ Bimap.fromList modules
