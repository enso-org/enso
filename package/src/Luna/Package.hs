module Luna.Package where

import Prologue

import qualified Control.Exception.Safe           as SafeException
import qualified Control.Monad.Exception          as Exception
import qualified Control.Monad.Exception.IO       as Exception
import qualified Data.Bimap                       as Bimap
import qualified Data.Map                         as Map
import qualified Data.Yaml                        as Yaml
import qualified Luna.Package.Configuration.Local as Local
import qualified Luna.Package.Structure.Name      as Name
import qualified Luna.Package.Structure.Utilities as Structure
import qualified OCI.Data.Name                    as Name
import qualified Path                             as Path
import qualified System.Directory                 as Directory
import qualified System.Environment               as Environment
import qualified System.FilePath                  as FilePath
import qualified System.FilePath.Find             as Find

import Control.Arrow           ((&&&))
import Control.Monad.Exception (MonadExceptions, MonadException)
import Data.Bimap              (Bimap)
import Data.Map                (Map)
import Path                    (Path, Abs, Rel, File, Dir, (</>))



--------------------------------------
-- === PackageNotFoundException === --
--------------------------------------

-- === Definition === --

data PackageNotFoundException
    = PackageNotFound (Path Abs File)
    | PackageRootNotFound (Path Abs Dir)
    | FSError String
    deriving Show


-- === Instances === --

instance Exception PackageNotFoundException where
    displayException (PackageNotFound file) =
        "File \"" <> Path.toFilePath file <> "\" is not a part of any package."
    displayException (PackageRootNotFound dir) =
        show dir <> " is not a Luna Package root."
    displayException (FSError str) = "FSError: " <> str



---------------------------
-- === NameException === --
---------------------------

-- === Definition === --

data RenameException
    = InvalidName Text
    | InaccessiblePath (Path Abs Dir)
    | InaccessibleFile (Path Abs File)
    | DestinationExists (Path Abs Dir)
    deriving (Eq, Generic, Ord, Show)


-- === Instances === --

instance Exception RenameException where
    displayException (InvalidName text) = convert text
        <> " is not a valid package name."
    displayException (InaccessiblePath path) = show path <> " is inaccessible."
    displayException (InaccessibleFile file) = show file <> " is inaccessible."
    displayException (DestinationExists path) = show path <> " already exists."


-----------------
-- === API === --
-----------------

packageSourcesForFile :: (MonadIO m, MonadException Path.PathException m)
    => Path Abs File -> m (Maybe (Bimap (Path Abs File) Name.Qualified))
packageSourcesForFile file = do
    packageRoot <- findPackageRootForFile file
    mapM findPackageSources packageRoot

findPackageRootForFile :: (MonadIO m, MonadException Path.PathException m)
    => Path Abs File -> m (Maybe (Path Abs Dir))
findPackageRootForFile = findPackageRoot . Path.parent

packageRootForFile :: ( MonadIO m, MonadExceptions '[ PackageNotFoundException
                                                    , Path.PathException ] m )
    => Path Abs File -> m (Path Abs Dir)
packageRootForFile file = do
    maybeRoot <- findPackageRoot $ Path.parent file
    maybe (Exception.throw (PackageNotFound file)) pure maybeRoot

findPackageFileForFile :: (MonadIO m, MonadException Path.PathException m)
    => Path Abs File -> m (Maybe (Path Abs File))
findPackageFileForFile = findPackageFile . Path.parent

getRelativePathForModule :: (MonadIO m, MonadCatch m) => Path Abs File
    -> Path Abs File -> m (Maybe (Path Rel File))
getRelativePathForModule packageFile =
    fmap eitherToMaybe . SafeException.try . Path.stripProperPrefix
        (Path.parent $ Path.parent packageFile)
    where
        eitherToMaybe :: Either Path.PathException (Path Rel File)
                      -> Maybe (Path Rel File)
        eitherToMaybe = either (const Nothing) Just

getLunaPackagesFromDir :: (MonadIO m, MonadException Path.PathException m)
    => Path Abs Dir -> m [Path Abs File]
getLunaPackagesFromDir dir = Exception.rethrowFromIO @Path.PathException $ do
    let configDirPath = dir </> Name.configDirectory
    hasConfigDir  <- liftIO . Directory.doesDirectoryExist $ Path.toFilePath
        configDirPath

    if not hasConfigDir then tryConvertPackageFormat dir else do
        filesInDir <- liftIO $ Directory.listDirectory
            (Path.toFilePath configDirPath)
        files      <- mapM Path.parseRelFile filesInDir

        pure . fmap (configDirPath </>) $ filter
            (\file -> Path.fileExtension file == Name.packageExt) files

tryConvertPackageFormat :: (MonadIO m, MonadException Path.PathException m)
    => Path Abs Dir -> m [Path Abs File]
tryConvertPackageFormat dir = Exception.rethrowFromIO @Path.PathException $ do
    filesInDir <- liftIO $ Directory.listDirectory (Path.toFilePath dir)
    files      <- mapM Path.parseRelFile filesInDir
    let configFiles = filter
            (\file -> Path.fileExtension file == Name.packageExt) files
    case configFiles of
        []    -> pure []
        files -> do
            let configDirPath = dir </> Name.configDirectory
            void . liftIO . for files $ \file -> do
                Directory.createDirectoryIfMissing True $
                    Path.toFilePath configDirPath
                Directory.copyFile
                    (Path.toFilePath $ dir </> file)
                    (Path.toFilePath $ configDirPath </> file)
                SafeException.tryAny $ Directory.removeFile
                    (Path.toFilePath $ dir </> file)
            pure $ map (configDirPath </>) files

findPackageFile :: (MonadIO m, MonadException Path.PathException m)
    => Path Abs Dir -> m (Maybe (Path Abs File))
findPackageFile dir = getLunaPackagesFromDir dir >>= \case
    [] -> let parentDir = Path.parent dir in
        if parentDir == dir then pure Nothing else findPackageFile parentDir
    [packageFile] -> pure $ Just packageFile
    _             -> pure Nothing

findPackageRoot :: (MonadIO m, MonadException Path.PathException m)
    => Path Abs Dir -> m (Maybe (Path Abs Dir))
findPackageRoot dir = getLunaPackagesFromDir dir >>= \case
    [] -> let parentDir = Path.parent dir in
        if parentDir == dir then pure Nothing else findPackageRoot parentDir
    [_] -> pure $ Just dir
    _             -> pure Nothing

getPackageName :: Path Abs Dir -> Name.Name
getPackageName =
    convert . FilePath.takeBaseName . FilePath.takeDirectory . Path.toFilePath

mkQualName :: Name.Name -> Path Rel File -> Name.Qualified
mkQualName pkgName file = qualName where
    qualName        = convert $ concat nameParts
    nameParts       = [ [pkgName], convert <$> path, [convert moduleName] ]
    path            = filter (/= ".") $ FilePath.splitDirectories dir
    moduleName      = FilePath.dropExtensions filename
    (dir, filename) = FilePath.splitFileName (Path.toFilePath file)

assignQualName :: Path Abs Dir -> Path Abs File
    -> (Path Abs File, Name.Qualified)
assignQualName pkg filePath = (filePath, qualName)
    where
        qualName         = mkQualName (getPackageName pkg) relFileName
        Just relFileName =
            Path.stripProperPrefix (pkg </> Name.sourceDirectory) filePath

findPackageSources :: (MonadIO m, MonadException Path.PathException m)
    => Path Abs Dir -> m (Bimap (Path Abs File) Name.Qualified)
findPackageSources pkg = Exception.rethrowFromIO @Path.PathException $ do
    let srcDir            = Path.toFilePath $ pkg </> Name.sourceDirectory
        lunaFilePredicate = Find.extension Find.~~? Name.lunaFileExt
    lunaFiles    <- liftIO $ Find.find Find.always lunaFilePredicate srcDir
    lunaFilesAbs <- mapM Path.parseAbsFile lunaFiles
    let modules  = assignQualName pkg <$> lunaFilesAbs
    pure $ Bimap.fromList modules

listDependencies :: (MonadIO m, MonadException Path.PathException m)
    => Path Abs Dir -> m [(Name.Name, FilePath.FilePath)]
listDependencies pkgSrc = Exception.rethrowFromIO @Path.PathException $ do
    let lunaModules     = pkgSrc </> Name.localLibsPath
        lunaModulesPath = Path.toFilePath lunaModules
    dependencies <- liftIO . SafeException.tryAny
        $ Directory.listDirectory lunaModulesPath
    case dependencies of
        Left _           -> pure []
        Right directDeps -> do
            indirectDeps <- for directDeps $ \proj -> do
                path <- Path.parseRelDir proj
                listDependencies (lunaModules </> path)
            pure $ fmap (convert &&& (lunaModulesPath FilePath.</>)) directDeps
                  <> concat indirectDeps

packageImportPaths :: (MonadIO m, MonadException Path.PathException m)
    => Path Abs Dir -> m [(Name.Name, FilePath.FilePath)]
packageImportPaths pkgRoot = do
    lunaroot     <- liftIO $ Directory.canonicalizePath
        =<< Environment.getEnv Name.lunaRootEnv
    dependencies <- listDependencies pkgRoot
    let importPaths = ("Std", lunaroot <> "/Std/")
                    : (getPackageName &&& Path.toFilePath) pkgRoot
                    : dependencies
    pure importPaths

fileSourcePaths :: (MonadIO m, MonadException Path.PathException m)
    => Path Abs File -> m (Map Name.Qualified FilePath.FilePath)
fileSourcePaths lunaFile = Exception.rethrowFromIO @Path.PathException $ do
    lunaRoot <- liftIO $ Directory.canonicalizePath
        =<< Environment.getEnv Name.lunaRootEnv
    let fileName    = FilePath.dropExtension . Path.fromRelFile
            $ Path.filename lunaFile
        fileImports :: [(Name.Name, FilePath.FilePath)]
        fileImports = [("Std", lunaRoot <> "/Std/")]

    importPaths   <- sequence $ Path.parseAbsDir . snd <$> fileImports
    importSources <- sequence $ findPackageSources <$> importPaths

    let projSrcMap = Map.map Path.toFilePath $ foldl' Map.union Map.empty
            $ Bimap.toMapR <$> importSources
        allSrcMap  = Map.insert (convertVia @Name.Name fileName)
            (Path.toFilePath lunaFile) projSrcMap

    pure allSrcMap

isLunaPackage :: (MonadIO m, MonadException Path.PathException m)
    => Path Abs Dir -> m Bool
isLunaPackage path = isJust <$> findPackageRoot path

name :: (MonadIO m, MonadExceptions '[ PackageNotFoundException
                                     , Path.PathException ] m)
    => Path Abs Dir -> m Text
name path = findPackageRoot path >>= \case
    Nothing   -> Exception.throw $ PackageRootNotFound path
    -- Safe because Path.fromAbsDir is guaranteed nonempty
    Just root -> pure . convert . unsafeLast . FilePath.splitDirectories
        $ Path.fromAbsDir root

rename :: ( MonadIO m
          , MonadExceptions '[ PackageNotFoundException
                             , RenameException
                             , Path.PathException] m )
    => Path Abs Dir -> Path Abs Dir -> m (Path Abs Dir)
rename src target = Exception.rethrowFromIO @Path.PathException $ do
    let srcPath  = Path.fromAbsDir src
        destPath = Path.fromAbsDir target

    srcExists <- liftIO $ Directory.doesDirectoryExist srcPath
    unless_ srcExists . Exception.throw $ InaccessiblePath src

    srcIsPackage <- isLunaPackage src
    unless_ srcIsPackage . Exception.throw $ PackageRootNotFound src

    destExists <- liftIO $ Directory.doesDirectoryExist destPath
    when_ destExists . Exception.throw $ DestinationExists target

    -- Safe as a `Path Abs Dir` cannot be empty
    let newName        = unsafeLast $ FilePath.splitDirectories destPath
        isValidPkgName = Structure.isValidPkgName newName
    unless_ isValidPkgName . Exception.throw . InvalidName $ convert newName

    -- Guaranteed to be in a package by now so default value is nonsensical
    defaultDir     <- Directory.getCurrentDirectory
    defaultDirPath <- Path.parseAbsDir defaultDir
    srcPackageRoot <- fromJust defaultDirPath <$> findPackageRoot src
    originalName   <- name srcPackageRoot

    liftIO $ Directory.renameDirectory (Path.fromAbsDir srcPackageRoot) destPath

    -- Rename the `*.lunaproject` file
    origProjFile <- Path.parseRelFile $ convert originalName <> Name.packageExt
    newName      <- name target
    newProjFile  <- Path.parseRelFile $ convert newName <> Name.packageExt

    let origProjPath = target </> Name.configDirectory </> origProjFile
        newProjPath  = target </> Name.configDirectory </> newProjFile

    liftIO $ Directory.renameFile (Path.fromAbsFile origProjPath)
        (Path.fromAbsFile newProjPath)

    -- Change the name in the `config.yaml` file
    configName <- Path.parseRelFile Name.configFile
    let configPath = target </> Name.configDirectory </> configName

    Yaml.decodeFileEither (Path.fromAbsFile configPath) >>= \case
        Left _    -> Exception.throw $ InaccessibleFile configPath
        Right cfg -> Yaml.encodeFile (Path.fromAbsFile configPath) $
            cfg & Local.projectName .~ newName

    pure target

