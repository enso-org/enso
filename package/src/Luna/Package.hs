module Luna.Package where

import Prologue

import qualified Control.Exception                        as Unsafe
import qualified Control.Exception.Safe                   as Safe
import qualified Control.Monad.Exception                  as Exception
import qualified Control.Monad.Exception.IO               as Exception
import qualified Data.Bimap                               as Bimap
import qualified Data.Map                                 as Map
import qualified Data.Yaml                                as Yaml
import qualified Luna.Package.Configuration.Local         as Local
import qualified Luna.Package.Structure.Generate.Internal as Generate
import qualified Luna.Package.Structure.Name              as Name
import qualified Luna.Package.Structure.Utilities         as Structure
import qualified Luna.Path.Path                           as Path
import qualified OCI.Data.Name                            as Name
import qualified Path                                     as Path
import qualified Path.IO                                  as Path
import qualified System.FilePath.Find                     as Find

import Control.Arrow           ((&&&))
import Control.Monad           (filterM)
import Control.Monad.Exception (MonadExceptions, MonadException)
import Data.Char               (isUpper)
import Data.Bimap              (Bimap)
import Data.Map                (Map)
import Path                    (Path, Abs, Rel, File, Dir, (</>), (<.>))
import Luna.Package.Orphans    ()


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
        "File \"" <> Path.fromAbsFile file
        <> "\" is not a part of any package."
    displayException (PackageRootNotFound dir) =
        show dir <> " is not a Luna Package root."
    displayException (FSError str) = "FSError: " <> str



---------------------------
-- === NameException === --
---------------------------

-- === Definition === --

data RenameException
    = InvalidName       Text
    | InaccessiblePath  (Path Abs Dir)
    | InaccessibleFile  (Path Abs File)
    | DestinationExists (Path Abs Dir)
    | CannotDelete      (Path Abs Dir)  SomeException
    | CannotRenameFile  (Path Abs File) SomeException
    deriving (Generic, Show)


-- === Instances === --

instance Exception RenameException where
    displayException (InvalidName text) = convert text
        <> " is not a valid package name."
    displayException (InaccessiblePath path) = Path.fromAbsDir path
        <> " is inaccessible."
    displayException (InaccessibleFile file) = Path.fromAbsFile file
        <> " is inaccessible."
    displayException (DestinationExists path) = Path.fromAbsDir path
        <> " already exists."
    displayException (CannotDelete path ex) = "Cannot delete"
        <> Path.fromAbsDir path <> ": " <> displayException ex
    displayException (CannotRenameFile file ex) = "Cannot rename "
        <> Path.fromAbsFile file <> ": " <> displayException ex



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

getRelativePathForModule :: (MonadIO m, MonadException Path.PathException m)
    => Path Abs File -> Path Abs File -> m (Maybe (Path Rel File))
getRelativePathForModule pkgFile = Exception.rethrowFromIO @Path.PathException .
    fmap eitherToMaybe . Safe.try . Path.stripProperPrefix
        (Path.parent $ Path.parent pkgFile)
    where
        eitherToMaybe :: Either Path.PathException (Path Rel File)
                      -> Maybe (Path Rel File)
        eitherToMaybe = either (const Nothing) Just

getLunaPackagesFromDir :: (MonadIO m)
    => Path Abs Dir -> m [Path Abs File]
getLunaPackagesFromDir dir = do
    let configDirPath = dir </> Name.configDirectory
    hasConfigDir <- Path.doesDirExist  configDirPath

    if not hasConfigDir then tryConvertPackageFormat dir else do
        (_, files) <- Path.listDirRel configDirPath
        pure . fmap (configDirPath </>) $ filter
            (\file -> Path.fileExtension file == Name.packageExtWithDot) files

tryConvertPackageFormat :: (MonadIO m)
    => Path Abs Dir -> m [Path Abs File]
tryConvertPackageFormat dir = do
    (_, files) <- Path.listDirRel dir
    let configFiles = filter
            (\file -> Path.fileExtension file == Name.packageExt) files
    case configFiles of
        []    -> pure []
        files -> do
            let configDirPath = dir </> Name.configDirectory
            void . liftIO . for files $ \file -> do
                Path.createDirIfMissing True configDirPath
                Path.copyFile
                    (dir </> file)
                    (configDirPath </> file)
                Path.removeFile (dir </> file)
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
    _   -> pure Nothing

getPackageName :: Path Abs Dir -> Name.Name
getPackageName = convert . Path.dirnameNoSlash

mkQualName :: Name.Name -> Path Rel File -> Name.Qualified
mkQualName pkgName file = qualName where
    qualName        = convert $ concat nameParts
    nameParts       = [ [pkgName], convert <$> path, [convert moduleName] ]
    path            = filter (Path.liftPredicate (/= "."))
        $ Path.splitDirectories dir
    moduleName      = Path.dropExtensions filename
    (dir, filename) = (Path.parent file, Path.filename file)

assignQualName :: Path Abs Dir -> Path Abs File
    -> (Path Abs File, Name.Qualified)
assignQualName pkg filePath = (filePath, qualName)
    where
        qualName         = mkQualName (getPackageName pkg) relFileName
        Just relFileName =
            Path.stripProperPrefix (pkg </> Name.sourceDirectory) filePath

findPackageSources :: (MonadIO m, MonadException Path.PathException m)
    => Path Abs Dir -> m (Bimap (Path Abs File) Name.Qualified)
findPackageSources pkg = do
    let srcDir            = Path.fromAbsDir (pkg </> Name.sourceDirectory)
        lunaFilePredicate = Find.extension Find.~~? Name.lunaFileExtWithDot
    lunaFiles    <- liftIO $ Find.find Find.always lunaFilePredicate srcDir
    let lunaFilesAbs = map Path.unsafeParseAbsFile lunaFiles
    let modules  = assignQualName pkg <$> lunaFilesAbs
    pure $ Bimap.fromList modules

listDependencies :: (MonadIO m, MonadException Path.PathException m)
    => Path Abs Dir -> m [(Name.Name, Path Abs Dir)]
listDependencies pkgSrc = do
    let lunaModules     = pkgSrc </> Name.localLibsPath
    dependencies <- liftIO . Safe.tryAny
        $ Path.listDirRel lunaModules
    case dependencies of
        Left _           -> pure []
        Right (directDeps, _) -> do
            indirectDeps <- for directDeps $ \path -> do
                listDependencies (lunaModules </> path)
            pure $ fmap (convert &&& (lunaModules </>)) directDeps
                  <> concat indirectDeps

includedLibs :: MonadIO m => Path Abs Dir -> m [(Name.Name, Path Abs Dir)]
includedLibs stdlibPath = do
    lunaroot     <- Path.canonicalizePath stdlibPath
    projectNames <- do
        (contents, _) <- Path.listDirRel lunaroot
        dirs          <- filterM (\a -> Path.doesDirExist $ lunaroot </> a)
                contents
        let projects = filter (Path.liftPredicate projectNamePred) dirs
        pure projects
    for projectNames $ \projectName ->
        pure (convert projectName, lunaroot </> projectName)
    where
    projectNamePred prjName = (isUpper <$> head prjName) == Just True

packageImportPaths :: (MonadIO m, MonadException Path.PathException m)
    => Path Abs Dir -> Path Abs Dir -> m [(Name.Name, Path Abs Dir)]
packageImportPaths pkgRoot stdlibPath = do
    dependencies    <- listDependencies pkgRoot
    includedImports <- includedLibs stdlibPath
    let importPaths = (getPackageName &&& id) pkgRoot
                    : dependencies
    pure $ includedImports <> importPaths

fileSourcePaths :: ( MonadIO m, MonadThrow m
                   , MonadException Path.PathException m)
    => Path Abs File -> Path Abs Dir -> m (Map Name.Qualified (Path Abs File))
fileSourcePaths lunaFile stdlibPath = do
    fileName    <- liftIO $ Path.setFileExtension "" $ Path.filename lunaFile
    fileImports <- includedLibs stdlibPath

    let importPaths = snd <$> fileImports
    importSources <- sequence $ findPackageSources <$> importPaths

    let projSrcMap = foldl' Map.union Map.empty
            $ Bimap.toMapR <$> importSources
        allSrcMap  = Map.insert (convertVia @Name.Name fileName)
            lunaFile projSrcMap

    pure allSrcMap

isLunaPackage :: (MonadIO m, MonadException Path.PathException m)
    => Path Abs Dir -> m Bool
isLunaPackage path = isJust <$> findPackageRoot path

name :: (MonadIO m, MonadExceptions '[ PackageNotFoundException
                                     , Path.PathException ] m)
    => Path Abs Dir -> m (Path Rel Dir)
name path = do
    findPackageRoot path >>= \case
        Nothing   -> Exception.throw $ PackageRootNotFound path
        -- Safe because Path.fromAbsDir is guaranteed nonempty
        Just root -> pure . unsafeLast . Path.splitDirectories $ root

rename :: ( MonadIO m
          , MonadExceptions '[ PackageNotFoundException
                             , RenameException
                             , Path.PathException] m )
    => Path Abs Dir -> Path Abs Dir -> m (Path Abs Dir, Maybe RenameException)
rename srcPath destPath = do
    srcExists <- Path.doesDirExist srcPath
    unless_ srcExists . Exception.throw $ InaccessiblePath srcPath

    srcIsPackage <- isLunaPackage srcPath
    unless_ srcIsPackage . Exception.throw $ PackageRootNotFound srcPath

    destExists <- Path.doesDirExist destPath
    when_ destExists . Exception.throw $ DestinationExists destPath

    -- Safe as a `Path Abs Dir` cannot be empty
    let newName        = unsafeLast $ Path.splitDirectories destPath
        isValidPkgName = Structure.isValidPkgName newName
    unless_ isValidPkgName . Exception.throw . InvalidName $ convert newName

    -- Guaranteed to be in a package by now so default value is nonsensical
    defaultDirPath  <- Path.getCurrentDir
    srcPackageRoot <- fromJust defaultDirPath <$> findPackageRoot srcPath
    originalName   <- name srcPackageRoot

    -- Determine things to copy and copy/create them

    (dirsToCreate, filesToCopy) <- recursiveListDir srcPath

    for_ dirsToCreate $ \dir -> do
        relPath <- Exception.rethrowFromIO @Path.PathException
            $ Path.stripProperPrefix srcPath dir
        Path.createDirIfMissing True (destPath </> relPath)

    for_ filesToCopy $ \file -> do
        relPath <- Exception.rethrowFromIO @Path.PathException
            $ Path.stripProperPrefix srcPath file
        Path.copyFile file (destPath </> relPath)

    -- Rename the `*.lunaproject` file
    origProjFile <- Exception.rethrowFromIO @Path.PathException $ do
        let file = Path.coerceToFile originalName
        (file <.> Name.packageExt)
    newName      <- name destPath
    newProjFile  <- Exception.rethrowFromIO @Path.PathException $ do
        let file = Path.coerceToFile newName
        file <.> Name.packageExt

    let origProjPath = destPath </> Name.configDirectory </> origProjFile
        newProjPath  = destPath </> Name.configDirectory </> newProjFile

    -- TODO [Ara] Fix MonadException and move back to that Exception.CatchAll
    liftIO . Unsafe.handle (\(e :: SomeException) ->
            Exception.throw $ CannotRenameFile newProjPath e)
        $ Path.renameFile origProjPath newProjPath

    -- Change the name in the `config.yaml` file
    let configDir  = destPath  </> Name.configDirectory
        configPath = configDir </> Name.configFile

    decoded <- liftIO $ Yaml.decodeFileEither (Path.fromAbsFile configPath)

    -- Generates a super default config if none exists

    case decoded of
        Left _    -> liftIO $ Generate.packageConfig Nothing newName
            "" "" configDir
        Right cfg -> liftIO . Yaml.encodeFile (Path.fromAbsFile configPath) $
            cfg & Local.projectName .~ (convert $ Path.fromRelDir newName)

    -- Bubble up an error if the original directory can't be removed.
    liftIO . Unsafe.handle (\(e :: SomeException) ->
            pure (destPath, Just (CannotDelete srcPath e))) $ do
        Path.removeDirRecur srcPath
        pure (destPath, Nothing)

    pure (destPath, Nothing)

recursiveListDir :: (MonadIO m)
    => Path Abs Dir -> m ([Path Abs Dir], [Path Abs File])
recursiveListDir dir = do
    (dirs, files) <- Path.listDir dir
    (recDirs, recFiles) <- fmap unzip $ mapM recursiveListDir dirs
    pure (dirs <> concat recDirs, files <> concat recFiles)

