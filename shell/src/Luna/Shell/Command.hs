module Luna.Shell.Command where

import Prologue hiding (init)

import qualified Control.Exception                  as Exception
import qualified Control.Monad.Exception            as MException
import qualified Control.Monad.Exception.IO         as MException
import qualified Control.Monad.State.Layered        as State
import qualified Data.Version                       as Version
import qualified Data.Yaml                          as Yaml
import qualified GitHash                            as GitHash
import qualified Luna.Package                       as Package
import qualified Luna.Package.Configuration.Global  as Global
import qualified Luna.Package.Configuration.License as License
import qualified Luna.Package.Configuration.Local   as Local
import qualified Luna.Package.Structure.Generate    as Generate
import qualified Luna.Package.Structure.Name        as Package
import qualified Luna.Shell.CWD                     as CWD
import qualified Luna.Shell.Interpret               as Interpret
import qualified Luna.Shell.GenerateDocumentation   as GenerateDocumentation
import qualified Path                               as Path
import qualified System.Directory                   as Directory
import qualified System.Info                        as Info
import qualified Text.Megaparsec                    as Megaparsec

import Control.Lens.Prism      (_Just)
import Control.Monad.Exception (MonadException)
import Path                    (Path, Abs, Dir)
import System.Exit             (die)
import System.FilePath         ((</>))
import System.IO               (hPutStrLn, stderr)


-------------------------------
-- === Config State Monad == --
-------------------------------

-- === Definition === --
type ConfigStateIO m =
    ( MonadIO m
    , State.MonadStates '[Global.Config, Local.Config] m)



----------------------------------
-- === Command Option Types === --
----------------------------------

-- === Definition === --

newtype RunOpts = RunOpts
    { _target :: FilePath
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''RunOpts

data DocumentOpts = DocumentOpts
    { __target :: FilePath
    , __out    :: FilePath
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''DocumentOpts

data InitOpts = InitOpts
    { _name            :: String
    , _lunaVersion     :: String
    , _licenseOverride :: String
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''InitOpts

data RenameOpts = RenameOpts
    { _srcName  :: String
    , _destName :: String
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''RenameOpts

data BuildOpts = BuildOpts
    { __acquireDeps        :: Bool
    , __cleanBuild         :: Bool
    , __standaloneFileName :: String
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''BuildOpts

data TestOpts = TestOpts
    { __doNotBuild  :: Bool
    , __noBenchmark :: Bool
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''TestOpts

data CleanOpts = CleanOpts
    { __full  :: Bool
    , __docs  :: Bool
    , __cache :: Bool
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''CleanOpts

data PublishOpts = PublishOpts
    { __bumpMajor :: Bool
    , __bumpMinor :: Bool
    , __bumpPatch :: Bool
    , __prerelease :: String
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''PublishOpts

newtype RetractOpts = RetractOpts
    { __version :: String
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''RetractOpts

newtype OptionOpts = OptionOpts
    { __options :: [String]
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''OptionOpts

newtype RollbackOpts = RollbackOpts
    { __hash :: String
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''RollbackOpts

newtype UpdateOpts = UpdateOpts
    { __dependencyName :: [String]
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''UpdateOpts

newtype FreezeOpts = FreezeOpts
    { __dependencyName :: [String]
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''FreezeOpts

newtype InstallOpts = InstallOpts
    { __packages :: [String]
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''InstallOpts

newtype DownloadOpts = DownloadOpts
    { __packages :: [String]
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''DownloadOpts

data Command
    = Build BuildOpts
    | Clean CleanOpts
    | Document DocumentOpts
    | Download DownloadOpts
    | Freeze FreezeOpts
    | Init InitOpts
    | Install InstallOpts
    | Options OptionOpts
    | Publish PublishOpts
    | Rename RenameOpts
    | Retract RetractOpts
    | Rollback RollbackOpts
    | Run RunOpts
    | Test TestOpts
    | Unfreeze FreezeOpts
    | Update UpdateOpts
    | None
    deriving (Eq, Generic, Ord, Show)

data CommandOpts
    = ShowVersion
    | Exec Command
    deriving (Eq, Generic, Ord, Show)



-------------------------------
-- === Command Execution === --
-------------------------------

-- === API === --

run :: ConfigStateIO m => RunOpts -> m ()
run (RunOpts target) = liftIO $ catch compute recover where
    compute =
        if not $ null target then do
            canonicalPath <- Directory.canonicalizePath target
            fileExists    <- Directory.doesFileExist canonicalPath
            projectExists <- Directory.doesDirectoryExist canonicalPath

            if fileExists then do
                filePath <- Path.parseAbsFile canonicalPath
                if Path.fileExtension filePath /= Package.lunaFileExt then
                    hPutStrLn stderr $ canonicalPath <> " is not a Luna file."
                else Interpret.file filePath
            else if projectExists then runPackage canonicalPath
            else hPutStrLn stderr $ target <> " not found."
        else do
            cwd <- CWD.get
            runPackage cwd

    -- FIXME This can be done much better.
    recover (e :: SomeException) = die (displayException e)

    runPackage path = do
        packagePath   <- Path.parseAbsDir path
        isLunaPackage <- Package.isLunaPackage packagePath

        if isLunaPackage then Interpret.package packagePath
        else hPutStrLn stderr $ path <> " is not a Luna Package."

init :: (ConfigStateIO m, MonadException Path.PathException m) => InitOpts
    -> m ()
init opts = do
    globalCfg <- State.get @Global.Config

    let licenseConfig = view Global.license globalCfg
        mLicense      = if null $ view licenseOverride opts then
            case licenseConfig ^? _Just . Global.defaultLicense . _Just of
                Nothing -> Nothing
                Just key -> hush $ Megaparsec.runParser License.license "" key
        else hush $ Megaparsec.runParser License.license "" . convert
            $ view licenseOverride opts

    Generate.genPackageStructure (view name opts) mLicense globalCfg >>= \case
        Left err -> case err of
            Generate.InvalidPackageLocation msg -> putStrLn msg
            Generate.InvalidPackageName _       -> putStrLn
                $ view name opts <> " is not a valid package name."
            Generate.SystemError msg -> putStrLn
                $ "System Error: " <> convert msg
        Right projectPath -> putStrLn
            $ "Initialised package at " <> projectPath

version :: (MonadIO m) => m ()
version = putStrLn versionMsg where
    gitInfo    = $$(GitHash.tGitInfoCwd)
    versionMsg = "luna "
        <> "(" <> Info.os <> "-" <> Info.arch <> ") "
        <> "(" <> Info.compilerName <> "-"
        <> Version.showVersion Info.compilerVersion <> ") "
        <> "["
        <> GitHash.giBranch gitInfo
        <> "@"
        <> GitHash.giHash gitInfo <> ", "
        <> isDirty <> ", "
        <> show (GitHash.giCommitCount gitInfo) <> " commits, "
        <> "latest on " <> GitHash.giCommitDate gitInfo
        <> "]"
    isDirty = if GitHash.giDirty gitInfo then "Dirty" else "Clean"

rename :: forall m . (ConfigStateIO m, MonadException Path.PathException m)
    => RenameOpts -> m ()
rename opts = MException.catch printRenameEx . MException.catch printPNFEx $ do
    let sourceDir = opts ^. srcName
        targetDir = opts ^. destName

    canonicalSource <- getPath sourceDir
    canonicalTarget <- getPath targetDir

    resultPath <- Package.rename canonicalSource canonicalTarget

    putStrLn $ "Package renamed to " <> Path.fromAbsDir resultPath

    where
        printRenameEx :: Package.RenameException -> m ()
        printRenameEx e = liftIO . hPutStrLn stderr $ displayException e

        printPNFEx :: (MonadIO n, MonadException Package.RenameException n)
            => Package.PackageNotFoundException -> n ()
        printPNFEx e = liftIO . hPutStrLn stderr $ displayException e

        getPath :: (MonadIO n, MonadException Path.PathException n)
            => FilePath -> n (Path Abs Dir)
        getPath fp = MException.rethrowFromIO @Path.PathException $ do
            canonicalPath <- liftIO $ Directory.canonicalizePath fp
            Path.parseAbsDir canonicalPath



-------------------------
-- === Luna Runner === --
-------------------------

-- === API === --

runLuna :: (MonadIO m) => CommandOpts -> m ()
runLuna input = case input of
    Exec command -> do
        globalConfig <- acquireGlobalConfig >>= \case
            Left errText -> do
                putStrLn $ convert errText
                pure $ def @Global.Config
            Right config -> pure config

        -- Defaulting to be filled later where relevant.
        let localConfig = def @Local.Config

        (flip State.evalT) localConfig $ (flip State.evalT) globalConfig $
            case command of
                Build    _    -> putStrLn
                    "Building of executables is not yet implemented."
                Clean    _    -> putStrLn
                    "Cleaning build artefacts is not yet implemented."
                Document opts -> do
                    let DocumentOpts tgt out = opts
                    liftIO $ GenerateDocumentation.generateDocumentation out tgt
                Download _    -> putStrLn
                    "Downloading of packages is not yet implemented."
                Freeze   _    -> putStrLn
                    "Freezing package dependencies is not yet implemented."
                Init opts     -> MException.catch (\(e :: Path.PathException) ->
                    liftIO . hPutStrLn stderr $ displayException e) (init opts)
                Install  _    -> putStrLn
                    "Installing dependencies is not yet implemented."
                Options  _    -> putStrLn
                    "Setting compiler options is not yet implemented."
                Publish  _    -> putStrLn
                    "Publishing packages is not yet implemented."
                Rename opts   -> MException.catch (\(e:: Path.PathException) ->
                    liftIO . hPutStrLn stderr $ displayException e)
                    (rename opts)
                Retract  _    -> putStrLn
                    "Retraction of package versions is not yet implemented."
                Rollback _    -> putStrLn
                    "Rolling back dependencies is not yet implemented."
                Run opts      -> run opts
                Test     _    -> putStrLn
                    "Executing test suites is not yet implemented."
                Unfreeze _    -> putStrLn
                    "Unfreezing package dependencies is not yet implemented."
                Update   _    -> putStrLn
                    "Updating package dependencies is not yet implemented."
                None          -> putStrLn "Command None. Should never happen."
    ShowVersion -> version

acquireGlobalConfig :: forall m . MonadIO m => m (Either Text Global.Config)
acquireGlobalConfig = liftIO $ Exception.catch acquire recovery where
    recovery :: Exception.IOException -> IO (Either Text Global.Config)
    recovery e = pure . Left
        $ "Cannot create global configuration: " <> convertTo @Text (show e)

    acquire :: IO (Either Text Global.Config)
    acquire  = do
        homeDir <- Directory.getHomeDirectory

        let lunaConfigDir = homeDir </> Global.configDir
        configDirExists <- Directory.doesDirectoryExist lunaConfigDir

        unless configDirExists
            $ Directory.createDirectoryIfMissing True lunaConfigDir

        let lunaConfigFile = lunaConfigDir </> Global.configName
        configFileExists <- Directory.doesFileExist lunaConfigFile

        unless configFileExists $ do
            putStrLn "Generating Global Config"
            putStrLn $ "Please fill in your name in " <> lunaConfigFile
            putStrLn $ "Please fill in your email in " <> lunaConfigFile
            let defaultConfig = def @Global.Config
            Yaml.encodeFile lunaConfigFile defaultConfig

        globalConfig <- Yaml.decodeFileEither @Global.Config lunaConfigFile

        case globalConfig of
            Left _    -> pure $ Left "Unable to decode global configuration."
            Right cfg -> pure $ Right cfg

