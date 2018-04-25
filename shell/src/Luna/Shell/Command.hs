module Luna.Shell.Command where

import Prologue hiding (init)

import qualified Control.Exception                 as Exception
import qualified Control.Monad.State.Layered       as State
import qualified Data.Text.IO                      as Text
import qualified Data.Yaml                         as Yaml
import qualified Luna.Package.Configuration.Global as Global
import qualified Luna.Package.Configuration.Local  as Local
import qualified Luna.Package.Structure.Generate   as Generate
import qualified System.Directory                  as Directory

import System.FilePath ((</>))

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

data InitOpts = InitOpts
    { _name        :: String
    , _lunaVersion :: String
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''InitOpts

data BuildOpts = BuildOpts
    { __acquireDeps        :: Bool
    , __cleanBuild         :: Bool
    , __standaloneFileName :: String
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''BuildOpts

data RunOpts = RunOpts
    { __standaloneFileName :: String
    , __doNotBuild         :: Bool
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''RunOpts

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
    = Init InitOpts
    | Build BuildOpts
    | Run RunOpts
    | Test TestOpts
    | Clean CleanOpts
    | Doc
    | Publish PublishOpts
    | Retract RetractOpts
    | Options OptionOpts
    | Rollback RollbackOpts
    | Update UpdateOpts
    | Freeze FreezeOpts
    | Unfreeze FreezeOpts
    | Install InstallOpts
    | Download DownloadOpts
    deriving (Eq, Generic, Ord, Show)



-------------------------------
-- === Command Execution === --
-------------------------------

-- === API === --

-- TODO [Ara] Circular Dep Detection
-- TODO [Ara] Check optional version for validity, actually use it.
init :: ConfigStateIO m => InitOpts -> m ()
init opts = Generate.genPackageStructure (view name opts) >>= \case
    Left err -> case err of
        Generate.InvalidPackageLocation msg -> liftIO $ Text.putStrLn msg
        Generate.InvalidPackageName _       -> putStrLn
            $ view name opts <> " is not a valid package name."
        Generate.SystemError msg -> liftIO . Text.putStrLn
            $ "System Error: " <> msg
    Right projectPath -> liftIO . putStrLn
        $ "Initialised package at " <> projectPath

-- TODO [Ara] Swap build modes based on flags and file
build :: ConfigStateIO m => BuildOpts -> m ()
build opts = liftIO $ print opts

-- TODO [Ara] Run based on inputs
run :: ConfigStateIO m => RunOpts -> m ()
run opts = liftIO $ print opts

-- TODO [Ara] Work out how to detect benchmarks
test :: ConfigStateIO m => TestOpts -> m ()
test opts = liftIO $ print opts

clean :: ConfigStateIO m => CleanOpts -> m ()
clean opts = liftIO $ print opts

-- TODO [Ara] print warning message for now
doc :: ConfigStateIO m => m ()
doc = liftIO $ print ("DOC" :: String)

-- TODO [Ara] Need to validate the prerelease as valid and an increase..
publish :: ConfigStateIO m => PublishOpts -> m ()
publish opts = liftIO $ print opts

-- TODO [Ara] Validate provided version
-- TODO [Ara] Ask user for confirmation
retract :: ConfigStateIO m => RetractOpts -> m ()
retract opts = liftIO $ print opts

options :: ConfigStateIO m => OptionOpts -> m ()
options opts = liftIO $ print opts

rollback :: ConfigStateIO m => RollbackOpts -> m ()
rollback opts = liftIO $ print opts

update :: ConfigStateIO m => UpdateOpts -> m ()
update opts = liftIO $ print opts

freeze :: ConfigStateIO m => FreezeOpts -> m ()
freeze opts = liftIO $ print opts

unfreeze :: ConfigStateIO m => FreezeOpts -> m ()
unfreeze opts = liftIO $ print opts

install :: ConfigStateIO m => InstallOpts -> m ()
install opts = liftIO $ print opts

download :: ConfigStateIO m => DownloadOpts -> m ()
download opts = liftIO $ print opts



-------------------------
-- === Luna Runner === --
-------------------------

-- === API === --

runLuna :: MonadIO m => Command -> m ()
runLuna command = do
    globalConfig <- acquireGlobalConfig >>= \case
        Left errText -> do
            putStrLn $ convert errText
            pure $ def @Global.Config
        Right config -> pure config

    -- TODO [Ara] Pick the real config up if inside project
    let localConfig = def @Local.Config

    -- TODO [Ara] Needs to be some shorthand for this.
    (flip State.evalT) localConfig $ (flip State.evalT) globalConfig
        $ case command of
            Init     opts -> init     opts
            Build    opts -> build    opts
            Run      opts -> run      opts
            Test     opts -> test     opts
            Clean    opts -> clean    opts
            Doc           -> doc
            Publish  opts -> publish  opts
            Retract  opts -> retract  opts
            Options  opts -> options  opts
            Rollback opts -> rollback opts
            Update   opts -> update   opts
            Freeze   opts -> freeze   opts
            Unfreeze opts -> unfreeze opts
            Install  opts -> install  opts
            Download opts -> download opts

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
            print ("Generating Global Config" :: String)
            let defaultConfig = def @Global.Config
            Yaml.encodeFile lunaConfigFile defaultConfig

        globalConfig <- Yaml.decodeFileEither @Global.Config lunaConfigFile

        case globalConfig of
            Left _ -> pure $ Left "Unable to decode global configuration."
            Right config -> do
                when (view (Global.user . Global.name) config == "") $
                    putStrLn $ "Please fill in your name in " <> lunaConfigFile
                when (view (Global.user . Global.email) config == "") $
                    putStrLn $ "Please fill in your email in " <> lunaConfigFile

                pure $ Right config

