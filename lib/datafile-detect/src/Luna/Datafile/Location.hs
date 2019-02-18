module Luna.Datafile.Location where

import Prologue

import qualified Control.Monad.Exception    as Exception
import qualified Control.Monad.Exception.IO as Exception
import qualified Path                       as Path
import qualified System.Directory           as Directory
import qualified System.Environment         as Environment

import Control.Monad.Exception (MonadException, MonadExceptions)
import Path                    ((</>), Path, Rel, Dir, Abs)



--------------------------------
-- === Location Constants === --
--------------------------------

-- What should be found inside the `stdlib` folder in all cases
stdlibContents :: Path Rel Dir
stdlibContents = $(Path.mkRelDir "./Std/src/")

-- From the repository root
stdlibRelPath :: Path Rel Dir
stdlibRelPath = $(Path.mkRelDir "./stdlib")

-- From the repository root
gitFolderName :: Path Rel Dir
gitFolderName = $(Path.mkRelDir ".git")

-- From the package root
binaryPackageRelPath :: Path Rel Dir
binaryPackageRelPath = $(Path.mkRelDir "./bin/public")

-- From the package root
stdlibPackageRelPath :: Path Rel Dir
stdlibPackageRelPath = $(Path.mkRelDir "./config/env/stdlib")

-- If this is set, it should point to the root of the stdlib package such that
-- `dir/Stdlib/src` is a valid path (where `dir` is the value of this
-- environment variable). It must be an absolute path.
stdlibOverride :: String
stdlibOverride = "LUNA_STDLIB_OVERRIDE"



-----------------------------
-- === LocationException === --
-----------------------------

-- === Definition === --

data LocationException
    = EnvVarSetButInvalid String    (Maybe Path.PathException)
    | RepositoryHeadNotFoundFrom    (Path Abs Dir)
    | RepositoryHeadFoundButInvalid (Path Abs Dir)
    deriving (Eq, Show)


-- === Instances === --

instance Exception LocationException where
    displayException (EnvVarSetButInvalid str _) =
        "Environment variable set, but contents are not a valid location: "
        <> show str
    displayException (RepositoryHeadNotFoundFrom path) =
        "Repository head not found starting from: " <> show path
    displayException (RepositoryHeadFoundButInvalid path) =
        "Repository head found at " <> show path <> " but does not contain the"
        <> " required files."



-----------------------
-- === Utilities === --
-----------------------

-- Returns the root of the Stdlib path hierarchy (i.e. `stdlib`, not `Std/src`)
getStdlibPath :: ( MonadIO m
                 , MonadExceptions '[Path.PathException, LocationException] m )
    => m (Path Abs Dir)
getStdlibPath = liftIO $ Environment.lookupEnv stdlibOverride >>= \case
    Just val -> getPathOverrideFromEnvVar val
    Nothing  -> getStdlibDefaultPath

getPathOverrideFromEnvVar :: (MonadIO m, MonadException LocationException m)
    => String -> m (Path Abs Dir)
getPathOverrideFromEnvVar val = do
        pathVal <- Exception.catch @Path.PathException
            (\e -> Exception.throw . EnvVarSetButInvalid val $ Just e)
            . Exception.rethrowFromIO @Path.PathException $ Path.parseAbsDir val

        pathExists <- liftIO . Directory.doesDirectoryExist
            . Path.fromAbsDir $ pathVal </> stdlibContents

        if pathExists then pure pathVal else
            Exception.throw $ EnvVarSetButInvalid val Nothing

getStdlibDefaultPath :: ( MonadIO m
                        , MonadExceptions '[ Path.PathException
                                           , LocationException] m )
    => m (Path Abs Dir)
getStdlibDefaultPath = do
    currentDir     <- liftIO $ Directory.getCurrentDirectory
    currentDirPath <- Exception.rethrowFromIO @Path.PathException
        $ Path.parseAbsDir currentDir

    let potentialPackageRoot = Path.parent $ Path.parent currentDirPath
        stdlibBasePath       = potentialPackageRoot </> stdlibPackageRelPath
        expectedPath         = stdlibBasePath </> stdlibContents

    expectedExists <- liftIO . Directory.doesDirectoryExist
        $ Path.fromAbsDir expectedPath

    if expectedExists then pure stdlibBasePath
    else do
        repoHead <- findRepoHead currentDirPath

        stdlibExistsInRepo <- liftIO $ Directory.doesDirectoryExist
            . Path.fromAbsDir $ repoHead </> stdlibRelPath </> stdlibContents

        if stdlibExistsInRepo then pure $ repoHead </> stdlibRelPath else
            Exception.throw $ RepositoryHeadFoundButInvalid currentDirPath

-- Finds the head of the repository based on looking for the `.git` folder using
-- a directory tree walk.
findRepoHead :: (MonadIO m, MonadException LocationException m) => Path Abs Dir
    -> m (Path Abs Dir)
findRepoHead cwd = if isRoot cwd then
        Exception.throw $ RepositoryHeadNotFoundFrom cwd
    else do
        let expectedGit = cwd </> gitFolderName

        gitFolderExists <- liftIO . Directory.doesDirectoryExist
            $ Path.fromAbsDir expectedGit

        if gitFolderExists then pure cwd else findRepoHead $ Path.parent cwd

-- Filesystem independent root detection. Relies on the idempotency of
-- `Path.parent` when applied to a root.
isRoot :: Path Abs Dir -> Bool
isRoot path = Path.parent path == path

