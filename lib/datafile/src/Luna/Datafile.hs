module Luna.Datafile where

import Prologue

import qualified Control.Monad.Exception    as Exception
import qualified Control.Monad.Exception.IO as Exception
import qualified Path                       as Path
import qualified System.Directory           as Directory
import qualified System.FilePath            as FilePath
import qualified System.Environment         as Environment

import Control.Monad.Exception (MonadException)
import Path                    ((</>), Path, Rel, Dir, Abs)



--------------------------------
-- === Location Constants === --
--------------------------------

-- From the repository root
gitFolderName :: Path Rel Dir
gitFolderName = $(Path.mkRelDir ".git")



--------------------
-- === Locate === --
--------------------

-- === Definition === --

-- These classes allow people to run computations to provide these results if
-- needed
class (Eq a, Ord a, Show a) => PackageRootData a where
    pkgRootFromExe  :: MonadIO m => m FilePath       -- Package root from exe
    dataFromPkgRoot :: MonadIO m => m (Path Rel Dir) -- Data from package root

class (Eq a, Ord a, Show a) => RepoRootData a where
    dataFromRepoRoot :: MonadIO m => m (Path Rel Dir) -- Data from repo root

-- If the algorithm finds a potential match, this function is used to check any
-- additional criteria to ensure it's a real match.
type Verifier = Path Abs Dir -> IO Bool

-- Environment variables to override locations if needed
type EnvVar = String

type MonadDatafile m = (MonadIO m, MonadException DatafileException m)
type DatafileData a = (PackageRootData a, RepoRootData a)


-- === API === --

locate :: forall a m . (MonadDatafile m, DatafileData a)
    => Maybe EnvVar -> Verifier -> m (Path Abs Dir)
locate mEnvVar verifier = case mEnvVar of
        Just envVar -> liftIO $ Environment.lookupEnv envVar >>= \case
            Just val -> dataPathFromEnvVar envVar val verifier
            Nothing  -> getDatafileDefaultPath @a verifier
        Nothing -> getDatafileDefaultPath @a verifier


----------------------
-- === Internal === --
----------------------

dataPathFromEnvVar :: forall m . (MonadDatafile m)
    => EnvVar -> String -> Verifier -> m (Path Abs Dir)
dataPathFromEnvVar var val verify = do
    pathVal <- Exception.catch @Path.PathException
        (\e -> Exception.throw . EnvVarSetButInvalid var val $ Just e)
        . Exception.rethrowFromIO @Path.PathException $ Path.parseAbsDir val

    validPath <- liftIO $ verify pathVal

    if validPath then pure pathVal else
        Exception.throw $ EnvVarSetButInvalid var val Nothing

getDatafileDefaultPath :: forall a m . (MonadDatafile m, DatafileData a)
    => Verifier -> m (Path Abs Dir)
getDatafileDefaultPath verify = do
    -- Folder where the executable is located
    canonicalExeFP <- liftIO $ Directory.canonicalizePath =<<
        (fst . FilePath.splitFileName) <$> Environment.getExecutablePath

    -- The root of the potential package
    potentialPkgRoot <- liftIO $ Directory.canonicalizePath =<<
        (canonicalExeFP FilePath.</>) <$> pkgRootFromExe @a

    potentialRootPath <- Exception.catch @Path.PathException
        (\_ -> Exception.throw $ InvalidExecutableLocation canonicalExeFP)
        . Exception.rethrowFromIO @Path.PathException
        $ Path.parseAbsDir potentialPkgRoot

    expectedPkgDataPath <- (potentialRootPath </>) <$> dataFromPkgRoot @a

    isValidDataLoc <- liftIO $ verify expectedPkgDataPath

    if isValidDataLoc then pure expectedPkgDataPath else do
        -- In this case we're not in a package or the package is incorrect

        canonicalExePath <- Exception.catch @Path.PathException
            (\_ -> Exception.throw $ InvalidExecutableLocation canonicalExeFP)
            . Exception.rethrowFromIO @Path.PathException
            $ Path.parseAbsDir canonicalExeFP

        repoHead <- findRepoHead canonicalExePath

        potentialRepoDataPath <- (repoHead </>) <$> dataFromRepoRoot @a

        validRepoData <- liftIO $ verify potentialRepoDataPath

        if validRepoData then pure potentialRepoDataPath else
            Exception.throw $ RepositoryHeadFoundButInvalid repoHead

-- Finds the head of the repository based on looking for the `.git` folder using
-- a directory tree walk.
findRepoHead :: (MonadIO m, MonadException DatafileException m) => Path Abs Dir
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



-------------------------------
-- === DatafileException === --
-------------------------------

-- === Definition === --

data DatafileException
    = EnvVarSetButInvalid EnvVar String (Maybe Path.PathException)
    | RepositoryHeadNotFoundFrom (Path Abs Dir)
    | RepositoryHeadFoundButInvalid (Path Abs Dir)
    | InvalidExecutableLocation FilePath
    deriving (Eq, Show)


-- === Instances === --

instance Exception DatafileException where
    displayException (EnvVarSetButInvalid var str _) =
        "Environment variable " <> var
        <> " set, but contents is not a valid location: " <> show str <> "."
    displayException (RepositoryHeadNotFoundFrom path) =
        "Repository head not found starting from: " <> show path <> "."
    displayException (RepositoryHeadFoundButInvalid path) =
        "Repository head found at " <> show path <> " but does not contain the"
        <> " required files."
    displayException (InvalidExecutableLocation path) = "The detected exe path "
        <> show path <> " is invalid."

