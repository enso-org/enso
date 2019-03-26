-- | Datafile
--
-- This library provides utilities for locating compiler datafiles in both
-- development and packaged workflows, allowing the programs to successfully
-- locate their data dependencies whether they are under active development or
-- packaged for distribution.
--
-- It provides the following features:
--  - Lookup via a known location in the development repository.
--  - Lookup via a known location in the package structure.
--  - Override with the use of an environment variable.
--
-- The library works as follows to locate the files.
--
--  1. If provided with an override environment variable, it checks this first.
--     If set, the library uses the value contained in the environment variable,
--     erroring if it isn't valid.
--  2. It then looks for the expected location of the specified data file within
--     a package. If this is not found, the library assumes that it is being
--     called from a development binary. Otherwise, it is used.
--  3. In this final state, the library looks for the data in the location
--     provided to it for development circumstances. This is used if found.
--  4. If the data still cannot be found, this is an error as it indicates a
--     misconfiguration.
--
-- To create a fileset for this library to find, you can take the following
-- steps. We're going to work through the example of `Luna.Datafile.Stdlib` to
-- explain how this works.
--
--  1. We work out where things are stored in both the repo and in a package:
--      - The location of the package root from the executable: "../../". This
--        is because the executable is stored in `pkg/bin/public`, so to get to
--        `pkg` we need to go up two directories.
--      - The location of the data from the package root. In the case of the
--        stdlib, we store it in `pkg/config/env/stdlib`. This makes the
--        location from the root: `config/env/stdlib`.
--      - The location of the data in the repository. We store the stdlib code
--        at `luna/stdlib`, so this is just `stdlib/`.
--
--  2. Now that we have the locations of all the data, we need to decide if we
--     want to be able to provide an override via an environment variable. In
--     the case of the stdlib we do, so we encode it as follows:
--
--     ```
--     stdlibOverride :: Maybe Datafile.EnvVar
--     stdlibOverride = Just "LUNA_STDLIB_OVERRIDE"
--     ```
--
--     If we didn't want to have an environment variable, we could instead pass
--     `Nothing` in its place.
--
--  3. As users may need to run arbitrary computations at runtime to resolve
--     these locations, the actual resolution is done via type-classes. To tell
--     these classes what we want to resolve, we need an uninhabited type, which
--     we define as follows:
--
--     ```
--     data Stdlib
--     ```
--
--  4. Now we have this, we can write the class instances to tell the library
--     where to find the data. First we start with `RepoRootData` which
--     describes the data location relative to the repo root:
--
--     ```
--     instance Datafile.RepoRootData Stdlib where
--         dataFromRepoRoot = pure $(Path.mkRelDir "./stdlib")
--     ```
--
--     After that, we have to write our instance for `PackageRootData`, which
--     describes locations relative to the package. The first entry describes
--     how to get to the package root from the executable, while the second
--     describes how to get to the data from the package root.
--
--     ```
--     instance Datafile.PackageRootData Stdlib where
--         pkgRootFromExe  = pure "../../"
--         dataFromPkgRoot = pure $(Path.mkRelDir "./config/env/stdlib")
--     ```
--
--     While these _could_ run arbitrary computations if needed, in this case we
--     know the location statically, and can easily encode it.
--
--  5. The next component of the library is the verification function, which has
--     type `Verifier`. This function is used to check that the resolved
--     location actually contains the data we expect it to. It takes the
--     resolved path as input, and returns a bool.
--
--     In the case of the stdlib, we know that our `stdlib` directory should
--     always contain the directory `./Std/src`, so we write a function to check
--     that the resolved location meets this criteria:
--
--     ```
--     verifyStdlib :: Datafile.Verifier
--     verifyStdlib path = do
--         let newPath = path </> $(Path.mkRelDir "./Std/src")
--
--         Directory.doesDirectoryExist $ Path.fromAbsDir newPath
--     ```
--
--  6. Now, all that remains is to put the pieces together! The main routine is
--     `Datafile.locate`, so we wrap this in a function that passes all the
--     correct arguments. The arguments are documented on the function itself.
--
--     ```
--     findPath = Datafile.MonadDatafile m => m (Path Abs Dir)
--     findPath = Datafile.locate @Stdlib stdlibOverride verifyStdlib
--     ```
--
--     The only slightly tricky thing here is that we tell the library what to
--     resolve by passing it the uninhabited type via a type-application (the
--     @Stdlib argument to `locate`).
--
--  7. We're done! To get the location of the standard library, it is as simple
--     as calling our `findPath` function. Don't forget to account for the fact
--     that the API can throw an exception if something goes wrong!

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
class PackageRootData a where
    pkgRootFromExe  :: MonadIO m => m FilePath       -- Package root from exe
    dataFromPkgRoot :: MonadIO m => m (Path Rel Dir) -- Data from package root

class RepoRootData a where
    dataFromRepoRoot :: MonadIO m => m (Path Rel Dir) -- Data from repo root

-- If the algorithm finds a potential match, this function is used to check any
-- additional criteria to ensure it's a real match.
type Verifier = Path Abs Dir -> IO Bool

-- Environment variables to override locations if needed
type EnvVar = String

type MonadDatafile m = (MonadIO m, MonadException DatafileException m)
type DatafileData a = (PackageRootData a, RepoRootData a)


-- === API === --

locate :: forall a m . ( MonadDatafile m
                       , DatafileData a ) -- ^ Resolution type, by type app.
    => Maybe EnvVar     -- ^ The override environment variable, if present.
    -> Verifier         -- ^ The verification function.
    -> m (Path Abs Dir) -- ^ The path to the requested data.
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

