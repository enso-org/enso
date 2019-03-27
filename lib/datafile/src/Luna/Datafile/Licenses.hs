module Luna.Datafile.Licenses where

import Prologue

import qualified Luna.Datafile    as Datafile
import qualified Path             as Path
import qualified System.Directory as Directory
import qualified System.FilePath  as FilePath

import Path          ( Path, Dir, Abs )
import Luna.Datafile ( PackageRootData, RepoRootData, Verifier, EnvVar
                     , MonadDatafile )



----------------------
-- === FindPath === --
----------------------

-- === Definition === --

data Licenses

-- If this is set, it should point to the root of the licenses folder such that
-- `dir` contains the *.license files (where `dir` is the value of this
-- environment variable). It must be an absolute path.
licensesOverride :: Maybe EnvVar
licensesOverride = Just "LUNA_LICENSE_FILES_OVERRIDE"


-- === API === --

findPath :: MonadDatafile m => m (Path Abs Dir)
findPath = Datafile.locate @Licenses licensesOverride verifyLicenses

-- The `*.license` files should be found inside the folder.
verifyLicenses :: Verifier
verifyLicenses path = do
    dirExists <- Directory.doesDirectoryExist $ Path.fromAbsDir path

    if dirExists then do
        files <- Directory.listDirectory $ Path.fromAbsDir path

        -- Check there is at least one `.license` file in the folder
        let hasCorrectExtn fp = FilePath.takeExtension fp == ".license"

        pure $ foldl' (\a b -> a || hasCorrectExtn b) False files
    else pure False


-- === Instances === --

instance PackageRootData Licenses where
    pkgRootFromExe  = pure "../../"
    dataFromPkgRoot = pure $(Path.mkRelDir "./config/env/licenses")

instance RepoRootData Licenses where
    dataFromRepoRoot = pure $(Path.mkRelDir "./package/data/licenses")

