module Luna.Datafile.Stdlib where

import Prologue

import qualified Luna.Datafile    as Datafile
import qualified Path             as Path
import qualified System.Directory as Directory

import Path          ( Path, Dir, Abs, (</>) )
import Luna.Datafile ( PackageRootData, RepoRootData, Verifier, EnvVar
                     , MonadDatafile )



----------------------
-- === FindPath === --
----------------------

-- === Definition === --

data Stdlib

-- If this is set, it should point to the root of the stdlib package such that
-- `dir/Stdlib/src` is a valid path (where `dir` is the value of this
-- environment variable). It must be an absolute path.
stdlibOverride :: Maybe EnvVar
stdlibOverride = Just "LUNA_STDLIB_OVERRIDE"


-- === API === --

findPath :: MonadDatafile m => m (Path Abs Dir)
findPath = Datafile.locate @Stdlib stdlibOverride verifyStdlib

-- The path `Std/src` should be found inside the `stdlib` folder in all cases
verifyStdlib :: Verifier
verifyStdlib path = do
    let newPath = path </> $(Path.mkRelDir "./Std/src/")

    Directory.doesDirectoryExist $ Path.fromAbsDir newPath


-- === Instances === --

instance PackageRootData Stdlib where
    pkgRootFromExe  = pure "../../"
    dataFromPkgRoot = pure $(Path.mkRelDir "./config/env/stdlib")

instance RepoRootData Stdlib where
    dataFromRepoRoot = pure $(Path.mkRelDir "./stdlib")

