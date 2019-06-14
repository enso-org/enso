module Luna.Shell.CWD where

import Prologue

import qualified System.Environment as Environment

import Path     (Path, Dir, Abs, parseAbsDir)
import Path.IO  (getCurrentDir, canonicalizePath)



-----------------------
-- === Constants === --
-----------------------

appImageEnvVar :: String
appImageEnvVar = "OWD"



-----------------
-- === API === --
-----------------

get :: MonadIO m => m (Path Abs Dir)
get = do
    cwd <- liftIO . catch (Environment.getEnv appImageEnvVar >>= parseAbsDir)
            $ \(_ :: SomeException) -> liftIO getCurrentDir
    canonicalizePath cwd
