module Luna.Shell.CWD where

import Prologue

import qualified System.Directory   as Directory
import qualified System.Environment as Environment

import System.FilePath (FilePath)



-----------------------
-- === Constants === --
-----------------------

appImageEnvVar :: String
appImageEnvVar = "OWD"



-----------------
-- === API === --
-----------------

get :: MonadIO m => m FilePath
get = do
    cwd <- liftIO . catch (Environment.getEnv appImageEnvVar)
            $ \(_ :: SomeException) -> liftIO Directory.getCurrentDirectory
    liftIO $ Directory.canonicalizePath cwd

