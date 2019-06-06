module Luna.Package.Configuration.License.Data where

import Prologue

import qualified Data.Map                           as Map
import qualified Luna.Package.Configuration.License as License
import qualified Luna.Datafile                      as Datafile
import qualified Luna.Datafile.Licenses             as Licenses
import qualified Path                               as Path
import qualified Path.IO                            as Path
import qualified System.Directory                   as Directory

import Control.Monad.Exception (MonadException)
import Data.Map                (Map)


-----------------
-- === API === --
-----------------

-- === Definition === --

licenseMap :: Map License.License (Path.Path Path.Rel Path.File)
licenseMap = Map.fromList
    [ (License.AFL_3_0            , $(Path.mkRelFile "afl_3.0.license"))
    , (License.AGPL_3_0           , $(Path.mkRelFile "agpl_3.0.license"))
    , (License.Apache_2_0         , $(Path.mkRelFile "apache_2.0.license"))
    , (License.Artistic_2_0       , $(Path.mkRelFile "artistic_2.0.license"))
    , (License.BSD_2_Clause       , $(Path.mkRelFile "bsd_2_clause.license"))
    , (License.BSD_3_Clause       , $(Path.mkRelFile "bsd_3_clause.license"))
    , (License.BSD_3_Clause_Clear , $(Path.mkRelFile "bsd_3_clause_clear.license"))
    , (License.BSL_1_0            , $(Path.mkRelFile "bsl_1.0.license"))
    , (License.CCBySA_4_0         , $(Path.mkRelFile "cc_by_sa_4.0.license"))
    , (License.CCBy_4_0           , $(Path.mkRelFile "cc_by_4.0.license"))
    , (License.CC_0_1_0           , $(Path.mkRelFile "cc0.license"))
    , (License.EUPL_1_0           , $(Path.mkRelFile "eupl_1.0.license"))
    , (License.GPL_2_0            , $(Path.mkRelFile "gpl_2.0.license"))
    , (License.GPL_3_0            , $(Path.mkRelFile "gpl_3.0.license"))
    , (License.ISC                , $(Path.mkRelFile "isc.license"))
    , (License.LGPL_2_1           , $(Path.mkRelFile "lgpl_2.1.license"))
    , (License.LGPL_3_0           , $(Path.mkRelFile "lgpl_3.0.license"))
    , (License.MIT                , $(Path.mkRelFile "mit.license"))
    , (License.MPL_2_0            , $(Path.mkRelFile "mpl_2.0.license"))
    , (License.MS_PL              , $(Path.mkRelFile "mspl.license"))
    , (License.Unlicense          , $(Path.mkRelFile "unlicense.license")) ]


-- === API === --

getLicenseText :: (MonadIO m, MonadException Datafile.DatafileException m)
    => License.License -> m String
getLicenseText license = do
    licenseDir <- Licenses.findPath
    case Map.lookup license licenseMap of
        Just fileName -> do
            let filePath = licenseDir Path.</> fileName
            fileExists  <- Path.doesFileExist filePath
            if not fileExists then pure "" else liftIO $ readFile (Path.fromAbsFile filePath)
        Nothing -> pure ""


