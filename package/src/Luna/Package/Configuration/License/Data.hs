module Luna.Package.Configuration.License.Data where

import Prologue

import qualified Data.Map                           as Map
import qualified Luna.Package.Configuration.License as License
import qualified Luna.Datafile                      as Datafile
import qualified Luna.Datafile.Licenses             as Licenses
import qualified Path                               as Path
import qualified System.Directory                   as Directory

import Control.Monad.Exception (MonadException)
import Data.Map                (Map)
import System.FilePath         ((</>))


-----------------
-- === API === --
-----------------

-- === Definition === --

licenseMap :: Map License.License FilePath
licenseMap = Map.fromList
    [ (License.AFL_3_0            , "afl_3.0.license")
    , (License.AGPL_3_0           , "agpl_3.0.license")
    , (License.Apache_2_0         , "apache_2.0.license")
    , (License.Artistic_2_0       , "artistic_2.0.license")
    , (License.BSD_2_Clause       , "bsd_2_clause.license")
    , (License.BSD_3_Clause       , "bsd_3_clause.license")
    , (License.BSD_3_Clause_Clear , "bsd_3_clause_clear.license")
    , (License.BSL_1_0            , "bsl_1.0.license")
    , (License.CCBySA_4_0         , "cc_by_sa_4.0.license")
    , (License.CCBy_4_0           , "cc_by_4.0.license")
    , (License.CC_0_1_0           , "cc0.license")
    , (License.EUPL_1_0           , "eupl_1.0.license")
    , (License.GPL_2_0            , "gpl_2.0.license")
    , (License.GPL_3_0            , "gpl_3.0.license")
    , (License.ISC                , "isc.license")
    , (License.LGPL_2_1           , "lgpl_2.1.license")
    , (License.LGPL_3_0           , "lgpl_3.0.license")
    , (License.MIT                , "mit.license")
    , (License.MPL_2_0            , "mpl_2.0.license")
    , (License.MS_PL              , "mspl.license")
    , (License.Unlicense          , "unlicense.license") ]


-- === API === --

getLicenseText :: (MonadIO m, MonadException Datafile.DatafileException m)
    => License.License -> m String
getLicenseText license = do
    licenseDir <- Licenses.findPath

    let fileName = fromJust "" $ Map.lookup license licenseMap
        filePath = Path.fromAbsDir licenseDir </> fileName

    fileExists  <- liftIO $ Directory.doesFileExist filePath

    if not fileExists then pure "" else liftIO $ readFile filePath

