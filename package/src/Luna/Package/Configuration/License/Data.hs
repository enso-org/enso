module Luna.Package.Configuration.License.Data where

import Prologue

import qualified Data.Map                           as Map
import qualified Luna.Package.Configuration.License as License
import qualified Luna.Datafile                      as Datafile
import qualified Luna.Datafile.Licenses             as Licenses
import qualified Path                               as Path
import qualified Path.IO                            as Path

import Control.Monad.Exception (MonadException)
import Data.Map                (Map)

import Path                    (Path, Rel, File, (</>), mkRelFile)



-----------------
-- === API === --
-----------------

-- === Definition === --

licenseMap :: Map License.License (Path Rel File)
licenseMap = Map.fromList
    [ (License.AFL_3_0            , $(mkRelFile "afl_3.0.license"))
    , (License.AGPL_3_0           , $(mkRelFile "agpl_3.0.license"))
    , (License.Apache_2_0         , $(mkRelFile "apache_2.0.license"))
    , (License.Artistic_2_0       , $(mkRelFile "artistic_2.0.license"))
    , (License.BSD_2_Clause       , $(mkRelFile "bsd_2_clause.license"))
    , (License.BSD_3_Clause       , $(mkRelFile "bsd_3_clause.license"))
    , (License.BSD_3_Clause_Clear , $(mkRelFile "bsd_3_clause_clear.license"))
    , (License.BSL_1_0            , $(mkRelFile "bsl_1.0.license"))
    , (License.CCBySA_4_0         , $(mkRelFile "cc_by_sa_4.0.license"))
    , (License.CCBy_4_0           , $(mkRelFile "cc_by_4.0.license"))
    , (License.CC_0_1_0           , $(mkRelFile "cc0.license"))
    , (License.EUPL_1_0           , $(mkRelFile "eupl_1.0.license"))
    , (License.GPL_2_0            , $(mkRelFile "gpl_2.0.license"))
    , (License.GPL_3_0            , $(mkRelFile "gpl_3.0.license"))
    , (License.ISC                , $(mkRelFile "isc.license"))
    , (License.LGPL_2_1           , $(mkRelFile "lgpl_2.1.license"))
    , (License.LGPL_3_0           , $(mkRelFile "lgpl_3.0.license"))
    , (License.MIT                , $(mkRelFile "mit.license"))
    , (License.MPL_2_0            , $(mkRelFile "mpl_2.0.license"))
    , (License.MS_PL              , $(mkRelFile "mspl.license"))
    , (License.Unlicense          , $(mkRelFile "unlicense.license")) ]


-- === API === --

getLicenseText :: (MonadIO m, MonadException Datafile.DatafileException m)
    => License.License -> m String
getLicenseText license = do
    licenseDir <- Licenses.findPath
    case Map.lookup license licenseMap of
        Just fileName -> do
            let filePath = licenseDir </> fileName
            fileExists  <- Path.doesFileExist filePath
            if not fileExists
                then pure ""
                else liftIO $ readFile (Path.fromAbsFile filePath)
        Nothing -> pure ""


