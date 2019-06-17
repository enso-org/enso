module Luna.Package.Environment where

import Prologue

import qualified Data.Char          as Char
import qualified Path               as Path
import qualified System.Environment as Env

import Path    (Path, Abs, Dir)
import Luna.IR (Name)

packageEnvVarPrefix :: String
packageEnvVarPrefix = "LUNA_LIBRARY_"

-- | This function translates a name like MyLibrary to an underscored
--   environment variable name: LUNA_LIBRARY_MY_LIBRARY.
packageNameToEnvVarName :: Text -> Text
packageNameToEnvVarName txt = convert $ packageEnvVarPrefix <> envName where
    envName = Char.toUpper <$> intercalate "_" wordsByCapitalization
    wordsByCapitalization = reverse $ split "" (convert txt)
    split []       []             = []
    split prevWord []             = [reverse prevWord]
    split prevWord (char : chars) = if Char.isUpper char
        then let
            recur = split [char] chars
            in if null prevWord then recur else reverse prevWord : recur
        else split (char : prevWord) chars

setLibraryVar :: MonadIO m => Name -> Path Abs Dir -> m ()
setLibraryVar name path =
    liftIO $ Env.setEnv varName (Path.fromAbsDir path)
    where
    varName = convert . packageNameToEnvVarName . convert $ name

setLibraryVars :: MonadIO m => [(Name, Path Abs Dir)] -> m ()
setLibraryVars = traverse_ $ uncurry setLibraryVar
