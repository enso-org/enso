module Luna.Package.Configuration.Global where

import Prologue

import qualified Control.Lens.Aeson as Lens
import qualified Data.Aeson.Types   as Aeson
import qualified Data.Yaml          as Yaml
import qualified Path               as Path

import Path (Path, Rel, Dir, File, (</>))



--------------------------
-- === Utility Defs === --
--------------------------

projectName :: String
projectName = "luna"

projectNameFile :: Path Rel File
projectNameFile = $(Path.mkRelFile "luna")

projectNameDir :: Path Rel Dir
projectNameDir = $(Path.mkRelDir "luna")

configDir :: Path Rel Dir
configDir = $(Path.mkRelDir ".config") </> projectNameDir

configName :: Path Rel File
configName = $(Path.mkRelFile "config.yaml")



--------------------------------
-- === User Configuration === --
--------------------------------

-- === Definition === --

data UserConfig = UserConfig
    { _name  :: Text
    , _email :: Text
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''UserConfig


-- === Instances === --

instance Yaml.FromJSON UserConfig where
    parseJSON = Lens.parseYamlStyle

instance Yaml.ToJSON UserConfig where
    toJSON = Lens.toJSONYamlStyle
    toEncoding = Lens.toEncodingYamlStyle

instance Default UserConfig where
    def = UserConfig "" ""



-----------------------------------
-- === License Configuration === --
-----------------------------------

-- === Definition === --

data LicenseConfig = LicenseConfig
    { _generateLicense :: Maybe Bool
    , _defaultLicense  :: Maybe Text
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''LicenseConfig


-- === Instances === --

instance Yaml.FromJSON LicenseConfig where
    parseJSON = Lens.parseYamlStyle

instance Yaml.ToJSON LicenseConfig where
    toJSON = Lens.toJSONYamlStyle
    toEncoding = Lens.toEncodingYamlStyle

instance Default LicenseConfig where
    def = LicenseConfig Nothing Nothing



------------------------------------
-- === Compiler Configuration === --
------------------------------------

-- === Definition === --

data CompilerConfig = CompilerConfig
    { _version :: Maybe Text
    , _options :: Maybe [Text]
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''CompilerConfig


-- === Instances === --

instance Yaml.FromJSON CompilerConfig where
    parseJSON = Lens.parseYamlStyle

instance Yaml.ToJSON CompilerConfig where
    toJSON     = Lens.toJSONYamlStyle
    toEncoding = Lens.toEncodingYamlStyle

instance Default CompilerConfig where
    def = CompilerConfig Nothing Nothing



-------------------------------
-- === Global Config Type === --
-------------------------------

-- === Definition === --

data Config = Config
    { _user     :: UserConfig
    , _license  :: Maybe LicenseConfig
    , _compiler :: Maybe CompilerConfig
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''Config


-- === Instances === --

instance Yaml.FromJSON Config where
    parseJSON = Lens.parseYamlStyle

instance Yaml.ToJSON Config where
    toJSON     = Lens.toJSONYamlStyle
    toEncoding = Lens.toEncodingYamlStyle

instance Default Config where
    def = Config (def @UserConfig) Nothing Nothing
