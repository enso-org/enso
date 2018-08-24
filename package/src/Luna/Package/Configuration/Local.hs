module Luna.Package.Configuration.Local where

import Prologue

import qualified Control.Lens.Aeson as Lens
import qualified Data.Aeson.Types   as Aeson
import qualified Data.Yaml          as Yaml

import Luna.Package.Configuration.License ( License )
import Luna.Package.Version               ( Version )

-- TODO Make this more robust against invalid keys
-- TODO Proper Indentation for the generated yaml

------------------------
-- === Build Type === --
------------------------

-- === Definition === --

data BuildType
    = Executable
    | ExecutableLibrary
    | Library
    deriving (Eq, Generic, Ord, Show)


-- === Instances === --

instance Default BuildType where
    def = ExecutableLibrary

instance Yaml.FromJSON BuildType where
    parseJSON = Lens.parseYamlStyle

instance Yaml.ToJSON BuildType where
    toJSON     = Lens.toJSONYamlStyle
    toEncoding = Lens.toEncodingYamlStyle



--------------------------
-- === Target Stage === --
--------------------------

-- === Definition === --

data TargetStage
    = BeforeCompilation
    | AfterCompilation
    deriving (Eq, Generic, Ord, Show)


-- === Instances === --

instance Default TargetStage where
    def = BeforeCompilation

instance Yaml.FromJSON TargetStage where
    parseJSON = Lens.parseYamlStyle

instance Yaml.ToJSON TargetStage where
    toJSON     = Lens.toJSONYamlStyle
    toEncoding = Lens.toEncodingYamlStyle



-----------------------------
-- === External Target === --
-----------------------------

-- === Definition === ---

data ExternalTarget = Target
    { _name    :: Text
    , _tool    :: Text
    , _output  :: FilePath
    , _options :: [Text]
    , _stage   :: TargetStage
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''ExternalTarget


-- === Instances === --

instance Default ExternalTarget where
    def = Target "" "" "" [] def

instance Yaml.FromJSON ExternalTarget where
    parseJSON = Lens.parseYamlStyle

instance Yaml.ToJSON ExternalTarget where
    toJSON     = Lens.toJSONYamlStyle
    toEncoding = Lens.toEncodingYamlStyle



--------------------------------
-- === Package Config === --
--------------------------------

-- === Definition === --

type LunaOption = Text

data Config = Config
    { _author          :: Text
    , _maintainer      :: Text
    , _projectName     :: Text
    , _projectVersion  :: Version
    , _license         :: License
    , _buildType       :: BuildType
    , _synopsis        :: Text
    , _description     :: Text
    , _lunaOptions     :: Maybe [LunaOption]
    , _externalTargets :: Maybe [ExternalTarget]
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''Config


-- === Instances === --

instance Default Config where
    def = Config def def def def def def def def Nothing Nothing

instance Yaml.FromJSON Config where
    parseJSON = Lens.parseYamlStyle

instance Yaml.ToJSON Config where
    toJSON     = Lens.toJSONYamlStyle
    toEncoding = Lens.toEncodingYamlStyle

