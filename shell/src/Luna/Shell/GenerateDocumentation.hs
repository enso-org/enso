module Luna.Shell.GenerateDocumentation where

import Prologue

import qualified Data.Aeson                  as Aeson
import qualified Data.Bimap                  as Bimap
import qualified Data.Graph.Data.Graph.Class as Graph
import qualified Data.Map                    as Map

import qualified Luna.IR                       as IR
import qualified Luna.Package                  as Package
import qualified Luna.Pass.Data.Stage          as TC
import qualified Luna.Pass.Scheduler           as Scheduler
import qualified Luna.Pass.Sourcing.Data.Class as Class
import qualified Luna.Pass.Sourcing.Data.Def   as Def
import qualified Luna.Pass.Sourcing.Data.Unit  as Unit
import qualified Luna.Pass.Sourcing.UnitLoader as ModLoader
import qualified Luna.Pass.Sourcing.UnitMapper as UnitMap
import qualified Luna.Shell.CWD                as CWD
import qualified Path
import qualified System.Directory              as Directory

import Data.Aeson                    (ToJSON)
import Luna.Pass.Sourcing.Data.Class (Class)
import Luna.Pass.Sourcing.Data.Unit  (Unit)
import Luna.Pass.Sourcing.Data.Def   (Def, Documented (..))

data DefDocumentation = DefDocumentation
    { name :: Text
    , documentation :: Maybe Text
    } deriving (Show, Eq, Generic)

data ClassDocumentation = ClassDocumentation
    { name :: Text
    , documentation :: Maybe Text
    , methods :: [DefDocumentation]
    } deriving (Show, Eq, Generic)

data UnitDocumentation = UnitDocumentation
    { name :: Text
    , documentation :: Maybe Text
    , classes   :: [ClassDocumentation]
    , functions :: [DefDocumentation]
    } deriving (Show, Eq, Generic)

data ProjectDocumentation = ProjectDocumentation
    { name :: Text
    , documentation :: Maybe Text
    , units :: [UnitDocumentation]
    } deriving (Show, Eq, Generic)

instance ToJSON DefDocumentation
instance ToJSON ClassDocumentation
instance ToJSON UnitDocumentation
instance ToJSON ProjectDocumentation

documentDef :: IR.Name -> Documented Def -> DefDocumentation
documentDef name doc
    = DefDocumentation (convert name) $ doc ^. Def.documentation

documentClass :: IR.Name -> Documented Class -> ClassDocumentation
documentClass name doc
    = ClassDocumentation (convert name) docs methodDocs where
        docs       = doc ^. Def.documentation
        methods    = Map.toList $ unwrap $ doc ^. Def.documented . Class.methods
        methodDocs = uncurry documentDef <$> methods

documentUnit :: Text -> Unit -> UnitDocumentation
documentUnit name unit = UnitDocumentation name Nothing classDocs funDocs where
    funs      = Map.toList $ unwrap $ unit ^. Unit.definitions
    classes   = Map.toList $ unit ^. Unit.classes
    funDocs   = uncurry documentDef   <$> funs
    classDocs = uncurry documentClass <$> classes

defaultOutFileName :: FilePath
defaultOutFileName = "documentation.json"

generateDocumentation :: FilePath -> FilePath -> IO ()
generateDocumentation outFile' modPath' = do
    let outFile = if null outFile' then defaultOutFileName else outFile'
    modPath      <- if null modPath' then CWD.get else pure modPath'
    canonModPath <- Directory.canonicalizePath modPath
    path         <- Path.parseAbsDir canonModPath
    sourcesMap   <- fmap Path.toFilePath . Bimap.toMapR
        <$> Package.findPackageSources path
    let projectName = convert $ Package.getPackageName path
    Graph.encodeAndEval @TC.Stage $ Scheduler.evalT $ do
        ModLoader.init
        Scheduler.registerAttr @Unit.UnitRefsMap
        Scheduler.enableAttrByType @Unit.UnitRefsMap
        refs <- traverse (\(n, p) -> (n,) <$> ModLoader.readUnit p n)
            $ Map.toList sourcesMap
        units <- for refs $ \(n, ref) -> (n,) <$> case ref ^. Unit.root of
            Unit.Graph r       -> UnitMap.mapUnit n r
            Unit.Precompiled u -> pure u

        let unitDocs = (\(n, u) -> documentUnit (convertVia @IR.Name n) u)
                <$> units
            projectDoc = ProjectDocumentation projectName Nothing unitDocs
        liftIO $ Aeson.encodeFile outFile projectDoc
