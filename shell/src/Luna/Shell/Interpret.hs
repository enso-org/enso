module Luna.Shell.Interpret where

import Prologue

import qualified Control.Monad.Exception.IO          as Exception
import qualified Data.Bimap                          as Bimap
import qualified Data.Graph.Data.Graph.Class         as Graph
import qualified Data.Map                            as Map
import qualified Luna.IR                             as IR
import qualified Luna.Package                        as Package
import qualified Luna.Package.Structure.Name         as Package
import qualified Luna.Pass.Data.Stage                as TC
import qualified Luna.Pass.Flow.ProcessUnits         as ProcessUnits
import qualified Luna.Pass.Preprocess.PreprocessUnit as PreprocessUnit
import qualified Luna.Pass.Resolve.Data.Resolution   as Res
import qualified Luna.Pass.Scheduler                 as Scheduler
import qualified Luna.Pass.Sourcing.Data.Unit        as Unit
import qualified Luna.Pass.Sourcing.UnitLoader       as ModLoader
import qualified Luna.Pass.Sourcing.UnitMapper       as UnitMap
import qualified Luna.Pass.Typing.Data.Typed         as Typed
import qualified Luna.Runtime                        as Runtime
import qualified Luna.Std                            as Std
import qualified OCI.Data.Name                       as Name
import qualified Path                                as Path
import qualified System.Directory                    as Directory
import qualified System.FilePath                     as FilePath
import qualified System.IO                           as IO

import Control.Monad.Exception               (MonadExceptions)
import Data.Map                              (Map)
import Path                                  (Path, Abs, Dir, File)


-------------------------------
-- === Interpreter Monad === --
-------------------------------

-- === Definition === --

type InterpreterMonad m = ( MonadIO m
                          , MonadExceptions '[ Scheduler.Error
                                             , ModLoader.UnitLoadingError
                                             , Path.PathException] m
                          , MonadFix m )



---------------------------------
-- === Interpreter Harness === --
---------------------------------

-- === API === --

interpretWithMain :: IR.Qualified -> Map IR.Qualified FilePath -> IO ()
interpretWithMain name sourcesMap = Graph.encodeAndEval @TC.Stage
    $ Scheduler.evalT $ do
        ModLoader.init
        (_, stdUnitRef) <- Std.stdlib @TC.Stage
        Scheduler.registerAttr @Unit.UnitRefsMap
        Scheduler.setAttr $ Unit.UnitRefsMap
            $ Map.singleton "Std.Primitive" stdUnitRef

        ModLoader.loadUnit def sourcesMap [] name
        for Std.stdlibImports $ ModLoader.loadUnit def sourcesMap []
        Unit.UnitRefsMap mods <- Scheduler.getAttr

        units <- flip Map.traverseWithKey mods $ \n u -> case u ^. Unit.root of
            Unit.Graph r       -> UnitMap.mapUnit n r
            Unit.Precompiled u -> pure u

        let unitResolvers = Map.mapWithKey Res.resolverFromUnit units
            importResolvers = Map.mapWithKey (Res.resolverForUnit unitResolvers)
                $ over wrapped ("Std.Base" :) . over wrapped ("Std.Primitive" :)
                . view Unit.imports <$> mods
            unitsWithResolvers = flip Map.mapWithKey units $ \n u ->
                (importResolvers Map.! n, u)

        for (Map.toList importResolvers) $ \(unitName, resolver) -> do
            case Map.lookup unitName units of
                Just uni -> PreprocessUnit.preprocessUnit resolver uni
                Nothing  -> liftIO $ IO.hPutStrLn IO.stderr $
                            "Unable to resolve compilation unit "
                            <> convert unitName

        (tUnits, cUnits) <- ProcessUnits.processUnits def def unitsWithResolvers

        tFunc <- Typed.getDef name (convert Package.mainFuncName) tUnits
        mainFunc <- Runtime.lookupSymbol cUnits name
            $ convert Package.mainFuncName

        case unwrap tFunc of
            Left e  -> print e
            Right _ -> do
                putStrLn $ "Running in interpreted mode."
                void $ liftIO $ Runtime.runIO mainFunc

file :: (InterpreterMonad m) => Path Abs File -> Path Abs Dir -> m ()
file filePath stdlibPath = liftIO $ Directory.withCurrentDirectory fileFP $ do
    fileSources <- Package.fileSourcePaths filePath stdlibPath

    let fileName = convertVia @Name.Name . FilePath.dropExtension
            . Path.fromRelFile $ Path.filename filePath

    liftIO $ interpretWithMain fileName fileSources

    where fileFP = Path.fromAbsDir $ Path.parent filePath

package :: (InterpreterMonad m) => Path Abs Dir -> Path Abs Dir -> m ()
package pkgPath stdlibPath = liftIO . Directory.withCurrentDirectory pkgDir $ do
    packageRoot    <- fromJust pkgPath <$> Package.findPackageRoot pkgPath
    packageImports <- Package.packageImportPaths packageRoot stdlibPath
    importPaths    <- Exception.rethrowFromIO @Path.PathException .
        sequence $ Path.parseAbsDir . snd <$> packageImports
    projectSrcs    <- sequence $ Package.findPackageSources <$> importPaths

    let pkgSrcMap    = Map.map Path.toFilePath . foldl' Map.union Map.empty
            $ Bimap.toMapR <$> projectSrcs
        mainFileName = (convert $ Package.getPackageName packageRoot) <> "."
            <> Package.mainFileName

    liftIO $ interpretWithMain mainFileName pkgSrcMap

    where pkgDir = Path.fromAbsDir pkgPath

