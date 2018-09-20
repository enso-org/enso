module Luna.Package.Structure.Utilities where

import Prologue

import qualified Luna.Package.Structure.Name   as Name
import qualified Luna.Syntax.Text.Lexer.Runner as Lexer
import qualified Luna.Syntax.Text.Lexer.Symbol as Symbol
import qualified Path                          as Path
import qualified System.Directory              as Directory
import qualified System.FilePath               as FilePath

import Control.Lens    (element)
import System.FilePath (FilePath, (</>))

import Luna.Syntax.Text.Lexer.Token as Token

------------------------------------------
-- === Project Generation Utilities === --
------------------------------------------

-- === API === --

isValidPkgName :: String -> Bool
isValidPkgName name = length validTokens == 1
    where validTokens = catMaybes $ Symbol.matchCons . view Token.element
                                <$> Lexer.evalDefLexer (convert name)

findParentPackageIfInside :: MonadIO m => FilePath -> m (Maybe FilePath)
findParentPackageIfInside path = do
    let parentPath = FilePath.takeDirectory path

    canonicalPath   <- liftIO . Directory.canonicalizePath
        $ parentPath </> Path.fromRelDir Name.configDirectory
    hasPkgConfigDir <- liftIO $ Directory.doesDirectoryExist canonicalPath

    if hasPkgConfigDir then
        pure $ Just parentPath
    else if not $ FilePath.isDrive parentPath then
        findParentPackageIfInside parentPath
    else pure Nothing

