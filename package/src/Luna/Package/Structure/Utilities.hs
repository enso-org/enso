module Luna.Package.Structure.Utilities where

import Prologue

import qualified Luna.Package.Structure.Name   as Name
import qualified Luna.Syntax.Text.Lexer.Runner as Lexer
import qualified Luna.Syntax.Text.Lexer.Symbol as Symbol
import qualified Luna.Path.Path                as Path
import qualified Path                          as Path
import qualified Path.IO                       as Path
import qualified System.Directory              as Directory

import Luna.Syntax.Text.Lexer.Token as Token

------------------------------------------
-- === Project Generation Utilities === --
------------------------------------------

-- === API === --

isValidPkgName :: Path.Path Path.Rel Path.Dir -> Bool
isValidPkgName path = length validTokens == 1
    where validTokens = catMaybes $ Symbol.matchCons . view Token.element
                                <$> Lexer.evalDefLexer (convert $ Path.toFilePath path)

findParentPackageIfInside :: MonadIO m => Path.Path Path.Abs Path.Dir -> m (Maybe (Path.Path Path.Abs Path.Dir))
findParentPackageIfInside path = do
    let parentPath = Path.parent path

    canonicalPath   <- Path.canonicalizePath
        $ parentPath Path.</> Name.configDirectory
    hasPkgConfigDir <- Path.doesDirExist canonicalPath

    if hasPkgConfigDir then
        pure $ Just parentPath
    else if not $ Path.isDrive parentPath then
        findParentPackageIfInside parentPath
    else pure Nothing


