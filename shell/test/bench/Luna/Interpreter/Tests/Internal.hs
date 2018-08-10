module Luna.Interpreter.Tests.Internal where

import Prologue

import qualified Language.Haskell.TH as TH
import qualified System.Directory    as Directory
import qualified System.FilePath     as FilePath



-----------------------
-- === Constants === --
-----------------------

staticFileDir :: FilePath
staticFileDir = $(do
    let currentFileLoc =
            $(TH.LitE . TH.StringL. TH.loc_filename <$> TH.location)
    absFilePath <- TH.runIO . Directory.makeAbsolute
        $ FilePath.dropFileName currentFileLoc
    TH.litE $ TH.StringL absFilePath)

testsRelPath :: FilePath
testsRelPath = "../../../../bench-test"

