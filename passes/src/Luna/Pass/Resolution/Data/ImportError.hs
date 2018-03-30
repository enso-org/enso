{-# LANGUAGE OverloadedStrings #-}

module Luna.Pass.Resolution.Data.ImportError where

import Luna.Prelude
import OCI.IR.Name

data ImportError = SymbolNotFound
                 | SymbolAmbiguous [Name]

consImportErrorDoc :: Name -> ImportError -> Text
consImportErrorDoc n SymbolNotFound         =
    "Can't find constructor: " <> fromString (show n)
consImportErrorDoc n (SymbolAmbiguous mods) =
    let moduleList = foldl (\l r -> l <> "\n" <> r) "" $ fromString . show <$> mods
    in "Constructor " <> fromString (show n) <> " is ambiguous.\n"
       <> "It's exported by the following modules: \n" <> moduleList

