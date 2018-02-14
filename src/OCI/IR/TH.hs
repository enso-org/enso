module OCI.IR.TH where

import Prelude

import Data.Monoid ((<>))
import Data.Tag
import Foreign.Storable.TH
import Language.Haskell.TH as TH

-- | Create a set of datatypes along with aliases like the following:
-- `tagFamily "Fam" ["Foo", "Bar"]` will generate:
-- ```
-- data FAM; type FamTag = Tag FAM
-- data FOO; type Foo    = FamTag FOO
-- data BAR; type Bar    = FamTag Bar
-- ```
tagFamily :: String -> [String] -> Q [TH.Dec]
tagFamily famNameStr subTypeNamesStr = build $ do
    let famName            = typeName famNameStr
        upperFamName       = toUpper famName
        prefixedFamName    = typeName $ famNameStr <> "Tag"
        tag                = toTypeName ''Tag
        defineSubtype nStr = do
            let name      = typeName nStr
                upperName = toUpper name
            define $ data' upperName
            define $ alias name $ app prefixedFamName upperName

    define $ data' upperFamName
    define $ alias prefixedFamName $ app tag upperFamName

    mapM_ defineSubtype subTypeNamesStr
    return []
