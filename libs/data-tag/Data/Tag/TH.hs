module Data.Tag.TH where

import Prologue

import Data.Tag.Class
import Language.Haskell.TH.Builder
import Language.Haskell.TH as TH


dataWithAlias :: Name -> Name -> Name -> [TH.Dec]
dataWithAlias dataName aliasName aliasCons = [dataDecl, aliasDecl]
    where dataDecl  = data' dataName
          aliasDecl = alias aliasName $ app (var aliasCons) (var dataName)

-- | Define a subtype of the parent type.
--   `defineSubtype "Fam" "Foo"` will generate:
--   > data FOO; type Foo = FamTag FOO
defineSubtype :: Name -> Name -> [TH.Dec]
defineSubtype famName name = dataWithAlias upperName name prefixedFamName
    where upperName       = toUpper name
          prefixedFamName = mapName (<> "Tag") famName

-- | Define a parent type definition that can later be used to construct
--   its effectively-subtypes.
--   `defineType "Fam"` will generate:
--   > data FAM; type FamTag = Tag FAM
defineType :: Name -> [TH.Dec]
defineType famName = dataWithAlias upperFamName prefixedFamName ''Tag
    where upperFamName    = toUpper famName
          prefixedFamName = mapName (<> "Tag") famName


-- | Create a set of datatypes along with aliases like the following:
--   `tagFamily "Fam" ["Foo", "Bar"]` will generate:
--
--   > data FAM; type FamTag = Tag FAM
--   > data FOO; type Foo    = FamTag FOO
--   > data BAR; type Bar    = FamTag Bar
tagFamily :: String -> [String] -> Q [TH.Dec]
tagFamily famNameStr subTypeNamesStr = return $ mainDecls <> subDecls
    where famName         = mkName famNameStr
          upperFamName    = toUpper famName
          prefixedFamName = mapName (<> "Tag") famName
          subTypeNames    = map mkName subTypeNamesStr
          mainDecls       = defineType famName
          subDecls        = concatMap (defineSubtype famName) subTypeNames
