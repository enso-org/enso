module Data.Tag.TH where

import Prologue

import Data.Tag.Class
import Language.Haskell.TH.Builder
import Language.Haskell.TH as TH


dataWithAlias :: Name -> Name -> Name -> [TH.Dec]
dataWithAlias dataName aliasName aliasCons = [dataDecl, aliasDecl]
    where dataDecl  = data' dataName
          aliasDecl = alias aliasName $ app (cons' aliasCons) (cons' dataName)


-- | Define a subtype of the parent type.
--   `Tag.familyInstance "Fam" "Foo"` will generate:
--   > data FOO; type Foo = Fam FOO
familyInstance  :: String -> String -> Q [TH.Dec]
familyInstance' :: String -> String ->   [TH.Dec]
familyInstance = return .: familyInstance'
familyInstance' famNameStr nameStr = dataWithAlias upperName name prefixedFamName
    where name            = convert nameStr
          famName         = convert famNameStr
          upperName       = toUpper name
          prefixedFamName = famName


-- | Define a parent type definition that can later be used to construct
--   its effectively-subtypes.
--   `Tag.familyHeader "Fam"` will generate:
--   > data FAM; type Fam = Tag FAM
familyHeader :: String -> [TH.Dec]
familyHeader famNameStr = dataWithAlias upperFamName prefixedFamName ''Tag
    where famName         = convert famNameStr
          upperFamName    = toUpper famName
          prefixedFamName = famName


-- | Create a set of datatypes along with aliases like the following:
--   `Tag.familyWithInstances "Fam" ["Foo", "Bar"]` will generate:
--
--   > data FAM; type Fam = Tag FAM
--   > data FOO; type Foo = Fam FOO
--   > data BAR; type Bar = Fam Bar
familyWithInstances :: String -> [String] -> [TH.Dec]
familyWithInstances famNameStr subTypeNamesStr = mainDecls <> subDecls
    where mainDecls = familyHeader famNameStr
          subDecls  = concat $ familyInstance' famNameStr <$> subTypeNamesStr


-- | Create tag family with optional provided instances.
--   You can always add further instances by using `familyInstance`
--   `Tag.family "Fam" ["Foo", "Bar"]` will generate closed family.
--   `Tag.family "Fam"                 will generate open   family.
class Family a where family :: String -> a
instance t ~ [String] => Family (t -> Q [TH.Dec]) where family = return .: familyWithInstances
instance                 Family      (Q [TH.Dec]) where family = return .  familyHeader
