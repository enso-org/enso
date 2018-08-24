{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

module Data.Tag.TH where

import Prologue

import Data.Tag.Class
import Language.Haskell.TH         as TH
import Language.Haskell.TH.Builder as THBuilder

import qualified Data.Char as Char


dataWithAlias :: Name -> Name -> Name -> [TH.Dec]
dataWithAlias dataName aliasName aliasCons = [dataDecl, aliasDecl]
    where dataDecl  = convert $ data'' dataName & derivs .~ [TH.DerivClause Nothing [cons' ''Generic]]
          aliasDecl = alias aliasName $ app (cons' aliasCons) (cons' dataName)


-- | Define a tag family instance.
--   `Tag.familyInstance "Fam" "Foo"` will generate:
--   > data FOO; type Foo = Fam FOO
familyInstance  :: Name -> String -> Q [TH.Dec]
familyInstance' :: Name -> String ->   [TH.Dec]
familyInstance = return .: familyInstance'
familyInstance' fam el = nonStandardFamilyInstance' fam el (Char.toUpper <$> el)

-- | Define a custom-named tag family instance.
--   `Tag.nonStandardFamilyInstance "Fam" "Foo" "AnyFoo"` will generate:
--   > data AnyFoo; type Foo = Fam AnyFoo
nonStandardFamilyInstance  :: (Convertible' fam Name, Convertible' el Name)
                           => fam -> el -> String -> Q [TH.Dec]
nonStandardFamilyInstance = return .:. nonStandardFamilyInstance'

nonStandardFamilyInstance' :: (Convertible' fam Name, Convertible' el Name)
                           => fam -> el -> String ->   [TH.Dec]
nonStandardFamilyInstance' fam el inst = dataWithAlias instName elName famName
    where elName   = convert' el
          famName  = convert' fam
          instName = convert' inst


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
          subDecls  = concat $ familyInstance' (convert famNameStr) <$> subTypeNamesStr


-- | Create tag family with optional provided instances.
--   You can always add further instances by using `familyInstance`
--   `Tag.family "Fam" ["Foo", "Bar"]` will generate closed family.
--   `Tag.family "Fam"                 will generate open   family.
class Family a where family :: String -> a
instance t ~ [String] => Family (t -> Q [TH.Dec]) where family = return .: familyWithInstances
instance                 Family      (Q [TH.Dec]) where family = return .  familyHeader

