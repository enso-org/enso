{-# LANGUAGE OverloadedStrings #-}

module Luna.Syntax.Text.Parser.Hardcoded where

import Luna.Prelude

import OCI.IR.Name
import Luna.Syntax.Text.Scope
import OCI.IR.Name.Multipart
import Control.Monad.State.Dependent

import qualified Language.Symbol.Operator.Prec  as Prec


-------------------------------------------
-- Hardcoded stuff, to be removed slowly --
-------------------------------------------

hardcodePrecRelMap :: Prec.RelWriter Name m => m ()
hardcodePrecRelMap = do
    Prec.writeRel LT (arrowName :: Name) (typedName :: Name)
    Prec.writeRel LT (typedName :: Name) ("$"  :: Name)
    Prec.writeRel LT ("$"     :: Name) ("<"  :: Name)
    Prec.writeRel LT ("<"     :: Name) ("+"  :: Name)
    Prec.writeRel LT ("+"     :: Name) ("*"  :: Name)
    Prec.writeRel LT ("*"     :: Name) ("^"  :: Name)
    Prec.writeRel LT ("^"     :: Name) ("==" :: Name)
    Prec.writeRel LT ("=="    :: Name) (accName :: Name)
    Prec.writeRel LT (accName :: Name) (appName :: Name)
    Prec.writeRel LT (appName :: Name) (lamName :: Name)

    Prec.writeRel EQ ("-"     :: Name) ("+" :: Name)
    Prec.writeRel EQ (">"     :: Name) ("<" :: Name)
    Prec.writeRel EQ ("-"     :: Name) (uminusName :: Name)
    Prec.writeRel EQ ("%"     :: Name) ("*" :: Name)
    Prec.writeRel EQ ("/"     :: Name) ("*" :: Name)

hardcodeMultiNames :: MonadState Scope m => m ()
hardcodeMultiNames = do
    addMultipartName $ mkMultipartName "if"  ["then", "else"]
    addMultipartName $ mkMultipartName "if"  ["then"]
    -- addNameDesc (NameDesc False "if"  ["then"])


hardcode :: (Prec.RelWriter Name m, MonadState Scope m, MonadIO m) => m () -- FIXME: remove IO
hardcode = do
    hardcodePrecRelMap
    hardcodeMultiNames



minusName, uminusName, appName, accName, lamName, typedName, wildcardName, unifyName, updateName, arrowName :: IsString s => s
minusName    = "-"
uminusName   = "#uminus#"
appName      = "#app#"
accName      = "."
lamName      = ":"
typedName    = "::"
wildcardName = "_"
unifyName    = "="
updateName   = "=" -- #update# ?
arrowName    = "->"
