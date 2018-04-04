{-# LANGUAGE OverloadedStrings #-}

module Luna.Syntax.Text.Parser.Hardcoded where

import Prologue

import qualified Control.Monad.State.Layered   as State
import qualified Language.Symbol.Operator.Prec as Prec
import qualified Luna.Syntax.Text.Scope        as Scope
import qualified OCI.Data.Name.Multipart       as Name.Multipart

import Luna.IR                (Name)
import Luna.Syntax.Text.Scope (Scope)


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
    Prec.writeRel EQ (">="    :: Name) (">" :: Name)
    Prec.writeRel EQ ("<="    :: Name) ("<" :: Name)
    Prec.writeRel EQ ("-"     :: Name) (uminusName :: Name)
    Prec.writeRel EQ ("%"     :: Name) ("*" :: Name)
    Prec.writeRel EQ ("/"     :: Name) ("*" :: Name)

hardcodeMultiNames :: State.Monad Scope m => m ()
hardcodeMultiNames = do
    Scope.addMultipartName $ Name.Multipart.make "if"  ["then", "else"]
    Scope.addMultipartName $ Name.Multipart.make "if"  ["then"]
    -- addNameDesc (NameDesc False "if"  ["then"])


hardcode :: (Prec.RelWriter Name m, State.Monad Scope m) => m ()
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
