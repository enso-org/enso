{-# LANGUAGE OverloadedStrings #-}

module Luna.Syntax.Text.Parser.Data.Name.Hardcoded where

import Prologue

import qualified Control.Monad.State.Layered               as State
import qualified Language.Symbol.Operator.Prec             as Prec
import qualified Luna.Syntax.Text.Parser.Data.Name.Special as Name
import qualified Luna.Syntax.Text.Scope                    as Scope

import Luna.IR                (Name)
import Luna.Syntax.Text.Scope (Scope)


-- TODO: Remove

-----------------------------
-- === Hardcoded names === --
-----------------------------

hardcodePrecRelMap :: Prec.RelWriter Name m => m ()
hardcodePrecRelMap = do
    writeRel LT (Name.arrow :: Name) (Name.typed :: Name)
    writeRel LT (Name.typed :: Name) ("$"        :: Name)
    writeRel LT ("$"        :: Name) ("<"        :: Name)
    writeRel LT ("<"        :: Name) ("+"        :: Name)
    writeRel LT ("+"        :: Name) ("*"        :: Name)
    writeRel LT ("*"        :: Name) ("^"        :: Name)
    writeRel LT ("^"        :: Name) ("=="       :: Name)
    writeRel LT ("=="       :: Name) (Name.acc   :: Name)
    writeRel LT (Name.acc   :: Name) (Name.app   :: Name)
    writeRel LT (Name.app   :: Name) (Name.lam   :: Name)

    writeRel EQ (Name.app   :: Name) (Name.app   :: Name)

    writeRel EQ ("-"     :: Name) ("+"         :: Name)
    writeRel EQ (">"     :: Name) ("<"         :: Name)
    writeRel EQ (">="    :: Name) (">"         :: Name)
    writeRel EQ ("<="    :: Name) ("<"         :: Name)
    writeRel EQ ("-"     :: Name) (Name.uminus :: Name)
    writeRel EQ ("%"     :: Name) ("*"         :: Name)
    writeRel EQ ("/"     :: Name) ("*"         :: Name)

hardcodeMultiNames :: State.Monad Scope m => m ()
hardcodeMultiNames = do
    Scope.addMultipartName $ "if" :| ["then", "else"]
    Scope.addMultipartName $ "if" :| ["then"]
    -- addNameDesc (NameDesc False "if"  ["then"])

hardcode :: (Prec.RelWriter Name m, State.Monad Scope m) => m ()
hardcode = do
    hardcodePrecRelMap
    hardcodeMultiNames


writeRel :: Prec.RelWriter Name m => Ordering -> Name -> Name -> m ()
writeRel rel l r = do
    Prec.writeRel rel l r
    Prec.writeRel GT  l (Name.invalid :: Name)
    Prec.writeRel GT  r (Name.invalid :: Name)
