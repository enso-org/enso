{-# LANGUAGE NoStrict     #-}
{-# LANGUAGE NoStrictData #-}

module Luna.Syntax.Text.Parser.Data.Name.Hardcoded where

import Prologue

import qualified Control.Monad.State.Layered          as State
import qualified Language.Symbol.Operator.Prec        as Prec
import qualified Luna.Syntax.Text.Parser.Data.Name.Special as Name
import qualified Luna.Syntax.Text.Scope               as Scope

import Luna.IR                (Name)
import Luna.Syntax.Text.Scope (Scope)


-- TODO: Remove

-----------------------------
-- === Hardcoded names === --
-----------------------------

hardcodePrecRelMap :: Prec.RelWriter Name m => m ()
hardcodePrecRelMap = do
    Prec.writeRel LT (Name.arrow :: Name) (Name.typed :: Name)
    Prec.writeRel LT (Name.typed :: Name) ("$"        :: Name)
    Prec.writeRel LT ("$"        :: Name) ("<"        :: Name)
    Prec.writeRel LT ("<"        :: Name) ("+"        :: Name)
    Prec.writeRel LT ("+"        :: Name) ("*"        :: Name)
    Prec.writeRel LT ("*"        :: Name) ("^"        :: Name)
    Prec.writeRel LT ("^"        :: Name) ("=="       :: Name)
    Prec.writeRel LT ("=="       :: Name) (Name.acc   :: Name)
    Prec.writeRel LT (Name.acc   :: Name) (Name.app   :: Name)
    Prec.writeRel LT (Name.app   :: Name) (Name.lam   :: Name)

    Prec.writeRel EQ (Name.app   :: Name) (Name.app   :: Name)

    Prec.writeRel EQ ("-"     :: Name) ("+"         :: Name)
    Prec.writeRel EQ (">"     :: Name) ("<"         :: Name)
    Prec.writeRel EQ (">="    :: Name) (">"         :: Name)
    Prec.writeRel EQ ("<="    :: Name) ("<"         :: Name)
    Prec.writeRel EQ ("-"     :: Name) (Name.uminus :: Name)
    Prec.writeRel EQ ("%"     :: Name) ("*"         :: Name)
    Prec.writeRel EQ ("/"     :: Name) ("*"         :: Name)

hardcodeMultiNames :: State.Monad Scope m => m ()
hardcodeMultiNames = do
    Scope.addMultipartName $ "if" :| ["then", "else"]
    Scope.addMultipartName $ "if" :| ["then"]
    -- addNameDesc (NameDesc False "if"  ["then"])

hardcode :: (Prec.RelWriter Name m, State.Monad Scope m) => m ()
hardcode = do
    hardcodePrecRelMap
    hardcodeMultiNames

