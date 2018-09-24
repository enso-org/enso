{-# LANGUAGE NoStrict          #-}
{-# LANGUAGE NoStrictData      #-}
{-# LANGUAGE OverloadedStrings #-}

module Luna.Syntax.Text.Parser.Hardcoded where

import Prologue

import qualified Control.Monad.State.Layered         as State
import qualified Language.Symbol.Operator.Assoc      as Assoc
import qualified Language.Symbol.Operator.Prec       as Prec
import qualified Luna.Syntax.Text.Parser.Lexer.Names as Name
import qualified Luna.Syntax.Text.Scope              as Scope

import Luna.IR                (Name)
import Luna.Syntax.Text.Scope (Scope)


-- TODO: Remove

-----------------------------
-- === Hardcoded names === --
-----------------------------

hardcodePrecRelMap :: (Prec.RelWriter Name m, Assoc.Writer Name m) => m ()
hardcodePrecRelMap = do
    Prec.writeRel LT (Name.typed :: Name) (Name.arrow :: Name)
    Prec.writeRel LT (Name.arrow :: Name) ("$"        :: Name)
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

    Assoc.write Assoc.Right (Name.lam :: Name)


hardcodeMultiNames :: State.Monad Scope m => m ()
hardcodeMultiNames = do
    Scope.addMultipartName $ "if" :| ["then", "else"]
    Scope.addMultipartName $ "if" :| ["then"]
    -- addNameDesc (NameDesc False "if"  ["then"])

hardcode :: (Prec.RelWriter Name m, State.Monad Scope m, Assoc.Writer Name m)
         => m ()
hardcode = do
    hardcodePrecRelMap
    hardcodeMultiNames
