{-# LANGUAGE NoMonomorphismRestriction #-}

module Luna.Parser.Token.Ident where

import Prelude.Luna hiding (lex)

import Luna.Parser.Combinators
import Luna.Parser.Indent          (MonadIndent)
import Luna.Parser.Lex
import Text.Parser.Char
import Text.Parser.Token           (highlight, reserve)
import Text.Trifecta.Combinators   (DeltaParsing)

import qualified Text.Parser.Token.Highlight as Highlight
import qualified Luna.Parser.Token.Layout    as Layout
import qualified Luna.Parser.Token.Style     as Style






-- === Keyword parsers === --

reserveKeyword = reserve Style.basic

_alias_     = reserveKeyword "alias"
_as_        = reserveKeyword "as"
_case_      = reserveKeyword "case"
_class_     = reserveKeyword "class"
_def_       = reserveKeyword "def"
_else_      = reserveKeyword "else"
_from_      = reserveKeyword "from"
_if_        = reserveKeyword "if"
_interface_ = reserveKeyword "interface"
_import_    = reserveKeyword "import"
_type_      = reserveKeyword "type"
_foreign_   = reserveKeyword "foreign"
