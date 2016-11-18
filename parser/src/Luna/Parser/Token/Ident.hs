{-# LANGUAGE NoMonomorphismRestriction #-}

module Luna.Parser.Token.Ident where

import Luna.Prelude hiding (lex)

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

_alias_     = reserveKeyword "alias"     <?> "'alias' keyword"
_as_        = reserveKeyword "as"        <?> "'as' keyword"
_case_      = reserveKeyword "case"      <?> "'case' keyword"
_class_     = reserveKeyword "class"     <?> "'class' keyword"
_def_       = reserveKeyword "def"       <?> "'def' keyword"
_else_      = reserveKeyword "else"      <?> "'else' keyword"
_from_      = reserveKeyword "from"      <?> "'from' keyword"
_if_        = reserveKeyword "if"        <?> "'if' keyword"
_interface_ = reserveKeyword "interface" <?> "'interface' keyword"
_import_    = reserveKeyword "import"    <?> "'import' keyword"
_type_      = reserveKeyword "type"      <?> "'type' keyword"
_foreign_   = reserveKeyword "foreign"   <?> "'foreign' keyword"
