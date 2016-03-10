module Luna.Parser.Token.Style where

import Prelude.Luna hiding (lex)

import Luna.Parser.Combinators
import Luna.Parser.Lex
import Text.Parser.Char
import Text.Parser.Token
import Text.Parser.Token.Style    (emptyIdents)
import Luna.Parser.Token.Keyword  (keywords)

import qualified Luna.Parser.Token.Operator  as Operator
import qualified Data.HashSet                as HashSet
import qualified Text.Parser.Token.Highlight as Highlight

basic :: TokenParsing m => IdentifierStyle m
basic = emptyIdents
      & styleName              .~ "identifier"
      & styleStart             .~ letter
      & styleLetter            .~ (alphaNum <|> char '\'')
      & styleReserved          .~ HashSet.fromList keywords
      & styleHighlight         .~ Highlight.Identifier
      & styleReservedHighlight .~ Highlight.ReservedIdentifier
{-# INLINE basic #-}

pragma :: TokenParsing m => IdentifierStyle m
pragma = basic
    & styleName  .~ "pragma identifier"
    & styleStart .~ letter

var :: TokenParsing m => IdentifierStyle m
var = basic
    & styleName  .~ "variable identifier"
    & styleStart .~ lower

cons :: TokenParsing m => IdentifierStyle m
cons = basic
    & styleName  .~ "constructor identifier"
    & styleStart .~ upper

typeVar :: TokenParsing m => IdentifierStyle m
typeVar = basic
    & styleName  .~ "type variable identifier"
    & styleStart .~ lower

tp :: TokenParsing m => IdentifierStyle m
tp = basic
    & styleName  .~ "type identifier"
    & styleStart .~ upper

operator :: TokenParsing m => IdentifierStyle m
operator = basic
    & styleName              .~ "operator"
    & styleStart             .~ Operator.startChar
    & styleLetter            .~ Operator.char
    & styleReserved          .~ HashSet.fromList [":","::","=","\\","|","<-","->","@","~","=>"]
    & styleHighlight         .~ Highlight.Operator
    & styleReservedHighlight .~ Highlight.ReservedOperator
