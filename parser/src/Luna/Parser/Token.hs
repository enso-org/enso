{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoOverloadedStrings #-}

module Luna.Parser.Token where

import Data.Char
import qualified Data.HashSet as HashSet
import Data.HashSet (HashSet)
import Data.List (foldl', nub)
import Data.Monoid
import Data.String
import Data.Text hiding (empty,zip,foldl,foldl', concat, length)
import qualified Text.ParserCombinators.ReadP as ReadP
import Text.Parser.Char hiding (spaces)
import Text.Parser.Combinators
import Text.Parser.Token.Highlight hiding (Comment)
import Text.Parser.Token hiding (symbol, symbolic, ident)
import Text.Parser.Token (symbol, ident)
import Prelude.Luna as Prelude hiding (op, noneOf, lex, use)
import Prelude (read)

import qualified Luna.Parser.Indent as Indent
import           Luna.Parser.Indent (MonadIndent)
import           Luna.Parser.Combinators
import qualified Luna.Syntax.AST.Term.Lit as Lit
import qualified Luna.Syntax.AST.Term.Lit as Number
import           Luna.Syntax.AST.Term.Lit (Number)
import           Luna.Parser.Lex
import qualified Luna.Parser.State as State

import qualified Text.Trifecta.Combinators as Trifecta
import           Control.Monad.State.Class (MonadState)
import qualified Luna.Parser.Token.Ident as Token
import qualified Luna.Parser.Token.Layout as Layout
import qualified Luna.Parser.Token.Ident as Ident
import qualified Luna.Parser.Token.Style as Style
import Text.Parser.Token           (reserve)
import Luna.Parser.Class (Parser)
import Luna.Data.Name (Segment(..), MultiName(..))

isSpaceLine c = isSpace c && c /= '\n' && c /= '\r'

spaceLine = satisfy isSpaceLine <?> "space"

--spacesLine = skipMany spaceLine <?> "white space"

lineSpaces = many spaceLine <?> "white space"

--ident s = ident s <* Indent.indented

--tokenBlock :: m a -> m a
--tokenBlock p = p <* (try (someSpace <* Indent.checkIndented) <|> pure ())

--tokenBlock2 p = p <**> spaces2







betweenNative p = between nativeSym nativeSym p


--tokBase = many1 space <|> lineCom <?> ""


--spaces2 = (try lineCom <|> pure id) <* (try (spaces <* Indent.checkIndented) <|> pure ())

--ident :: (TokenParsing m, Monad m, IsString s) => IdentifierStyle m -> m s
--ident s = fmap fromString $ token $ try $ do
--  name <- highlight (_styleHighlight s)
--          ((:) <$> _styleStart s <*> many (_styleLetter s) <?> _styleName s)
--  when (HashSet.member name (_styleReserved s)) $ unexpected $ "reserved " ++ _styleName s ++ " " ++ show name
--  return name




-- | Parse a non-reserved identifier or symbol
multiName :: (TokenParsing m, Monad m) => IdentifierStyle m -> m MultiName
multiName s = token $ do
    let sname = s ^. styleName
    mname <- highlight (_styleHighlight s)
             (MultiName <$> segment s <*> many (segment s) <?> sname)
    let name = toString mname
    when (HashSet.member name (s ^. styleReserved)) $ unexpected $ "reserved " ++ sname ++ " " ++ show name
    return mname
{-# INLINE multiName #-}


segment s = Segment <$> (length <$> many (char '_'))
                    <*> (fromString ∘∘ (:) <$> s ^. styleStart <*> many (s ^. styleLetter))
                    <?> (s ^. styleName <> " segment")


--identifier :: (TokenParsing m, Monad m) => m String
--identifier = ident Ident.style

--varOp           = varIdent <|> operator
--varIdent :: (TokenParsing m, Monad m, Indent.MonadIndentState Indent.State m) => m String
--varIdent = ident Style.var



varIdent = multiName Style.var

pragmaIdent = ident Style.pragma

--conIdent :: (TokenParsing m, Monad m, Indent.MonadIndentState Indent.State m) => m String
conIdent = ident Style.cons

--typeVarIdent :: (TokenParsing m, Monad m, Indent.MonadIndentState Indent.State m) => m String
typeVarIdent = ident Style.typeVar

--typeIdent :: (TokenParsing m, Monad m, Indent.MonadIndentState Indent.State m) => m String
typeIdent = ident Style.tp

reservedOp    = reserve Style.operator

identLetter  = alphaNum <|> lex '_'


-- | Untokenized symbols
symbol' :: TokenParsing m => String -> m String
symbol' name = highlight Symbol (string name)



nameWildcard  = symbol "_"   <?> "parameter wildcard"
wildcard      = symbol "_"   <?> "wildcard"
recWildcard   = symbol "..." <?> "record wildcard"

indBlockBegin :: TokenParsing m => m String
indBlockBegin = symbol ":"
separator     = symbol ","
parenL        = symbol "("
parenR        = symbol ")"
bracketL      = symbol "["
bracketR      = symbol "]"
braceL        = symbol "{"
braceR        = symbol "}"
pipe          = symbol "|"
accessor      = symbol "." <?> "accessor (.)"
arrow         = symbol "->"
typeDecl      = symbol "::"
meta          = symbol "::"
metaRoot      = symbol "*"
importAll     = symbol "*"
assignment    = symbol "="
nativeSym     = symbol "```"
nameStart     = symbol "`"
nameEnd       = symbol "`"
range         = symbol ".."
curry         = symbol "@"
patAlias      = symbol "@"
terminator    = symbol ";" <?> "terminator"
terminator'   = symbol' ";" <?> "terminator"

pragma        = symbol "%"
pragmaEnable  = symbol "+"
pragmaDisable = symbol "-"
pragmaPush    = symbol "push"
pragmaPop     = symbol "pop"



segmentConnector = char '-'

indBlockBegin' = symbol' ":"

----------------------------------------------------------------------
-- Numbers
----------------------------------------------------------------------

--numberL = try (sign <**> token numBase) <?> "number"

--numBase = --  (numPrefix ['o', 'O'] *> (Number.octodecimal <$> numRepr octDigit <*> numExp 'e'))
--        -- <|> (numPrefix ['x', 'X'] *> (Number.hexadecimal <$> numRepr hexDigit <*> numExp 'p'))
--        (Number.decimal <$> numRepr digit) -- <*> numExp 'e')

--numPrefix pfxs = try (lex '0' *> choice (fmap lex pfxs))
--numRepr baseDigit = some baseDigit <**> ((flip Number.Float <$> try (lex '.' *> some baseDigit)) <|> pure Number.Decimal)
number :: CharParsing p => p Number
number = some digit <**> (try rationalMod <|> integerMod)

rationalMod :: CharParsing p => p (String -> Number)
rationalMod = ((\base ext -> Number.decimal $ Number.Double $ read $ base ++ ('.' : ext)) <$ lex '.' <*> some digit)

integerMod :: Applicative p => p (String -> Number)
integerMod = pure (Number.decimal ∘ Number.Integer ∘ read)

--numRepr baseDigit = some baseDigit <**> ((flip Number.Float <$> try (lex '.' *> some baseDigit)) <|> pure Number.Decimal)
                  -- <|> try (Number.Float "0" <$ lex '.' <*> some baseDigit)

--numExp c = (Just <$ lex c <*> numberL) <|> pure Nothing

--sign = highlight Operator
--     $ Number.Negative <$ lex '-'
--   <|> Number.Positive <$ lex '+'
--   <|> pure Number.Positive

---- c-pasted
--number :: TokenParsing m => Integer -> m Char -> m Integer
--number base baseDigit =
--  foldl' (\x d -> base*x + toInteger (digitToInt d)) 0 <$> some baseDigit

----------------------------------------------------------------------
-- Strings
----------------------------------------------------------------------

stringLiteral :: (TokenParsing m, IsString s, Trifecta.DeltaParsing m, MonadIndent m) => m s
stringLiteral = fromString <$> token (highlight StringLiteral lit) where
  lit = Prelude.foldr (Prelude.maybe id (:)) ""
    <$> strParser
    <?> "string"
  strParser    = between (lex '"')  (lex '"'  <?> "end of string") (many qqStringChar)
             <|> between (lex '\'') (lex '\'' <?> "end of string") (many qStringChar)
  qqStringChar = Just <$> qqStringLetter
             <|> stringEscape
             <?> "string character"
  qStringChar  = Just <$> qStringLetter
             <|> stringEscape
             <?> "string character"
  qqStringLetter = satisfy (\c -> (c /= '"' ) && (c /= '\\') && (c > '\026' || c == '\n' || c == '\r'))
  qStringLetter  = satisfy (\c -> (c /= '\'') && (c /= '\\') && (c > '\026' || c == '\n' || c == '\r'))

  stringEscape = highlight EscapeCode $ lex '\\' *> esc where
    esc = Nothing <$ escapeGap
      <|> Nothing <$ escapeEmpty
      <|> Just <$> escapeCode
  escapeEmpty = lex '&'
  escapeGap = skipSome space *> (lex '\\' <?> "end of string gap")
{-# INLINE stringLiteral #-}


--charLiteral :: TokenParsing m => m Char
charLiteral = token (highlight CharLiteral lit) where
  lit = between (lex '\'') (lex '\'' <?> "end of character") characterChar
    <?> "character"
{-# INLINE charLiteral #-}

-- c-pasted
escapeCode :: TokenParsing m => m Char
escapeCode = (charEsc <|> charAscii <|> charControl) <?> "escape code"
--escapeCode = (charEsc <|> charNum <|> charAscii <|> charControl) <?> "escape code"
  where
  charControl = (\c -> toEnum (fromEnum c - fromEnum '@')) <$> (lex '^' *> (upper <|> lex '@'))
  --charNum     = toEnum . fromInteger <$> num where
  --  num = decimal
  --    <|> (lex 'o' *> number 8 octDigit)
  --    <|> (lex 'x' *> number 16 hexDigit)
  charEsc = choice $ parseEsc <$> escMap
  parseEsc (c,code) = code <$ lex c
  escMap = zip "abfnrtv\\\"\'" "\a\b\f\n\r\t\v\\\"\'"
  charAscii = choice $ parseAscii <$> asciiMap
  parseAscii (asc,code) = try $ code <$ string asc
  asciiMap = zip (ascii3codes ++ ascii2codes) (ascii3 ++ ascii2)
  ascii2codes, ascii3codes :: [String]
  ascii2codes = [ "BS","HT","LF","VT","FF","CR","SO"
                , "SI","EM","FS","GS","RS","US","SP"]
  ascii3codes = ["NUL","SOH","STX","ETX","EOT","ENQ","ACK"
                ,"BEL","DLE","DC1","DC2","DC3","DC4","NAK"
                ,"SYN","ETB","CAN","SUB","ESC","DEL"]
  ascii2, ascii3 :: String
  ascii2 = "\BS\HT\LF\VT\FF\CR\SO\SI\EM\FS\GS\RS\US\SP"
  ascii3 = "\NUL\SOH\STX\ETX\EOT\ENQ\ACK\BEL\DLE\DC1\DC2\DC3\DC4\NAK\SYN\ETB\CAN\SUB\ESC\DEL"


----------------------------------------------------------------------
-- Chars
----------------------------------------------------------------------

--symbol   name = token (highlight Symbol (string name))
--symbolic name = token (highlight Symbol (lex name))

--symbol name = token (highlight Symbol (lex name))

----------------------------------------------------------------------
-- Combinators
----------------------------------------------------------------------

parens = nesting . between (symbol "(") (symbol ")")

brackets = nesting . between (symbol "[") (symbol "]")

braces = nesting . between (symbol "{") (symbol "}")

explicitName = between nameStart nameEnd
