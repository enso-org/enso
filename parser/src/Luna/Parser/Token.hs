{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoOverloadedStrings #-}

module Luna.Parser.Token where

--import Control.Applicative
--import Control.Monad (MonadPlus(..), when)
--import Data.Char
--import qualified Data.HashSet as HashSet
--import Data.HashSet (HashSet)
--import Data.List (foldl', nub)
--import Data.Monoid
--import Data.String
--import Data.Text hiding (empty,zip,foldl,foldl', concat)
--import qualified Text.ParserCombinators.ReadP as ReadP
--import Text.Parser.Char hiding (spaces)
--import Text.Parser.Combinators
--import Text.Parser.Token.Highlight hiding (Comment)
--import Text.Parser.Token hiding (symbol, symbolic, ident)
--import Prelude.Luna as P hiding (op, noneOf, lex, use)
--import qualified Luna.Syntax.Lit.Number as Number

--import qualified Luna.Parser.Indent as Indent
--import           Luna.Parser.Combinators
--import qualified Luna.Syntax.Lit as Lit
--import           Luna.Parser.Char
--import qualified Luna.Parser.State as State

--import qualified Text.Trifecta.Combinators as Trifecta
--import           Control.Monad.State.Class (MonadState)

--isSpaceLine c = isSpace c && c /= '\n' && c /= '\r'

--spaceLine = satisfy isSpaceLine <?> "space"

----spacesLine = skipMany spaceLine <?> "white space"

--lineSpaces = many spaceLine <?> "white space"

--reservedIdents :: [String]
--reservedIdents = ["alias", "as", "case", "class", "def", "foreign", "from", "interface", "import", "in", "type"]

----ident s = ident s <* Indent.indented

----tokenBlock :: m a -> m a
----tokenBlock p = p <* (try (someSpace <* Indent.checkIndented) <|> pure ())
--tokenBlock p = p <* (try (spaces *> Indent.checkIndented) <|> pure ())

----tokenBlock2 p = p <**> spaces2


--spaces     = concat <$> many tokBase <?> ""


--lineCom   = (\a b -> a : (b ++ "\n")) <$> lineComStart <*> manyTill anyChar (eol <|> eof)



--betweenNative p = between nativeSym nativeSym p

--mlineCom = (++) <$> try mlineComStart <*> mlineComBody
----mlineCom = State.registerComment <=< try mlineComStart *> mlineComBody

--mlineComBody = try mlineComEnd
--                 <|> ((++) <$> mlineCom                <*> mlineComBody)
--                 <|> ((++) <$> many1 (noneOf startEnd) <*> mlineComBody)
--                 <|> oneOf startEnd                     *> mlineComBody
--                 <?> "end of comment"
--                 where startEnd = nub (mlineComEndLetter ++ mlineComStartLetter)

----tokBase = many1 space <|> lineCom <?> ""
--tokBase = many1 space <|> try mlineCom <|> lineCom <?> ""


----spaces2 = (try lineCom <|> pure id) <* (try (spaces <* Indent.checkIndented) <|> pure ())

----ident :: (TokenParsing m, Monad m, IsString s) => IdentifierStyle m -> m s
--ident s = fmap fromString $ tokenBlock $ try $ do
--  name <- highlight (_styleHighlight s)
--          ((:) <$> _styleStart s <*> many (_styleLetter s) <?> _styleName s)
--  when (HashSet.member name (_styleReserved s)) $ unexpected $ "reserved " ++ _styleName s ++ " " ++ show name
--  return name

--reserveBlock s name = tokenBlock $ try $ do
--   _ <- highlight (_styleReservedHighlight s) $ string name
--   notFollowedBy (_styleLetter s) <?> "end of " ++ show name
--{-# INLINE reserveBlock #-}


--opToken p = tokenBlock (highlight Operator p)

--opChars  = "!#$%&*+/<=>?\\^|-~"
--opLetter = oneOf opChars
--opStart  = opLetter

--operator = fmap fromString $ opToken ((:) <$> opStart <*> many opLetter <?> "operator")

--identStyle = IdentifierStyle
--  { _styleName      = "identifier"
--  , _styleStart     = letter <|> lex '_'
--  , _styleLetter    = alphaNum <|> oneOf "_'"
--  , _styleReserved  = HashSet.fromList reservedIdents
--  , _styleHighlight = Identifier
--  , _styleReservedHighlight = ReservedIdentifier
--  }


--pragmaStyle :: TokenParsing m => IdentifierStyle m
--pragmaStyle = identStyle { _styleName      = "pragma identifier"
--                         , _styleStart     = letter
--                         }

--varStyle :: TokenParsing m => IdentifierStyle m
--varStyle = identStyle { _styleName      = "variable identifier"
--                      , _styleStart     = lower
--                      }

--conStyle :: TokenParsing m => IdentifierStyle m
--conStyle = identStyle { _styleName      = "constructor identifier"
--                      , _styleStart     = upper
--                      }

--typeVarStyle :: TokenParsing m => IdentifierStyle m
--typeVarStyle = identStyle { _styleName      = "type variable identifier"
--                          , _styleStart     = lower
--                          }


--typeStyle :: TokenParsing m => IdentifierStyle m
--typeStyle = identStyle { _styleName      = "type identifier"
--                       , _styleStart     = upper
--                       }


--opStyle :: TokenParsing m => IdentifierStyle m
--opStyle = IdentifierStyle
--    { _styleName     = "operator"
--    , _styleStart    = opStart
--    , _styleLetter   = opLetter
--    , _styleReserved = HashSet.fromList [":","::","=","\\","|","<-","->","@","~","=>"]
--    , _styleHighlight = Operator
--    , _styleReservedHighlight = ReservedOperator
--    }

----identifier :: (TokenParsing m, Monad m) => m String
----identifier = ident identStyle

--varOp           = varIdent <|> operator
----varIdent :: (TokenParsing m, Monad m, Indent.MonadIndentState Indent.State m) => m String
--varIdent = ident varStyle

--pragmaIdent = ident pragmaStyle

----conIdent :: (TokenParsing m, Monad m, Indent.MonadIndentState Indent.State m) => m String
--conIdent = ident conStyle

----typeVarIdent :: (TokenParsing m, Monad m, Indent.MonadIndentState Indent.State m) => m String
--typeVarIdent = ident typeVarStyle

----typeIdent :: (TokenParsing m, Monad m, Indent.MonadIndentState Indent.State m) => m String
--typeIdent = ident typeStyle

--reservedIdent = reserveBlock identStyle
--reservedOp    = reserveBlock opStyle

--identLetter  = alphaNum <|> lex '_'


--nameWildcard  = symbol '_'   <?> "parameter wildcard"
--wildcard      = symbol '_'   <?> "wildcard"
--recWildcard   = symbol "..." <?> "record wildcard"
--indBlockBegin = symbol ':'
--separator     = symbol ','
--parenL        = symbol '('
--parenR        = symbol ')'
--bracketL      = symbol '['
--bracketR      = symbol ']'
--braceL        = symbol '{'
--braceR        = symbol '}'
--pipe          = symbol '|'
--accessor      = symbol '.' <?> "accessor (.)"
--arrow         = symbol "->"
--typeDecl      = symbol "::"
--meta          = symbol "::"
--metaRoot      = symbol '*'
--importAll     = symbol '*'
--assignment    = symbol '='
--nativeSym     = symbol "```"
--nameStart     = symbol '`'
--nameEnd       = symbol '`'
--range         = symbol ".."
--curry         = symbol '@'
--terminator    = symbol ';' <?> "terminator"

--pragma        = symbol '%'
--pragmaEnable  = symbol "+"
--pragmaDisable = symbol "-"
--pragmaPush    = symbol "push"
--pragmaPop     = symbol "pop"

--lineComStart        = lex '#'
--mlineComStartLetter = "#["
--mlineComEndLetter   = "#]"
--mlineComStart = string mlineComStartLetter
--mlineComEnd   = string mlineComEndLetter


--kwAlias     = reservedIdent "alias"
--kwAs        = reservedIdent "as"
--kwCase      = reservedIdent "case"
--kwClass     = reservedIdent "class"
--kwDef       = reservedIdent "def"
--kwElse      = reservedIdent "else"
--kwFrom      = reservedIdent "from"
--kwIf        = reservedIdent "if"
--kwInterface = reservedIdent "interface"
--kwImport    = reservedIdent "import"
--kwType      = reservedIdent "type"
--kwForeign   = reservedIdent "foreign"

---- context kewyords
--kwFHaskell  = reservedIdent "haskell"
--kwFCPP      = reservedIdent "c++"

---- type kewyords
--tkwMeta      = reservedIdent "Meta"

------------------------------------------------------------------------
---- Numbers
------------------------------------------------------------------------

--numberL = try (sign <**> tokenBlock numBase) <?> "number"

--numBase =   (numPrefix ['o', 'O'] *> (Number.octodecimal <$> numRepr octDigit <*> numExp 'e'))
--        <|> (numPrefix ['x', 'X'] *> (Number.hexadecimal <$> numRepr hexDigit <*> numExp 'p'))
--        <|> (Number.decimal <$> numRepr digit <*> numExp 'e')

--numPrefix pfxs = try (lex '0' *> choice (fmap lex pfxs))

--numRepr baseDigit = some baseDigit <**> ((flip Number.Float <$> try (lex '.' *> some baseDigit)) <|> pure Number.Decimal)
--                  <|> try (Number.Float "0" <$ lex '.' <*> some baseDigit)

--numExp c = (Just <$ lex c <*> numberL) <|> pure Nothing

--sign = highlight Operator
--     $ Number.Negative <$ lex '-'
--   <|> Number.Positive <$ lex '+'
--   <|> pure Number.Positive

---- c-pasted
--number :: TokenParsing m => Integer -> m Char -> m Integer
--number base baseDigit =
--  foldl' (\x d -> base*x + toInteger (digitToInt d)) 0 <$> some baseDigit

------------------------------------------------------------------------
---- Strings
------------------------------------------------------------------------

----stringLiteral :: (TokenParsing m, IsString s) => m s
--stringLiteral = fromString <$> tokenBlock (highlight StringLiteral lit) where
--  lit = Prelude.foldr (Prelude.maybe id (:)) ""
--    <$> between (lex '"') (lex '"' <?> "end of string") (many stringChar)
--    <?> "string"
--  stringChar = Just <$> stringLetter
--           <|> stringEscape
--       <?> "string character"
--  stringLetter    = satisfy (\c -> (c /= '"') && (c /= '\\') && (c > '\026' || c == '\n' || c == '\r'))

--  stringEscape = highlight EscapeCode $ lex '\\' *> esc where
--    esc = Nothing <$ escapeGap
--      <|> Nothing <$ escapeEmpty
--      <|> Just <$> escapeCode
--  escapeEmpty = lex '&'
--  escapeGap = skipSome space *> (lex '\\' <?> "end of string gap")
--{-# INLINE stringLiteral #-}


----charLiteral :: TokenParsing m => m Char
--charLiteral = tokenBlock (highlight CharLiteral lit) where
--  lit = between (lex '\'') (lex '\'' <?> "end of character") characterChar
--    <?> "character"
--{-# INLINE charLiteral #-}

---- c-pasted
--escapeCode :: TokenParsing m => m Char
--escapeCode = (charEsc <|> charNum <|> charAscii <|> charControl) <?> "escape code"
--  where
--  charControl = (\c -> toEnum (fromEnum c - fromEnum '@')) <$> (lex '^' *> (upper <|> lex '@'))
--  charNum     = toEnum . fromInteger <$> num where
--    num = decimal
--      <|> (lex 'o' *> number 8 octDigit)
--      <|> (lex 'x' *> number 16 hexDigit)
--  charEsc = choice $ parseEsc <$> escMap
--  parseEsc (c,code) = code <$ lex c
--  escMap = zip "abfnrtv\\\"\'" "\a\b\f\n\r\t\v\\\"\'"
--  charAscii = choice $ parseAscii <$> asciiMap
--  parseAscii (asc,code) = try $ code <$ string asc
--  asciiMap = zip (ascii3codes ++ ascii2codes) (ascii3 ++ ascii2)
--  ascii2codes, ascii3codes :: [String]
--  ascii2codes = [ "BS","HT","LF","VT","FF","CR","SO"
--                , "SI","EM","FS","GS","RS","US","SP"]
--  ascii3codes = ["NUL","SOH","STX","ETX","EOT","ENQ","ACK"
--                ,"BEL","DLE","DC1","DC2","DC3","DC4","NAK"
--                ,"SYN","ETB","CAN","SUB","ESC","DEL"]
--  ascii2, ascii3 :: String
--  ascii2 = "\BS\HT\LF\VT\FF\CR\SO\SI\EM\FS\GS\RS\US\SP"
--  ascii3 = "\NUL\SOH\STX\ETX\EOT\ENQ\ACK\BEL\DLE\DC1\DC2\DC3\DC4\NAK\SYN\ETB\CAN\SUB\ESC\DEL"


------------------------------------------------------------------------
---- Chars
------------------------------------------------------------------------

----symbol   name = tokenBlock (highlight Symbol (string name))
----symbolic name = tokenBlock (highlight Symbol (lex name))

--symbol name = tokenBlock (highlight Symbol (lex name))
------------------------------------------------------------------------
---- Combinators
------------------------------------------------------------------------

--parens = nesting . between (symbol '(') (symbol ')')

--brackets = nesting . between (symbol '[') (symbol ']')

--braces = nesting . between (symbol '{') (symbol '}')

--explicitName = between nameStart nameEnd
