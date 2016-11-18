{-# LANGUAGE NoMonomorphismRestriction #-}

module Luna.Parser.Layout where

import Luna.Prelude hiding (noneOf)

import qualified Luna.Parser.Token        as Tok
import qualified Luna.Parser.Token.Layout as Tok
import qualified Luna.Parser.Indent       as Indent
import           Luna.Parser.Combinators
import           Luna.Parser.Char         (CharParsing, noneOf, char)

import Luna.Parser.Indent        (MonadIndent)
import Text.Trifecta.Combinators (DeltaParsing)

moduleBlock p = braceBlockBegin p <|> indBlockBody p
blockBegin  p = indBlockBegin   p <|> braceBlockBegin p

indBlockBegin  p = Tok.indBlockBegin *> indBlock p
indBlock       p = Tok.spaces *> withBlockChecked (indBlockBody p)
indBlockBody   p = (:) <$> p <*> many (indBlockPrefix p)
indBlockBody'  p = (:) <$> p <*> many (try $ indBlockPrefix p)
indBlockBodyOpt p = ($) <$> (((:) <$> p) <|> pure id)
                        <*> many (indBlockPrefix p)
indBlockPrefix p = try ((try (Tok.spaces *> Indent.checkIndent) <|> try (Tok.terminator *> Tok.spaces *> Indent.checkIndentedOrEq )) *> notFollowedBy eof) *> p
--indBlockBody   p = (:) <$> p <*> many (try (Tok.spaces *> Indent.checkIndent) *> p)
indBlockSpacesIE = try (Tok.spaces <* Indent.checkIndentedOrEq) <|> pure mempty

braceBlockBegin p = Tok.braceL *> Tok.spaces *> Indent.withZeroIndentBlock (many1 (p <* braceBlockSpaces)) <* Tok.braceR
braceBlockSpaces  = Tok.terminator *> Tok.spaces

codeBlock    p = indCodeBlock p -- <|> braceCodeBlock p
indCodeBlock p = indCodeBlockStart *> indBlockBody p <* indBlockEnd

indCodeBlockStart = Tok.indBlockBegin *> indBlockStart
indBlockStart     = Tok.spaces *> Indent.indented *> Indent.beginBlock
indBlockEnd       = Indent.endBlock

blockStart  = indCodeBlockStart
blockEnd    = indBlockEnd
blockBody p = indBlockBody p
blockBody' p = indBlockBody' p
blockBodyOpt p = indBlockBodyOpt p


blockBeginFields p = Tok.indBlockBegin *> indBlockFields p
indBlockFields   p = Tok.spaces *> withBlockChecked (indBlockBodyFields p)
indBlockBodyFields   p = (:) <$> p <*> many (indBlockPrefixFields p)
indBlockPrefixFields p = ((try (Tok.spaces *> Indent.checkIndentedOrEq)) *> notFollowedBy eof) *> p



withBlockChecked p = Indent.indented *> Indent.withBlock p


--stage1Block  = (char ':' *> stage1Body2) <|> pure []

textBlock :: (DeltaParsing p, MonadIndent p) => p String
textBlock = indTextBlock line <|> pure mempty


line :: CharParsing p => p String
line = (many1 $ noneOf "\n\r")

indTextBlock      p = Tok.indBlockBegin' *> indentedText p
indentedText      p = (\s ls -> s ++ concat ls) <$> Tok.spaces <*> withBlockChecked (indentedTextBlock p)
indentedTextBlock p = (:) <$> p <*> many (indentedLine p)
indentedLine      p = (++) <$> try (indentedSpaces <* notFollowedBy eof) <*> p

indentSpaces   = Tok.spaces <* Indent.checkIndent
indentedSpaces = Tok.spaces <* Indent.checkIndentedOrEq




--stage1Body2 :: (DeltaParsing p, MonadIndent p) => p [String]
--stage1Body2 = ((:) <$> (try ((<>) <$> Tok.spaces <* Indent.checkIndented <*> line)) <*> stage1Body2) <|> pure [[]]

--blockSpaces = hspaces <*> many ((:) <$> Tok.terminator' <*> hspaces) <*> spaces <*> <check for indent>


--fooyyy = 15 where
--    a = [1,
--     2]
--   --b = 2
