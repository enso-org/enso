{-# LANGUAGE NoMonomorphismRestriction #-}

module Lunac.Parser.Struct where

--import Flowbox.Prelude

--import qualified Luna.Parser.Token       as Tok
--import qualified Luna.Parser.Indent as Indent
--import           Text.Parser.Combinators
--import           Luna.Parser.Combinators (many1)


--moduleBlock p = braceBlockBegin p <|> indBlockBody p
--blockBegin  p = indBlockBegin   p <|> braceBlockBegin p

--indBlockBegin  p = Tok.indBlockBegin *> indBlock p
--indBlock       p = Tok.spaces *> Indent.indented *> Indent.withPos (indBlockBody p)
--indBlockBody   p = (:) <$> p <*> many (indBlockPrefix p)
--indBlockBody'  p = (:) <$> p <*> many (try $ indBlockPrefix p)
--indBlockBodyOpt p = ($) <$> (((:) <$> p) <|> pure id)
--                        <*> many (indBlockPrefix p)
--indBlockPrefix p = try ((try (Tok.spaces *> Indent.checkIndent) <|> try (Tok.terminator *> Tok.spaces *> Indent.checkIndentedOrEq )) *> notFollowedBy eof) *> p
----indBlockBody   p = (:) <$> p <*> many (try (Tok.spaces *> Indent.checkIndent) *> p)
--indBlockSpacesIE = try (Tok.spaces <* Indent.checkIndentedOrEq) <|> pure mempty

--braceBlockBegin p = Tok.braceL *> Tok.spaces *> Indent.withDiscarded (many1 (p <* braceBlockSpaces)) <* Tok.braceR
--braceBlockSpaces  = Tok.terminator *> Tok.spaces

--codeBlock    p = indCodeBlock p -- <|> braceCodeBlock p
--indCodeBlock p = indCodeBlockStart *> indBlockBody p <* indBlockEnd

--indCodeBlockStart = Tok.indBlockBegin *> indBlockStart
--indBlockStart     = Tok.spaces *> Indent.indented *> Indent.startBlock
--indBlockEnd       = Indent.endBlock

--blockStart  = indCodeBlockStart
--blockEnd    = indBlockEnd
--blockBody p = indBlockBody p
--blockBody' p = indBlockBody' p
--blockBodyOpt p = indBlockBodyOpt p


--blockBeginFields p = Tok.indBlockBegin *> indBlockFields p
--indBlockFields   p = Tok.spaces *> Indent.indented *> Indent.withPos (indBlockBodyFields p)
--indBlockBodyFields   p = (:) <$> p <*> many (indBlockPrefixFields p)
--indBlockPrefixFields p = ((try (Tok.spaces *> Indent.checkIndentedOrEq)) *> notFollowedBy eof) *> p
