{-# LANGUAGE NoMonomorphismRestriction #-}

module Luna.Parser.Literal where

import Prelude.Luna
import qualified Luna.Parser.Token       as Tok
import           Text.Parser.Combinators
import qualified Luna.Syntax.AST.Lit     as Lit
import           Luna.Syntax.Model.Network.Builder.Term.Class

import Text.Trifecta.Combinators (DeltaParsing)
import Luna.Parser.Indent (MonadIndent)
--import Luna.Parser.Builder (labeled)


--literal = choice [ numL, charL, stringL ]
--charL   = labeled (Lit.Char   <$> Tok.charLiteral)
--stringL = labeled (Lit.String <$> Tok.stringLiteral)
--numL    = labeled (Lit.Number <$> Tok.numberL)

literal = choice [ string ]

string :: (DeltaParsing p, MonadIndent p, TermBuilder Lit.String m a) => p (m a)
string = str <$> Tok.stringLiteral

--str :: TermBuilder Lit.String m a => String -> m a

