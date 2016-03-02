module Luna.Parser.Class where

import Prelude.Luna

import Luna.Data.Name
import Luna.Parser.Indent                     (MonadIndent)
import Luna.Parser.State                      (MonadParserState)
import Luna.Syntax.AST.Term                   hiding (Name)
import Luna.Syntax.Model.Network.Builder.Term (TermBuilder)
import Text.Parser.Token                      (TokenParsing)
import Text.Trifecta.Combinators              (DeltaParsing)

import qualified Luna.Syntax.AST.Lit as Lit


type Parser     p = (ParserCore p, MonadIndent p, MonadParserState p)
type ParserCore p = (Monad p, TokenParsing p, DeltaParsing p)

type TermParser     p m a = (Parser p, TermBuilderCtx m a)
type TermParserCore p m a = (ParserCore p, TermBuilderCtx m a)


type TermBuilderCtx m a = ( a ~ Input a, MonadFix m
                          , TermBuilder Unify      m a
                          , TermBuilder Var        m a
                          , TermBuilder Cons       m a
                          , TermBuilder App        m a
                          , TermBuilder Unify      m a
                          , TermBuilder Match      m a
                          , TermBuilder Lit.String m a
                          , IsString (NameInput a)
                          , Convertible Name (NameInput a)
                          )
