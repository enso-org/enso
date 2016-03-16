{-# LANGUAGE GADTs            #-}

module Luna.Parser.Class where

import Prelude.Luna

import Luna.Data.Name
import Luna.Parser.Indent                     (MonadIndent)
import Luna.Parser.State                      (MonadParserState)
import Luna.Syntax.AST.Term                   hiding (Name)
import Luna.Syntax.Model.Network.Builder.Term (TermBuilder)
import Text.Parser.Token                      (TokenParsing, token, someSpace)
import Text.Trifecta.Combinators              (DeltaParsing)
import Luna.Syntax.Model.Text.Location        (MonadLocation)
import Text.Parser.Char                       (CharParsing, char)
import Text.Parser.Combinators                (Parsing)

import qualified Luna.Syntax.AST.Term.Lit as Lit
import qualified Text.Trifecta.Parser     as Trifecta



newtype Parser a = Parser { runParser :: Trifecta.Parser a } deriving ( Functor, Applicative, Alternative, Monad, MonadPlus
                                                                      , DeltaParsing, CharParsing, Parsing)

instance TokenParsing Parser where
    token p = p <* (someSpace <|> pure ())


type ParserCtx  p = (ParserCore p, MonadIndent p, MonadParserState p)
type ParserCore p = (Monad p, TokenParsing p, DeltaParsing p)

type ASTParser     p m a = (ParserCtx p, ASTBuilderCtx m a)
type ASTParserCore p m a = (ParserCore p, ASTBuilderCtx m a)


type ASTBuilderCtx m a = ( a ~ Input a
                         , MonadFix               m
                         , MonadLocation          m
                         , TermBuilder Unify      m a
                         , TermBuilder Var        m a
                         , TermBuilder Cons       m a
                         , TermBuilder App        m a
                         , TermBuilder Unify      m a
                         , TermBuilder Acc        m a
                         , TermBuilder Match      m a
                         , TermBuilder Lit.String m a
                         , IsString              (NameInput a)
                         )
