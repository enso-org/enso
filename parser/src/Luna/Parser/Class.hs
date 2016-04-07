{-# LANGUAGE GADTs            #-}

module Luna.Parser.Class where

import Prelude.Luna

import Luna.Syntax.Name
import Luna.Parser.Indent                     (MonadIndent)
import Luna.Parser.State                      (MonadParserState)
import Luna.Syntax.Term.Expr                   hiding (Name)
import Luna.Syntax.Model.Network.Builder.Term (TermBuilder_OLD)
import Text.Parser.Token                      (TokenParsing, token, someSpace)
import Text.Trifecta.Combinators              (DeltaParsing)
import Luna.Syntax.Model.Text.Location        (MonadLocation)
import Text.Parser.Char                       (CharParsing, char)
import Text.Parser.Combinators                (Parsing)

import qualified Luna.Syntax.Term.Lit as Lit
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
                         , TermBuilder_OLD Unify      m a
                         , TermBuilder_OLD Var        m a
                         , TermBuilder_OLD Cons       m a
                         , TermBuilder_OLD App        m a
                         , TermBuilder_OLD Unify      m a
                         , TermBuilder_OLD Acc        m a
                         , TermBuilder_OLD Match      m a
                         , TermBuilder_OLD Lit.String m a
                         , TermBuilder_OLD Lit.Number m a
                         , IsString              (NameInput a)
                         )
