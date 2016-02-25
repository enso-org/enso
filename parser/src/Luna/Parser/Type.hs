{-# LANGUAGE NoMonomorphismRestriction #-}

module Luna.Parser.Type where

--import           Flowbox.Prelude         hiding (noneOf, maybe, element, cons)
--import qualified Luna.Syntax.Type        as Type
--import           Luna.Syntax.Type        (Type)
--import qualified Luna.Parser.Token       as Tok
--import           Luna.Parser.Builder     (labeled, qualifiedPath)
--import qualified Luna.Parser.Builder     as B
--import           Text.Parser.Combinators
--import           Text.Parser.Token       (braces)
--import           Luna.Parser.Combinators (many1, sepBy2)

--typic   = choice [ try func
--                 , single
--                 ] <?> "type"

--single  = choice [ try app
--                 , term
--                 ] <?> "type"

--term    = choice [ try $ Tok.parens typic
--                 , entT
--                 ] <?> "type term"

--app     = labeled (Type.App <$> appBase <*> many1 term)

--argList = braces  (sepBy2 typic Tok.separator) <|> ((:[]) <$> single) <?> "type argument list"

--func    = labeled (Type.Function <$> argList <* Tok.arrow <*> typic)
--var     = labeled (Type.Var      <$> Tok.typeVarIdent)
--con     = labeled (Type.Con      <$> qualifiedPath Tok.typeIdent)
--tuple   = labeled (Type.Tuple    <$> B.tuple typic)
--list    = labeled (Type.List     <$> Tok.brackets typic)
--wild    = labeled (Type.Wildcard <$  Tok.wildcard)

--appBase = choice [ var, con ]

--entT    = choice [ labeled meta
--                 , var
--                 , con
--                 , tuple
--                 , list
--                 , wild
--                 ]

--meta = Type.Meta <$ Tok.tkwMeta <*> labeled metaBase

--metaBase = choice [ Type.MetaVar  <$> Tok.typeVarIdent
--                  , Type.MetaCons <$> Tok.typeIdent
--                  , Type.MetaRoot <$  Tok.metaRoot
--                  ]
