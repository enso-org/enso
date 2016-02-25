{-# LANGUAGE NoMonomorphismRestriction #-}

module Lunac.Parser.Pattern where

--import Flowbox.Prelude

--import           Text.Parser.Combinators
--import           Luna.Parser.Combinators (many1, sepBy2)
--import qualified Luna.Syntax.Pat    as Pat
--import           Luna.Parser.Builder (labeled, withLabeled)
--import qualified Luna.Parser.Token       as Tok
--import qualified Luna.Parser.Type as Type
--import           Luna.Parser.Type (typic)
--import qualified Luna.Syntax.Name.Path        as NamePath
--import qualified Luna.Parser.State            as ParserState
--import qualified Luna.Data.Namespace.State    as Namespace
--import           Luna.Data.StructInfo         (OriginInfo(OriginInfo))
--import           Luna.Parser.Literal          (literal)


--pattern    = choice [ try implTuple
--                    , patCon
--                    ]

--patTup     = pattern <|> (labeled (Pat.Tuple <$> pure []))

--patCon     = choice [ try app
--                    , term
--                    ]

--argPattern = termBase Type.term

--term       = termBase typic

--termBase t = choice [ try (labeled (Pat.Grouped <$> Tok.parens patTup))
--                    , try (labeled (Pat.Typed   <$> entP <* Tok.typeDecl <*> t))
--                    , entP
--                    ]
--              <?> "pattern term"

--var        = withLabeled $ \id -> do
--                name <- Tok.varIdent
--                let np = NamePath.single name
--                path <- ParserState.getModPath
--                Namespace.regVarName (OriginInfo path id) np
--                return $ Pat.Var (fromText name)

--lit        = labeled (Pat.Lit         <$> literal)
--implTuple  = labeled (Pat.Tuple       <$> sepBy2 patCon Tok.separator)
--wild       = labeled (Pat.Wildcard    <$  Tok.wildcard)
--recWild    = labeled (Pat.RecWildcard <$  Tok.recWildcard)
--con        = labeled (Pat.Con         <$> Tok.conIdent)
--app        = labeled (Pat.App         <$> con <*> many1 term)

--entP = choice [ var
--              , lit
--              , wild
--              , recWild
--              , con
--              ]
