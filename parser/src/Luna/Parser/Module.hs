{-# LANGUAGE NoMonomorphismRestriction #-}

module Luna.Parser.Module where

import Prelude.Luna hiding (cons, maybe, noneOf)

--import Luna.Parser.Builder (labeled)
--import           Luna.IR.Name.Path (QualPath(QualPath))
--import           Text.Parser.Combinators
--import Luna.Parser.Decl (imp, func, cls, typeAlias, typeWrapper, pragma, foreigns)
--import qualified Luna.Parser.State            as ParserState
--import           Luna.Syntax.Module (Module(Module))
--import           Luna.Syntax.Unit   (Unit(Unit))
--import           Luna.Parser.Struct (moduleBlock)
--import qualified Luna.Parser.Indent as Indent

--pModule qpath = do
--    ParserState.setModPath qpath
--    Module qpath <$> Indent.withPos (moduleBlock $ labeled moduleBody)
--    where moduleBody = decl <?> "module body"

--pUnit p = Unit <$> labeled p

--decl = choice [ imp, func, cls, typeAlias, typeWrapper, pragma, foreigns]



--unit p = do
--    --FIXME[WD] : change id to datatype
--    let id = -777
--    --id <- nextID
--    --Unit id <$> ParserState.withNewScope id p
--    ParserState.withNewScope id p
