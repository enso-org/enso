{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoStrict                  #-}
{-# LANGUAGE NoStrictData              #-}
{-# LANGUAGE UndecidableInstances      #-}

module Luna.Syntax.Text.Parser.Pass.Definition where

import Prologue

import qualified Control.Monad.State.Layered            as State
import qualified Data.Graph.Component.Node.Construction as Term
import qualified Luna.IR                                as IR
import qualified Luna.Pass                              as Pass
import qualified Luna.Pass.Attr                         as Attr
import qualified Luna.Pass.Scheduler                    as Scheduler
import qualified Luna.Syntax.Text.Lexer                 as Lexer
import qualified Luna.Syntax.Text.Parser.IR.Class       as Token
import qualified Luna.Syntax.Text.Parser.IR.Term        as Parsing
import qualified Luna.Syntax.Text.Parser.State.Marker   as Marker
import qualified Text.Megaparsec                        as Parser

import Data.Text.Position                          (FileOffset)
import Data.Text32                                 (Text32)
import Luna.Pass                                   (Pass)
import Luna.Syntax.Text.Parser.Data.CodeSpan       (CodeSpan, CodeSpanRange)
import Luna.Syntax.Text.Parser.Data.Invalid        (Invalids)
import Luna.Syntax.Text.Parser.Data.Name.Hardcoded (hardcode)
import Luna.Syntax.Text.Parser.Data.Result         (Result (Result))
import Luna.Syntax.Text.Parser.IR.Class            (Error, ParserBase, Stream,
                                                    Token)
import Luna.Syntax.Text.Parser.Pass.Class          (IRB (fromIRB), IRBS, Parser,
                                                    ParserPass, fromIRBS)
import Luna.Syntax.Text.Parser.State.Indent        (Indent)
import Luna.Syntax.Text.Parser.State.LastOffset    (LastOffset)
import Luna.Syntax.Text.Parser.State.Reserved      (Reserved)
import Luna.Syntax.Text.Scope                      (Scope)
import Luna.Syntax.Text.Source                     (Source)
import Text.Megaparsec                             (ParseError, ParsecT)
import Text.Megaparsec.Error                       (parseErrorPretty)



-------------------------
-- === Parser pass === --
-------------------------

-- === Definition === --

instance ParserPass (Pass stage Parser) => Pass.Definition stage Parser where
    definition = do
        src             <- Attr.get @Source
        (unit, markers) <- runParser__ Parsing.unit (convert src)
        Attr.put $ Result unit


-- === API === --

-- registerStatic :: Registry.Monad m => m ()
-- registerStatic = do
--     Registry.registerPrimLayer @IR.Terms @CodeSpan

registerDynamic :: ∀ stage m.
    ( ParserPass (Pass stage Parser)
    , Scheduler.PassRegister stage Parser m
    , Scheduler.Monad m
    ) => m ()
registerDynamic = do
    Scheduler.registerAttr     @Invalids
    Scheduler.enableAttrByType @Invalids
    Scheduler.registerAttr     @Source
    Scheduler.enableAttrByType @Source
    Scheduler.registerAttr     @Result
    Scheduler.enableAttrByType @Result
    Scheduler.registerPass     @stage @Parser


-- === Internal === --

runParsec__ :: MonadIO m =>
    ParserBase a -> Stream -> m (Either (ParseError Token Error) a)
runParsec__ p s = liftIO $ Parser.runParserT p "" s ; {-# INLINE runParsec__ #-}

runParserContext__ :: MonadIO m =>
    Token.Parser a -> Stream -> m (Either (ParseError Token Error) a)
runParserContext__ p s
    = flip runParsec__ s
    $ State.evalDefT @CodeSpanRange
    $ State.evalDefT @Reserved
    $ State.evalDefT @Scope
    $ State.evalDefT @LastOffset
    $ State.evalDefT @Marker.State
    -- $ State.evalDefT @Position
    $ State.evalDefT @FileOffset
    $ State.evalDefT @Indent
    $ hardcode >> p
{-# INLINE runParserContext__ #-}

runParser__ :: ParserPass (Pass stage Parser)
    => Token.Parser (IRBS a) -> Text32 -> Pass stage Parser (a, Marker.TermMap)
runParser__ p src = do
    let tokens = Lexer.evalDefLexer src
        parser = Parsing.stx *> p <* Parsing.etx
    runParserContext__ parser tokens >>= \case
        Left e -> error ("Parser error: " <> parseErrorPretty e <> "\ntokens:\n" <> show tokens)
        Right irbs -> do
            ((ref, unmarked), gidMap) <- State.runDefT @Marker.TermMap
                                       $ State.runDefT @Marker.TermOrphanList
                                       $ fromIRB $ fromIRBS irbs
            pure (ref, gidMap)

