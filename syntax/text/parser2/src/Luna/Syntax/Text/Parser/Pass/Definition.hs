{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE UndecidableInstances      #-}

module Luna.Syntax.Text.Parser.Pass.Definition where

import Prologue

import qualified Control.Monad.State.Layered            as State
import qualified Data.Attoparsec.Internal.Types         as Parser
import qualified Data.Graph.Component.Node.Construction as Term
import qualified Data.Text32                            as Text32
import qualified Luna.IR                                as IR
import qualified Luna.Pass                              as Pass
import qualified Luna.Pass.Attr                         as Attr
import qualified Luna.Pass.Scheduler                    as Scheduler
import qualified Luna.Syntax.Text.Lexer                 as Lexer
import qualified Luna.Syntax.Text.Lexer.Symbol          as Symbol
import qualified Luna.Syntax.Text.Parser.IR.Ast         as Ast
import qualified Luna.Syntax.Text.Parser.IR.Ast         as Parsing
import qualified Luna.Syntax.Text.Parser.IR.Class       as Token
import qualified Luna.Syntax.Text.Parser.IR.Term        as Parsing
import qualified Luna.Syntax.Text.Parser.State.Marker   as Marker
import qualified Text.Megaparsec                        as Parser
import qualified Text.Parser.State.Indent               as State.Indent

import Data.Attoparsec.Internal.Types              (IResult)
import Data.Parser                                 (PartialParser, closePartial,
                                                    parsePartial, token)
import Data.Text.Position                          (FileOffset, Position)
import Data.Text32                                 (Text32)
import Luna.IR                                     (SomeTerm)
import Luna.Pass                                   (Pass)
import Luna.Syntax.Text.Parser.Data.CodeSpan       (CodeSpan, CodeSpanRange)
import Luna.Syntax.Text.Parser.Data.Invalid        (Invalids)
import Luna.Syntax.Text.Parser.Data.Name.Hardcoded (hardcode)
import Luna.Syntax.Text.Parser.Data.Result         (Result (Result))
import Luna.Syntax.Text.Parser.IR.Class            (Error, ParserBase, Stream,
                                                    Token)
import Luna.Syntax.Text.Parser.Pass.Class          (IRB (fromIRB), IRBS, Parser,
                                                    ParserPass, fromIRBS)
import Luna.Syntax.Text.Parser.State.LastOffset    (LastOffset)
import Luna.Syntax.Text.Parser.State.Reserved      (Reserved)
import Luna.Syntax.Text.Scope                      (Scope)
import Luna.Syntax.Text.Source                     (Source)
import Text.Megaparsec                             (ParseError, ParsecT)
import Text.Megaparsec.Error                       (parseErrorPretty)
import Text.Parser.State.Indent                    (Indent)



-- -------------------------
-- -- === Parser pass === --
-- -------------------------

-- -- === Definition === --

instance ParserPass (Pass stage Parser) => Pass.Definition stage Parser where
    definition = do
        src             <- Attr.get @Source
        print "!!!!!"
        print src
        undefined
        -- (unit, markers) <- runParser__ Parsing.unit (convert src)
        -- Attr.put $ Result unit


-- -- === API === --

-- -- registerStatic :: Registry.Monad m => m ()
-- -- registerStatic = do
-- --     Registry.registerPrimLayer @IR.Terms @CodeSpan

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


-- -- === Internal === --

-- runParsec__ :: MonadIO m =>
--     ParserBase a -> Stream -> m (Either (ParseError Token Error) a)
-- runParsec__ p s = liftIO $ Parser.runParserT p "" s ; {-# INLINE runParsec__ #-}

-- runParserContext__ :: MonadIO m =>
--     Token.Parser a -> Stream -> m (Either (ParseError Token Error) a)
-- runParserContext__ p s
--     = flip runParsec__ s
--     $ State.evalDefT @CodeSpanRange
--     $ State.evalDefT @Reserved
--     $ State.evalDefT @Scope
--     $ State.evalDefT @LastOffset
--     $ State.evalDefT @Marker.State
--     -- $ State.evalDefT @Position
--     $ State.evalDefT @FileOffset
--     $ State.Indent.eval
--     $ hardcode >> p
-- {-# INLINE runParserContext__ #-}

-- runParser__ :: ParserPass (Pass stage Parser)
--     => Parsing.Parser (IRBS a) -> Text32 -> Pass stage Parser (a, Marker.TermMap)
-- runParser__ p src = do
--     let irbs = runParserxx__ p src
--     ((ref, unmarked), gidMap) <- State.runDefT @Marker.TermMap
--                                $ State.runDefT @Marker.TermOrphanList
--                                $ fromIRB $ fromIRBS irbs
--     pure (ref, gidMap)

runParser__ :: ParserPass (Pass stage Parser)
    => Parsing.SyntaxVersion -> Parsing.Parser Ast.Spanned -> Text32 -> Pass stage Parser (SomeTerm, Marker.TermMap)
runParser__ sv p src = do
    let ast = runParserxx__ sv p src
    ((ref, unmarked), gidMap) <- State.runDefT @Marker.TermMap
                               $ State.runDefT @Marker.TermOrphanList
                               $ Ast.buildIR ast
    pure (ref, gidMap)

    -- let tokens = Lexer.evalDefLexer src
        -- parser = Parsing.stx *> p <* Parsing.etx
    -- runParserContext__ parser tokens >>= \case
    --     Left e -> error ("Parser error: " <> parseErrorPretty e <> "\ntokens:\n"
    --            <> show (view Symbol.symbol <$> tokens))
    --     Right irbs -> do
    --         ((ref, unmarked), gidMap) <- State.runDefT @Marker.TermMap
    --                                    $ State.runDefT @Marker.TermOrphanList
    --                                    $ fromIRB $ fromIRBS irbs
    --         pure (ref, gidMap)



runParserxx__ :: Parsing.SyntaxVersion -> Parsing.Parser a -> Text32 -> a
runParserxx__ = \sv p s ->
    let s'  = Text32.dropWhile (== ' ') s
        off = Text32.length s - Text32.length s'
        go  = evalLexer__ sv p
    in  {- stx off : -} go s'
{-# INLINE runParserxx__ #-}

evalLexer__ :: Parsing.SyntaxVersion -> Parsing.Parser a -> Text32 -> a
evalLexer__ = \sv p txt -> case runner__ sv p txt of
    Parser.Done !(txt') !r -> if (Text32.length txt' /= 0)
        then error $ "Panic. Not all input consumed by lexer: " <> show txt'
        else r
    Parser.Partial g -> case g mempty of
        Parser.Done !(txt') !r -> if (Text32.length txt' /= 0)
            then error $ "Panic. Not all input consumed by lexer: " <> show txt'
            else r
        Parser.Fail !_ !_ !e   -> error e
        Parser.Partial {} -> impossible

    Parser.Fail !_ !_ !e   -> error e
{-# INLINE evalLexer__ #-}

runner__ :: Parsing.SyntaxVersion -> Parsing.Parser a -> Text32 -> IResult Text32 a
runner__ = \sv p txt
    -> flip parsePartial (txt <> "\ETX")
     $ State.evalDefT @Scope
     $ State.evalDefT @FileOffset
     $ State.evalDefT @Marker.State
     $ State.evalDefT @CodeSpanRange
     $ State.evalDefT @LastOffset
     $ State.evalDefT @Position
     $ State.Indent.eval
     $ flip (State.evalT @Parsing.SyntaxVersion) sv
     $ (p <* token '\ETX')
{-# INLINE runner__ #-}






-- type Parser = StatesT '[Result, Indent, Location] Parsec.Parser
-- type Parser = StatesT '[Indent, Location, LastOffset, CodeSpanRange, Marker.State, FileOffset] Parsec.Parser
